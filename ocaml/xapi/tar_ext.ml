(*
 * Copyright (c) Cloud Software Group, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Helpers

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let dir_perm = 0o755

let dir_size = 4096L

let inode_size = 4096L

let ( ++ ) = Int64.add

let ( // ) = Filename.concat

type unpack_error =
  | Illegal_file_path of string
  | Unsupported_file_type of string
  | Unpacked_exceeds_max_size_limit of Int64.t
  | File_size_mismatch of {
        path: string
      ; expected_size: Int64.t
      ; actual_size: Int64.t
    }
  | File_incomplete
  | File_corrupted
  | Unpacking_failure

let unpack_error_to_string = function
  | Illegal_file_path path ->
      Printf.sprintf "Illegal file path: %s" path
  | Unsupported_file_type t ->
      Printf.sprintf "Unsupported file type: %s" t
  | Unpacked_exceeds_max_size_limit size ->
      Printf.sprintf
        "Stop unpacking, otherwise unpacked files exceed max size limit: %s"
        (Int64.to_string size)
  | File_size_mismatch {path; expected_size; actual_size} ->
      Printf.sprintf
        "Unpacked file size mismatch, path: %s, expected size: %s, actual \
         size: %s"
        path
        (Int64.to_string expected_size)
        (Int64.to_string actual_size)
  | File_incomplete ->
      "File incomplete"
  | File_corrupted ->
      "File corrupted"
  | Unpacking_failure ->
      "Unpacking failure"

type unpack_result = Next of Int64.t

(* Unpack one of entries in the tar file to a specified directory. The file's
   information is defined in the file header. It includes the following parameters:
   dir: which directory to unpack the tar file.
   max_size_limit: the maximum size limitation of all the files in the tar.
   acc_size: the current accumulated unpacked files size.
   ifd: the tar file's descriptor.
   head: the header of the file to be unpacked.
*)
let unpack dir max_size_limit acc_size ifd head =
  (* Check if the entry's filename is legal. Including:
     Check if starting with '/'
     Check if including '.'
     Check if including '..'
  *)
  let assert_file_path_is_legal file_path =
    let file_list = String.split_on_char '/' file_path in
    if
      String.starts_with ~prefix:"/" file_path
      || List.exists (String.equal ".") file_list
      || List.exists (String.equal "..") file_list
    then
      Error (Illegal_file_path file_path)
    else
      Ok ()
  in
  let ( let* ) = Result.bind in
  (* Check if the accumulated files size plus the current file size exceeds the
     maximum size limitation. If so, return an error. *)
  let assert_file_not_exceed_max_size acc_size =
    if Int64.compare acc_size max_size_limit > 0 then
      Error (Unpacked_exceeds_max_size_limit max_size_limit)
    else
      Ok ()
  in
  let file_name = head.Tar.Header.file_name in
  debug "%s: Unpacking tar file: %s" __FUNCTION__ file_name ;
  let* () = assert_file_path_is_legal file_name in
  let path = dir // file_name in
  match head.Tar.Header.link_indicator with
  | Tar.Header.Link.Normal ->
      let expected_size = head.Tar.Header.file_size in
      let acc_size' = acc_size ++ expected_size ++ inode_size in
      let* () = assert_file_not_exceed_max_size acc_size' in
      Unixext.with_file path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
        head.Tar.Header.file_mode (fun ofd ->
          let actual_size = Unixext.copy_file ~limit:expected_size ifd ofd in
          if actual_size <> expected_size then
            Error (File_size_mismatch {path; expected_size; actual_size})
          else
            Ok (Next acc_size')
      )
  | Tar.Header.Link.Directory ->
      let acc_size' = acc_size ++ dir_size ++ inode_size in
      let* () = assert_file_not_exceed_max_size acc_size' in
      Unixext.mkdir_rec path head.Tar.Header.file_mode ;
      Ok (Next acc_size')
  | Hard
  | Symbolic
  | Character
  | Block
  | FIFO
  | GlobalExtendedHeader
  | PerFileExtendedHeader
  | LongLink ->
      Error
        (Unsupported_file_type
           (Tar.Header.Link.to_string head.Tar.Header.link_indicator)
        )

(* It will call function 'unpack' for every entry in the tar file. Each entry
   contains a header and the file self. The header include the destination file
   name, file type, file size, etc. The header will be passed to the function
   'unpack' as a parameter, then the function will verify the file size, file
   path, etc, and then unpack to the directory which is the other parameter of
   this function.
*)
let unpack_tar_file ~dir ~ifd ~max_size_limit =
  debug "%s: Unpacking to %s" __FUNCTION__ dir ;
  Unixext.rm_rec dir ;
  Unixext.mkdir_rec dir dir_perm ;
  let rec unpack_all_files f acc_size =
    match Tar_unix.Archive.with_next_file ifd (f acc_size) with
    | exception Tar.Header.End_of_stream ->
        debug "%s: Unpacked to %s successfully" __FUNCTION__ dir ;
        Ok ()
    | Ok (Next size) ->
        unpack_all_files f size
    | Error _ as err ->
        err
    | exception End_of_file ->
        Error File_incomplete
    | exception Tar.Header.Checksum_mismatch ->
        Error File_corrupted
    | exception e ->
        error "%s: Unpacking failure: %s" __FUNCTION__ (Printexc.to_string e) ;
        Error Unpacking_failure
  in
  let unpack' = unpack dir max_size_limit in
  unpack_all_files unpack' 0L
  |> Result.map_error (fun err ->
         error "%s: Failed to unpack to %s due to error: %s" __FUNCTION__ dir
           (unpack_error_to_string err) ;
         Unixext.rm_rec dir ;
         err
     )
