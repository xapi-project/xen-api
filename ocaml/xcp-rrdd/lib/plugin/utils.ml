(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

let now () = Unix.gettimeofday ()

let cut str =
  Astring.String.fields ~empty:false ~is_sep:(fun c -> c = ' ' || c = '\t') str

let list_directory_unsafe name =
  let handle = Unix.opendir name in
  let rec read_directory_contents acc handle =
    try
      let next_entry = Unix.readdir handle in
      read_directory_contents (next_entry :: acc) handle
    with End_of_file -> List.rev acc
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> read_directory_contents [] handle)
    (fun () -> Unix.closedir handle)

let list_directory_entries_unsafe dir =
  let dirlist = list_directory_unsafe dir in
  List.filter (fun x -> x <> "." && x <> "..") dirlist

let exec_cmd (module D : Debug.DEBUG) ~cmdstring
    ~(read_out_line : string -> 'a option) ~(read_err_line : string -> 'b option)
    =
  D.debug "Forking command %s" cmdstring ;
  (* create pipes for reading from the command's output *)
  let out_readme, out_writeme = Unix.pipe () in
  let err_readme, err_writeme = Unix.pipe () in
  let cmd, args =
    match Astring.String.cuts ~empty:false ~sep:" " cmdstring with
    | [] ->
        assert false
    | h :: t ->
        (h, t)
  in
  let pid =
    Forkhelpers.safe_close_and_exec None (Some out_writeme) (Some err_writeme)
      [] cmd args
  in
  Unix.close out_writeme ;
  Unix.close err_writeme ;
  let read_and_close f fd =
    let in_channel = Unix.in_channel_of_descr fd in
    let vals = ref [] in
    let rec loop () =
      let line = input_line in_channel in
      let ret = f line in
      (match ret with None -> () | Some v -> vals := v :: !vals) ;
      loop ()
    in
    (try loop () with End_of_file -> ()) ;
    Unix.close fd ; List.rev !vals
  in
  let stdout = read_and_close read_out_line out_readme in
  let stderr = read_and_close read_err_line err_readme in
  let pid, status = Forkhelpers.waitpid pid in
  ( match status with
  | Unix.WEXITED n ->
      D.debug "Process %d exited normally with code %d" pid n
  | Unix.WSIGNALED s ->
      D.debug "Process %d was killed by signal %a" pid Debug.Pp.signal s
  | Unix.WSTOPPED s ->
      D.debug "Process %d was stopped by signal %a" pid Debug.Pp.signal s
  ) ;
  (stdout, stderr)
