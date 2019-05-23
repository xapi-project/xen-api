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

let now () = Int64.of_float (Unix.gettimeofday ())

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

let exec_cmd (module D : Debug.DEBUG) ~cmdstring ~(f : string -> 'a option) =
  D.debug "Forking command %s" cmdstring;
  (* create pipe for reading from the command's output *)
  let (out_readme, out_writeme) = Unix.pipe () in
  let cmd, args = match Astring.String.cuts ~empty:false ~sep:" " cmdstring with [] -> assert false | h::t -> h,t in
  let pid = Forkhelpers.safe_close_and_exec None (Some out_writeme) None [] cmd args in
  Unix.close out_writeme;
  let in_channel = Unix.in_channel_of_descr out_readme in
  let vals = ref [] in
  let rec loop () =
    let line = input_line in_channel in
    let ret = f line in
    begin
      match ret with
      | None -> ()
      | Some v -> vals := v :: !vals
    end;
    loop ()
  in
  (try loop () with End_of_file -> ());
  Unix.close out_readme;
  let (pid, status) = Forkhelpers.waitpid pid in
  begin
    match status with
    | Unix.WEXITED n   -> D.debug "Process %d exited normally with code %d" pid n
    | Unix.WSIGNALED s -> D.debug "Process %d was killed by signal %d" pid s
    | Unix.WSTOPPED s  -> D.debug "Process %d was stopped by signal %d" pid s
  end;
  List.rev !vals
