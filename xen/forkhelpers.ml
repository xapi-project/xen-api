(*
 * Copyright (C) Citrix Systems Inc.
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
open Xenops_utils

module D = Debug.Make(struct let name = "forkhelpers" end)
open D

type syslog_stdout_t =
  | NoSyslogging
  | Syslog_DefaultKey
  | Syslog_WithKey of string

let dontwaitpid (sock, pid) = failwith "dontwaitpid"

let waitpid (sock, pid) = failwith "waitpid"

let getpid (sock, pid) = pid

type 'a result = Success of string * 'a | Failure of string * exn

exception Spawn_internal_error of string * string * Unix.process_status

(** Safe function which forks a command, closing all fds except a whitelist and
    having performed some fd operations in the child *)
let safe_close_and_exec ?env stdin stdout stderr (fds: (string * Unix.file_descr) list) ?(syslog_stdout=NoSyslogging)
    (cmd: string) (args: string list) =
	failwith "safe_close_and_exec"

(** Creates a temporary file and opens it for logging. The fd is passed to the function
    'f'. The logfile is guaranteed to be closed afterwards, and unlinked if either the delete flag is set or the call fails. If the
    function 'f' throws an error then the log file contents are read in *)
let with_logfile_fd ?(delete = true) prefix f = 
  let logfile = Filename.temp_file prefix ".log" in
  let read_logfile () = 
      let ic = open_in logfile in
      let buf = Buffer.create 128 in
      let line = String.make 128 '\000' in
      finally
          (fun () ->
              try
                  while true do
                      let n = input ic line 0 (String.length line) in
                      Buffer.add_string buf (String.sub line 0 n)
                  done
              with End_of_file -> ()
          ) (fun () -> close_in ic);
      let contents = Buffer.contents buf in
	  Unix.unlink logfile;
	  contents in

  let log_fd = Unix.openfile logfile [ Unix.O_WRONLY; Unix.O_CREAT ] 0o0 in
  try
    let result = f log_fd in
    Unix.close log_fd;
    Success((if delete then read_logfile() else logfile), result)
  with e ->
    Unix.close log_fd;
    Failure(read_logfile(), e)
