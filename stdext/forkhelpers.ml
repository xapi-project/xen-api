(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(* Functions to safely fork potentially long-running sub-processes without
   leaking file descriptors or accidentally deadlocking the parent process. *)

(* Functions should:
   1. Arrange to close all fds except the ones they actually want to keep open
   2. Not access any ocaml library or runtime function which might touch a lock
   (since that would cause deadlock) *)

(* XXX: this is a work in progress *)

open Pervasiveext

(** Standalone wrapper process which safely closes fds before exec()ing another
    program *)

exception Close_and_exec_binary_missing of string

(* This needs to point to where the binaray closeandexec is installed *)
let close_and_exec = "/opt/xensource/libexec/closeandexec"

let close_and_exec_cmdline (fds: Unix.file_descr list) (cmd: string) (args: string list) = 
  (* Sanity check: make sure the close_and_exec binary exists *)
  (try Unix.access close_and_exec [ Unix.X_OK ] 
   with _ -> raise (Close_and_exec_binary_missing close_and_exec));

  let fds = List.map (fun x -> string_of_int (Unixext.int_of_file_descr x)) fds in
  close_and_exec :: fds @ ("--" :: cmd :: args)

(* Low-level (unsafe) function which forks, runs a 'pre_exec' function and
   then executes some other binary. It makes sure to catch any exception thrown by
   exec* so that we don't end up with two ocaml processes. *)
let fork_and_exec ?(pre_exec=fun () -> ()) ?env (cmdline: string list) = 
  let args = Array.of_list cmdline in
  let argv0 = List.hd cmdline in
  let pid = Unix.fork () in
  if pid = 0 then begin
      try
	pre_exec ();
	(* CA-18955: xapi now runs with priority -3. We then set his sons priority to 0. *) 
	ignore_int (Unix.nice (-(Unix.nice 0)));
	ignore_int (Unix.setsid ());
	match env with
	| None -> Unix.execv argv0 args
	| Some env -> Unix.execve argv0 args env
      with _ -> exit 1
  end else pid

(** File descriptor operations to be performed after a fork.
    These are all safe in the presence of threads *)
type fd_operation = 
    | Dup2 of Unix.file_descr * Unix.file_descr
    | Close of Unix.file_descr

let do_fd_operation = function
  | Dup2(a, b) -> Unix.dup2 a b
  | Close a -> Unix.close a

(** Safe function which forks a command, closing all fds except a whitelist and
    having performed some fd operations in the child *)
let safe_close_and_exec ?env (pre_exec: fd_operation list) (fds: Unix.file_descr list) 
    (cmd: string) (args: string list) = 
  let cmdline = close_and_exec_cmdline fds cmd args in
  fork_and_exec ~pre_exec:(fun () -> List.iter do_fd_operation pre_exec) ?env cmdline

exception Subprocess_failed of int
exception Subprocess_killed of int

let waitpid pid = match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _, Unix.WEXITED n -> raise (Subprocess_failed n)
  | _, Unix.WSIGNALED n -> raise (Subprocess_killed n)
  | _, Unix.WSTOPPED n -> raise (Subprocess_killed n)

type 'a result = Success of string * 'a | Failure of string * exn

(** Creates a temporary file and opens it for logging. The fd is passed to the function
    'f'. The logfile is guaranteed to be closed afterwards, and unlinked if either the delete flag is set or the call fails. If the
    function 'f' throws an error then the log file contents are read in *)
let with_logfile_fd ?(delete = true) prefix f = 
  let logfile = Filename.temp_file prefix ".log" in
  let read_logfile () = 
    let log_fd = Unix.openfile logfile [ Unix.O_RDONLY ] 0o0 in
    finally
      (fun () -> Unixext.read_whole_file 1024 1024 log_fd)
      (fun () -> Unix.close log_fd; Unix.unlink logfile) in

  let log_fd = Unix.openfile logfile [ Unix.O_WRONLY; Unix.O_CREAT ] 0o0 in
  try
    let result = f log_fd in
    Unix.close log_fd;
    Success((if delete then read_logfile() else logfile), result)
  with e ->
    Unix.close log_fd;
    Failure(read_logfile(), e)


let with_dev_null f = Unixext.with_file "/dev/null" [ Unix.O_WRONLY ] 0o0 f
let with_dev_null_read f = Unixext.with_file "/dev/null" [ Unix.O_RDONLY ] 0o0 f


exception Spawn_internal_error of string * string * Unix.process_status

(* Execute a command, return the stdout logging or throw a Spawn_internal_error exception *)
let execute_command_get_output ?(cb_set=(fun _ -> ())) ?(cb_clear=(fun () -> ())) cmd args =
  let (stdout_exit, stdout_entrance) = Unix.pipe () in
  let fds_to_close = ref [ stdout_exit; stdout_entrance ] in
  let close' fd = 
    if List.mem fd !fds_to_close 
    then (Unix.close fd; fds_to_close := List.filter (fun x -> x <> fd) !fds_to_close) in
  
  let pid = ref 0 in
  finally  (* make sure I close all my open fds in the end *)
    (fun () ->
       (* Open /dev/null for reading. This will be given to the closeandexec process as its STDIN. *)
       with_dev_null_read (fun devnull_read ->
         (* Capture stderr output for logging *)
         match with_logfile_fd "execute_command_get_output"
         (fun log_fd ->
	    pid := safe_close_and_exec
	      [ Dup2(devnull_read, Unix.stdin);
	        Dup2(stdout_entrance, Unix.stdout);
	        Dup2(log_fd, Unix.stderr);
	        Close(stdout_exit) ]
	      [ Unix.stdin; Unix.stdout; Unix.stderr ] (* close all but these *)
	      cmd args;
	    (* parent *)
	    (try cb_set !pid with _ -> ());
	    close' stdout_entrance;
	    let output = (try Unixext.read_whole_file 500 500 stdout_exit with _ -> "") in
	    output, snd(Unix.waitpid [] !pid)) with
         | Success(log, (output, status)) ->
	     begin match status with
	     | Unix.WEXITED 0 -> output, log
	     | _ -> raise (Spawn_internal_error(log, output, status))
	     end
         | Failure(log, exn) ->
	     raise exn
       )
    ) (fun () -> 
	 (try cb_clear () with _ -> ());
	 List.iter Unix.close !fds_to_close)
