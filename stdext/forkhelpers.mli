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

(** Standalone wrapper process which safely closes fds before exec()ing another program.
	Needs the binary /opt/xensource/libexec/closeandexec installed by close-and-exec. *)

exception Subprocess_failed of int
exception Subprocess_killed of int
exception Spawn_internal_error of string * string * Unix.process_status

type pidty

val string_of_pidty : pidty -> string

val nopid : pidty

(** File descriptor operations to be performed after a fork.
    These are all safe in the presence of threads *)
type fd_operation =
    Dup2 of Unix.file_descr * Unix.file_descr
  | Close of Unix.file_descr

val do_fd_operation : fd_operation -> unit

(** Low-level (unsafe) function which forks, runs a 'pre_exec' function and
   then executes some other binary. It makes sure to catch any exception thrown by
   exec* so that we don't end up with two ocaml processes. *)
val fork_and_exec : ?pre_exec:(unit -> unit) -> ?env:string array -> string list -> pidty

(** Safe function which forks a command, closing all fds except a whitelist and
    having performed some fd operations in the child *)
val safe_close_and_exec : ?env:string array -> Unix.file_descr option -> Unix.file_descr option -> Unix.file_descr option -> (string * Unix.file_descr) list -> string -> string list -> pidty

type 'a result = Success of string * 'a | Failure of string * exn

(** Creates a temporary file and opens it for logging. The fd is passed to the function
    'f'. The logfile is guaranteed to be closed afterwards, and unlinked if either the delete flag is set or the call fails. If the
    function 'f' throws an error then the log file contents are read in *)
val with_logfile_fd : ?delete:bool -> string -> (Unix.file_descr -> 'a) -> 'a result

(*val with_dev_null : (Unix.file_descr -> 'a) -> 'a
val with_dev_null_read : (Unix.file_descr -> 'a) -> 'a*)

(** Execute a command, return the stdout logging or throw a Spawn_internal_error exception *)
val execute_command_get_output : ?cb_set:(int -> unit) -> ?cb_clear:(unit -> unit) -> string -> string list -> string * string

val waitpid : pidty -> (int * Unix.process_status)
val waitpid_nohang : pidty -> (int * Unix.process_status)
val dontwaitpid : pidty -> unit
val waitpid_fail_if_bad_exit : pidty -> unit
val getpid : pidty -> int

val with_dev_null : (Unix.file_descr -> 'a) -> 'a
