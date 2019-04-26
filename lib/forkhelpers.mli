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

(** Functions to execute processes, pass file descriptors around and return results *)

(** The low-level Unix.fork(), Unix.exec*() functions and friends are not safe to 
    call in multithreaded programs for two reasons:
    + parallel threads opening new file descriptors will have these descriptors captured
      by a fork(). This leads to annoying glitches like (for example) attempts to 'umount'
      a filesystem being rejected because a file is still open.
    + although Unix.fork() will call (via the ocaml runtime) a pthread_atfork handler
      which attempts to clean up the state of the threading system in the child, this relies
      on quite a complex glibc implementation.

    Additionally Unix.fork(), Unix.exec*() are very low-level primitives. When we call
    these functions what we actually want to do is run some separate process with certain
    file-descriptors, optionally returning results. 

    	The interface in this module
    + is higher-level than Unix.fork(), Unix.exec*()
    + allows us to offload Unix.fork(), Unix.exec*() to a single-threaded separate process
      where the glibc+ocaml runtime codepaths are simpler and hopefully more reliable. *)

(** {2 High-level interface } *)

type syslog_stdout_t =
  | NoSyslogging
  | Syslog_DefaultKey
  | Syslog_WithKey of string

val default_path: string list

(** [execute_command_get_output cmd args] runs [cmd args] and returns (stdout, stderr)
    	on success (exit 0). On failure this raises 
    [Spawn_internal_error(stderr, stdout, Unix.process_status)] *)
val execute_command_get_output : ?env:string array -> ?syslog_stdout:syslog_stdout_t -> ?timeout:float -> string -> string list -> string * string

(** [execute_command_get_output cmd args stdin] runs [cmd args], passes in the string [stdin] and returns (stdout, stderr)
    	on success (exit 0). On failure this raises 
    [Spawn_internal_error(stderr, stdout, Unix.process_status)] *)
val execute_command_get_output_send_stdin : ?env:string array -> ?syslog_stdout:syslog_stdout_t -> ?timeout:float -> string -> string list -> string -> string * string

(** Thrown by [execute_command_get_output] if the subprocess exits with a non-zero exit code *)
exception Spawn_internal_error of string * string * Unix.process_status

(** {2 Low-level interface } *)

(** Represents a forked process *)
type pidty

(** [string_of_pidty p] returns a printable string description of [p] *)
val string_of_pidty : pidty -> string

(** [getpid p] returns the integer process id *)
val getpid : pidty -> int

(** Thrown by [safe_close_and_exec] if the process exits with a non-zero exit code. *)
exception Subprocess_failed of int

(** Thrown by [safe_close_and_exec] if the process exits due to a signal *)
exception Subprocess_killed of int

(** Thrown by [execute_command_get_output] if the process fails to finish within the timeout *)
exception Subprocess_timeout

(** [safe_close_and_exec stdin stdout stderr id_to_fd_list cmd args] runs [cmd args]
    	with the optional [stdin], [stdout] and [stderr] file descriptors (or /dev/null if not
    	specified) and with any key from [id_to_fd_list] in [args] replaced by the integer
    	value of the file descriptor in the final process. *)
val safe_close_and_exec : ?env:string array -> Unix.file_descr option -> Unix.file_descr option -> Unix.file_descr option -> (string * Unix.file_descr) list -> ?syslog_stdout:syslog_stdout_t -> ?redirect_stderr_to_stdout: bool -> string -> string list -> pidty

(** [waitpid p] returns the (pid, Unix.process_status) *)
val waitpid : pidty -> (int * Unix.process_status)

(** [waitpid_nohang p] returns the (pid, Unix.process_status) if the process has already
    	quit or (0, Unix.WEXITTED 0) if the process is still running. *)
val waitpid_nohang : pidty -> (int * Unix.process_status)

(** [dontwaitpid p]: signals the caller's desire to never call waitpid. Note that the final
    	process will not persist as a zombie. *)
val dontwaitpid : pidty -> unit

(** [waitpid_fail_if_bad_exit p] calls waitpid on [p] and throws [Subprocess_failed x] if the 
    	process exits with non-zero code x and [Subprocess_killed x] if the process is killed by a 
    	signal and exits with non-zero code x. *)
val waitpid_fail_if_bad_exit : pidty -> unit

(** Result returned by {!with_logfile_fd}. *)
type 'a result =
  | Success of string * 'a	(** The function call completed successfully. *)
  | Failure of string * exn	(** The function raised an exception. *)

(** Creates a temporary file and opens it for logging. The fd is passed to the function
    [f]. The logfile is guaranteed to be closed afterwards, and unlinked if either the delete flag is set or the call fails. If the
    function [f] throws an error then the log file contents are read in *)
val with_logfile_fd : ?delete:bool -> string -> (Unix.file_descr -> 'a) -> 'a result

(** Temporary directory used for communication *)
val temp_dir: string


