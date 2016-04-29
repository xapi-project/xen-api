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

type daemon_check =
	| Pidfile of string
		(** Check whether the daemon is running by reading a pidfile, and checking
		    that the PID points to a running process. *)
	| Function of (unit -> bool)
		(** Generic user-defined check, *)

type daemon_state = [
	`unmanaged |
	(** No threads which care about the state of the daemon are running. *)
	`should_start |
	(** Daemon should be started when the last thread exits
	    with_daemon_stopped. *)
	`should_not_start
	(** Daemon should not be started when the last thread exits
	    with_daemon_stopped. *)
]
(** Tristate value for representing the state of a daemon we want to manage. *)

module type DAEMON = sig
	val check : daemon_check
	(** A way to check whether the daemon is running. *)

	val start : unit -> unit
	(** Function which will start the daemon. *)

	val stop : unit -> unit
	(** Function which will stop the daemon. *)
end

module Make : functor (D : DAEMON) -> sig
	val with_daemon_stopped : (unit -> 'a) -> 'a
	(** If the daemon is running, stop it while [f] runs and restart it once [f]
	    has returned. If multiple threads call [with_daemon_stopped] in parallel,
	    the daemon will not be restarted until all threads have left [f]. *)
end
