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

module type DAEMON = sig
  val check : daemon_check
  (** A way to check whether the daemon is running. *)

  val start : unit -> unit
  (** Function which will start the daemon. *)

  val stop : unit -> unit
  (** Function which will stop the daemon. *)
end

module Make : functor (D : DAEMON) -> sig
  val with_daemon_stopped : ?timeout:float -> (unit -> 'a) -> 'a
  (** If the daemon is running, stop it while [f] runs and restart it once [f]
      	    has returned. If multiple threads call [with_daemon_stopped] in parallel,
      	    the daemon will not be restarted until all threads have left [f].

      	    If [timeout] is set, [with_daemon_stopped] will catch any exceptions from
      	    [stop ()] and keep checking whether the daemon is running, until [timeout]
      	    expires. If the daemon is still running after [timeout], the original
      	    exception will be thrown. *)
end
