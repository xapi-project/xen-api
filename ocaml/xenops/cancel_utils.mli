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

open Xenstore
open Device_common
open Xenops_task

(** The object we are operating one. We assume we can only have one
	outstanding cancellable_watch per key. *)
type key =
	| Device of device
	| Domain of int
	| Qemu of int * int
	| TestPath of string

(** [on_shutdown xs domid] called whenever a domain has permanently shutdown *)
val on_shutdown: xs:Xs.xsh -> int -> unit

(** [cancellable_watch key success_watches error_watches task xs timeout]
    watches for cancellation, "success_watches" or "error_watches" up to a timeout.
    Returns true if a "success_watch" fires, false if an "error_watch" fires and throws
    an exception on cancellation. *)
val cancellable_watch: key -> unit Watch.t list -> unit Watch.t list -> Xenops_task.t -> xs:Xs.xsh -> timeout:float -> unit -> bool

(** [cancellable_subprocess task env stdin syslog_stdout cmd args]
    executes [cmd] with arguments [args], killing it if the task is cancelled. *)
val cancellable_subprocess: Xenops_task.t -> ?env:string array -> ?stdin:string -> ?syslog_stdout:Forkhelpers.syslog_stdout_t -> string -> string list -> (string * string)


