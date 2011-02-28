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
(*
 * Xen/Xenstored Abstraction Layer
 *
 * XAL is in charge of abstracting domain and devices events.
 *
 *
 * at init time:
 * - register for all xenstored watches for further update
 * - make a snapshot of domains running and dead,
 *   and put watch on devices on the live one.
 *
 * at processing time:
 * - make a diff of the current state, and the last known state
 * - update it's own state with the current state
 * - generate events, that going to fire either a specific event, or
 *   a generic callback registered at init time, from the difference
 *   it notice.
 *)

type domid = int

exception Domain_not_dead of domid
exception Device_not_monitored
exception Timeout

type dev_event =
	| DevEject of string
	| DevThread of string * int
	| DevShutdownDone of string * string
	| ChangeRtc of string * string
	| Message of string * string * int64 * string
	| HotplugChanged of string * string option * string option
	| ChangeUncooperative of bool
	| PciChanged of string

(* type dev_state = Connecting | Connected | Closing | Closed *)

val string_of_dev_event : dev_event -> string

type died_reason =
	| Crashed
	| Vanished
	| Halted
	| Rebooted
	| Suspended
	| Shutdown of int

val string_of_died_reason : died_reason -> string

val is_running : Xc.domaininfo -> bool
val dead_reason_of_xc : Xc.domaininfo -> died_reason

type ctx

val xc_of_ctx : ctx -> Xc.handle
val xs_of_ctx : ctx -> Xs.xsh

(* context query function *)
val domain_is_dead : ctx -> domid -> bool
val domain_is_paused : ctx -> domid -> bool
val domain_get_dead : ctx -> domid -> died_reason
val domain_get_domid : ctx -> string -> domid option
val domains_running : ctx -> domid list
val uuid_of_domid : ctx -> domid -> string

val device_is_connected : ctx -> domid -> string -> string -> bool
val device_get_hotplug : ctx -> domid -> string -> string -> string option

(* low level *)
val init : ?callback_introduce:(ctx -> domid -> unit)
        -> ?callback_release:(ctx -> domid -> unit)
        -> ?callback_devices:(ctx -> domid -> dev_event -> unit)
	-> ?callback_guest_agent:(ctx -> domid -> unit)
	-> ?callback_memory_target:(ctx -> domid -> unit)
        -> ?monitor_devices:bool
        -> unit -> ctx
val close : ctx -> unit

(** blocks the caller until either a timeout expire
    or the domain specified is released. *)
val wait_release : ctx -> ?timeout:float -> domid -> died_reason

(** blocks the caller processing xal for timeout time *)
val wait : ctx -> float -> unit

(* high level *)
val loop : ?callback_introduce:(ctx -> domid -> unit)
        -> ?callback_release:(ctx -> domid -> unit)
        -> ?callback_devices:(ctx -> domid -> dev_event -> unit)
	-> ?callback_guest_agent:(ctx -> domid -> unit)
	-> ?callback_memory_target:(ctx -> domid -> unit)
        -> unit -> unit
