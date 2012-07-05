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

(** Debug utilities *)

(** Throw away the cached hostname. The next log line will re-query the hostname *)
val invalidate_hostname_cache: unit -> unit

(** {2 Associate a task to the current actions} *)

(** Associate a task name to the current thread *)
val associate_thread_with_task : string -> unit

(** Dissociate a task name to the current thread *)
val dissociate_thread_from_task : unit -> unit

(** Do an action with a task name associated with the current thread *)
val with_thread_associated : string -> ('a -> 'b) -> 'a -> 'b

(** {2 Associate a name to the current thread} *)

val name_thread : string -> unit

val remove_thread_name : unit -> unit

val get_all_debug_keys : unit -> string list

module type BRAND = sig val name : string end

val gettimestring : unit -> string
(** The current time of day in a format suitable for logging *)

val set_facility : Syslog.facility -> unit
(** Set the syslog facility that will be used by this program. *)

val disable : string -> unit
(** [disable brand] Suppress all log output from the given [brand]. This function is idempotent. *)

val enable : string -> unit
(** [enable brand] Enable all log output from the given [brand]. This function is idempotent. *)

val log_to_stdout : unit -> unit
(** [log_to_stdout ()] will echo all log output to stdout (not the default) *)

module Debugger : functor (Brand : BRAND) ->
sig

	(** Debug function *)
	val debug : ('a, unit, string, unit) format4 -> 'a

    (** Warn function *)
	val warn : ('a, unit, string, unit) format4 -> 'a

	(** Info function *)
    val info : ('a, unit, string, unit) format4 -> 'a
    
	(** Error function *)
	val error : ('a, unit, string, unit) format4 -> 'a

    (** Audit function *)
	val audit : ?raw:bool -> ('a, unit, string, string) format4 -> 'a
    
	val log_backtrace : unit -> unit

	val log_and_ignore_exn : (unit -> unit) -> unit
end
	
