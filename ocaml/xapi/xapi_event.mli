(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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
open Event_types

val register: __context:Context.t -> classes:string list -> unit
(** Register an interest in events generated on objects of class <class_name> *)

val unregister: __context:Context.t -> classes:string list -> unit
(** Unregister interest in events generated on objects of class <class_name> *)

val next: __context:Context.t -> Rpc.t
(** Blocking call which returns the next set of events relevant to this session. *)

val with_wakeup : Context.t -> string -> ((unit -> unit) -> string list -> API.ref_task -> 'a) -> 'a
(** Allow us to make a function call that creates a task, so we can grab control back by killing the task *)

val with_safe_missing_handling : (unit -> 'a) -> 'a
(* Allow us to make a safer db call with a retry if we hit Db Not Found*)

val from: __context:Context.t -> classes:string list -> token:string -> timeout:float -> Rpc.t
(** Blocking call which returns the next set of events from a given set of
    classes/objects, or the empty list if the timeout is exceeded *)

val get_current_id: __context:Context.t -> int64

val inject: __context:Context.t -> _class:string -> _ref:string -> string

(** {2} Internal interfaces with the other parts of xapi. *)

val event_add: ?snapshot:Rpc.t -> string -> string -> string -> unit

val register_hooks: unit -> unit

(* Called whenever a session is being destroyed i.e. by Session.logout and db_gc *)
val on_session_deleted: API.ref_session -> unit

(* Inject an unnecessary update as a heartbeat. This will:
    1. hopefully prevent some firewalls from silently closing the connection
    2. allow the server to detect when a client has failed *)
val heartbeat: __context:Context.t -> unit

module Message : sig
  type t =
    | Create of (API.ref_message * API.message_t)
    | Del of API.ref_message

  val get_since_for_events : (__context:Context.t -> int64 -> (int64 * t list)) ref
end

