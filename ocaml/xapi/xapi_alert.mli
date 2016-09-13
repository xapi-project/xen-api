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

(** Creates an alert/message and guarantees not to block. If called on the master it will use the
    internal Xapi_message.create function. If called on a slave it will enqueue the alert and
    leave it for processing by a background thread. *)
val add : msg:(string * int64) -> cls:API.cls -> obj_uuid:string -> body:string -> unit

(** Calls the given function whenever values change *)
val edge_trigger : ('a -> 'a -> unit) -> 'a -> unit
