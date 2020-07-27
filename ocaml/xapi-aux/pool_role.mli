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

type t = Master | Slave of string  (** IP address *) | Broken

val with_pool_role_lock : (unit -> unit) -> unit

val string_of : t -> string
(** Returns a printable version ot [t] *)

val get_role : unit -> t
(** Returns the role of this node *)

val is_master : unit -> bool
(** Returns true if this node is a master *)

val is_slave : unit -> bool
(** Returns true if this node is a slave *)

val is_broken : unit -> bool
(** Returns true if this node is broken *)

val is_unit_test : unit -> bool
(** Returns true if this is a unit test *)

val set_pool_role_for_test : unit -> unit

exception This_host_is_a_master

exception This_host_is_broken

val get_master_address : unit -> string
(** If this node is a slave, returns the IP address of its master *)
