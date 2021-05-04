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

type t = Coordinator | Supporter of string  (** IP address *) | Broken

val with_pool_role_lock : (unit -> unit) -> unit

val string_of : t -> string
(** Returns a printable version ot [t] *)

val get_role : unit -> t
(** Returns the role of this node *)

val is_coordinator : unit -> bool
(** [is_coordinator ()] returns true if this node is the pool coordinator *)

val is_supporter : unit -> bool
(** [is_supporter ()] returns true if this node is a supporter *)

val is_broken : unit -> bool
(** Returns true if this node is broken *)

val is_unit_test : unit -> bool
(** Returns true if this is a unit test *)

val set_pool_role_for_test : unit -> unit

exception This_host_is_coordinator

exception This_host_is_broken

val get_address_of_coordinator_exn : unit -> string
(** [get_address_of_coordinator_exn ()] returns the IP of the pool coordinator
    if this node is a supporter, otherwise fails *)

val get_address_of_coordinator : unit -> string option
(** [get_address_of_coordinator ()] returns None if the current host the pool
    coordinator or the host is broken, and Some (address of the pool's
    coordinator) otherwise *)
