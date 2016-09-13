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

type t =
  | Master
  | Slave of string (** IP address *)
  | Broken

(** Returns a printable version ot [t] *)
val string_of: t -> string

(** Returns the role of this node *)
val get_role: unit -> t
(** Reset the role on disk, takes effect on next server restart only! *)
val set_role: t -> unit

(** Returns true if this node is a master *)
val is_master: unit -> bool
(** Returns true if this node is a slave *)
val is_slave: unit -> bool
(** Returns true if this node is broken *)
val is_broken: unit -> bool
(** Returns true if this is a unit test *)
val is_unit_test: unit -> bool

val set_pool_role_for_test: unit -> unit

exception This_host_is_a_master
exception This_host_is_broken

(** If this node is a slave, returns the IP address of its master *)
val get_master_address: unit -> string
