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
(** Module that defines API functions for Bond objects
 * @group Networking
*)

(**
   A host can have multiple network interfaces. These can be used to physically separate networks. However, multiple interfaces can also be {i bonded} together to form a single high-throughput interface.
   {ul
   {- The PIFs that are bonded together are called {i slaves}.}
   {- The datamodel has a {i Bond} object that joins the slaves.}
   {- The bond is used through a {i master} PIF, which is also associated with the Bond object. The master PIF is a special PIF that does not directly represent a physical interface. The master PIF is associated with a Network, and used as a regular PIF.}
   {- Bond slaves should never be used directly: they are not allowed to be plugged. Hence, they are not connected to bridges and therefore not accessible.}
   }
*)

(** Create a PIF to represent the bond master and a Bond record to represent the bond.
 *  Return a reference to the bond record. The given network must not have any local
 *  PIFs on it yet.
*)
val create :
  __context:Context.t ->
  network:[ `network ] Ref.t ->
  members:[ `PIF ] Ref.t list ->
  mAC:string ->
  mode:API.bond_mode ->
  properties:(string * string) list ->
  [ `Bond ] Ref.t

(** Destroy the bond master (PIF) and the Bond objects, unless the bond master
 *  is the management interface, or used as VLAN master. *)
val destroy : __context:Context.t -> self:[ `Bond ] Ref.t -> unit

(** Change the bond mode. *)
val set_mode : __context:Context.t -> self:[ `Bond ] Ref.t -> value:API.bond_mode -> unit

(** Change a property of the bond. *)
val set_property : __context:Context.t -> self:[ `Bond ] Ref.t -> name:string -> value:string -> unit

(** If the given bond has VIFs or VLANs on its slaves, move these to the master. *)
val fix_bond :
  __context:Context.t ->
  bond:[ `Bond ] Ref.t ->
  unit

(** Functions for test cases *)
val __test_add_lacp_defaults : (string * string) list -> (string * string) list
