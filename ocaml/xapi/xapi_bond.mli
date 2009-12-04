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
 * @group XenAPI functions
 *)

(** Create a PIF to represent the bond master and a Bond record to represent the bond. 
 *  Return a reference to the bond record. The given network must not have any local
 *  PIFs on it yet.
 *)
val create :
  __context:Context.t ->
  network:[ `network ] Ref.t ->
  members:[ `PIF ] Ref.t list -> mAC:string -> [ `Bond ] Ref.t

(** Destroy the bond master (PIF) and the Bond objects, unless the bond master
 *  is the management interface, or used as VLAN master. *)
val destroy : __context:Context.t -> self:[ `Bond ] Ref.t -> unit

