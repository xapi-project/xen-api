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
(** Module that defines API functions for VLANs *)

(** Create a VLAN with the given [tag] using the [tagged_PIF] as VLAN slave. 
 *  Creates a new PIF object as VLAN master (untagged PIF) and connects it to the
 *  given [network]. No other PIFs on the same host may be connected to this network. *)
val create :
  __context:Context.t ->
  tagged_PIF:[ `PIF ] Ref.t ->
  tag:int64 -> network:[ `network ] Ref.t -> [ `VLAN ] Ref.t
  
(** Destroy a VLAN. Removes the VLAN object as well as the VLAN master PIF. *)
val destroy : __context:Context.t -> self:[ `VLAN ] Ref.t -> unit
