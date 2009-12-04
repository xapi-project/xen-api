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
(** Assertion helpers used when attaching a network
 * @group Networking
 *)
 
(** Raises an exception if the network has VIFs in use on the host *)
val assert_network_has_no_vifs_in_use_on_me :
  __context:Context.t ->
  host:[ `host ] Ref.t -> network:[ `network ] Ref.t -> unit  
  
(** Raises an exception when the [disallow_unplug] flag is set *)
val assert_pif_disallow_unplug_not_set :
  __context:Context.t -> [ `PIF ] Ref.t -> unit
  
(** Raises an exception if the network cannot be attached.
 *  Returns a list of {i shafted} PIFs and a list of {i local} PIFs.
 
 * Cannot attach this network if it has a PIF AND this PIF 'shafts'
 * some other PIF which is attached to a network which is 'in-use'.
 * Bringing a bond master up, or a VLAN on a bond, shafts the bond slaves;
 * similarly, bringing a bond slave up shafts its master + that master's VLANs;
 * but sibling slaves don't shaft each other.
 *
 * There should be only one local PIF by construction.
 *)
val assert_can_attach_network_on_host :
  __context:Context.t ->
  self:[ `network ] Ref.t ->
  host:[ `host ] Ref.t ->
  overide_management_if_check:bool ->
  [ `PIF ] Ref.t list * [ `PIF ] Ref.t list
