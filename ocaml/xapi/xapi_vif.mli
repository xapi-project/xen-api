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
(** Module that defines API functions for VIF objects
 * @group Networking
*)

(**
   A {i VIF} object in the datamodel represents a virtual interface.
   {ul
   {- A VIF is used by a VM, and appears to a VM as a real network interface. A VIF has a MAC address.}
   {- The [VIF.currently_attached] field reflects whether a virtual interface is currently {i plugged} into its VM, meaning it is visible to the VM.
   	{ul
   	{- A VIF cannot be [currently_attached] when its VM is halted.}
   	{- When a VM starts up, its VIFs are automatically attached; when a VM shuts down, VIFs become detached.}
   	{- A VIF can be hot-plugged or hot-unplugged if its VM is running {i and} the VM has PV-drivers installed.}
   	}}
   {- A VIF can be attached to a Network (bridge) to connect it to a PIF (physical interface).}
   }
*)

(** {2 API functions} *)

(** Hotplug the VIF, dynamically attaching it to the running VM *)
val plug : __context:Context.t -> self:API.ref_VIF -> unit

(** Hot-unplug the VIF, dynamically unattaching it to the running VM *)
val unplug : __context:Context.t -> self:API.ref_VIF -> unit

(** Forcibly hot-unplug the VIF from the running VM *)
val unplug_force : __context:Context.t -> self:API.ref_VIF -> unit

(** Create a new VIF instance *)
val create :
  __context:Context.t ->
  device:string ->
  network:[ `network ] Ref.t ->
  vM:[ `VM ] Ref.t ->
  mAC:string ->
  mTU:int64 ->
  other_config:(string * string) list ->
  qos_algorithm_type:string ->
  qos_algorithm_params:(string * string) list ->
  locking_mode:API.vif_locking_mode ->
  ipv4_allowed:string list ->
  ipv6_allowed:string list -> API.ref_VIF

(** Destroy the specified VIF instance *)
val destroy : __context:Context.t -> self:[ `VIF ] Ref.t -> unit

(** {2 Helper Functions} *)

(** Move a VIF to another Network. *)
val move_internal :
  __context:Context.t ->
  network:[ `network ] Ref.t ->
  ?active:bool ->
  [ `VIF ] Ref.t ->
  unit

(** Move a VIF to another Network. *)
val move :
  __context:Context.t ->
  self:[ `VIF ] Ref.t ->
  network:[ `network ] Ref.t ->
  unit

(** Throw error if the given operation is not in the list of allowed operations.
 *  Implemented by {!Xapi_vif_helpers.assert_operation_valid} *)
val assert_operation_valid :
  __context:Context.t -> self:[ `VIF ] Ref.t -> op:API.vif_operations -> unit

(** Update the [PIF.allowed_operations] field.
 *  Implemented by {!Xapi_vif_helpers.update_allowed_operations} *)
val update_allowed_operations :
  __context:Context.t -> self:[ `VIF ] Ref.t -> unit

(** Set the locking mode of this VIF.
 *  Update the firewall rules associated with this VIF, if it is plugged. *)
val set_locking_mode :
  __context:Context.t -> self:[ `VIF ] Ref.t -> value:API.vif_locking_mode -> unit

(** Set the list of IPv4 addresses allowed to use this VIF. *)
val set_ipv4_allowed :
  __context:Context.t -> self:[ `VIF ] Ref.t -> value:string list -> unit

(** Associate an IPv4 address with this VIF. *)
val add_ipv4_allowed :
  __context:Context.t -> self:[ `VIF ] Ref.t -> value:string -> unit

(** Remove an IPv4 address from this VIF. *)
val remove_ipv4_allowed :
  __context:Context.t -> self:[ `VIF ] Ref.t -> value:string -> unit

(** Set the list of IPv6 addresses allowed to use this VIF. *)
val set_ipv6_allowed :
  __context:Context.t -> self:[ `VIF ] Ref.t -> value:string list -> unit

(** Associate an IPv6 address with this VIF. *)
val add_ipv6_allowed :
  __context:Context.t -> self:[ `VIF ] Ref.t -> value:string -> unit

(** Remove an IPv6 address from this VIF. *)
val remove_ipv6_allowed :
  __context:Context.t -> self:[ `VIF ] Ref.t -> value:string -> unit

(** Change the IP configuration of a VIF *)
val configure_ipv4 :
  __context:Context.t ->
  self:[ `VIF ] Ref.t ->
  mode:[`None | `Static] ->
  address:string -> gateway:string -> unit

(** Change the IP configuration of a VIF *)
val configure_ipv6 :
  __context:Context.t ->
  self:[ `VIF ] Ref.t ->
  mode:[`None | `Static] ->
  address:string -> gateway:string -> unit

