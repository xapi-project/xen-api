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
 * @group XenAPI functions
 *)

(** {2 API functions} *)

(** Hotplug the VIF, dynamically attaching it to the running VM *)
val plug : __context:Context.t -> self:API.ref_VIF -> unit

(** Hot-unplug the VIF, dynamically unattaching it to the running VM *)
val unplug : __context:Context.t -> self:API.ref_VIF -> unit

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
  qos_algorithm_params:(string * string) list -> API.ref_VIF

(** Destroy the specified VIF instance *)
val destroy : __context:Context.t -> self:[ `VIF ] Ref.t -> unit

(** {2 Helper Functions} *)

val assert_operation_valid :
  __context:Context.t -> self:[ `VIF ] Ref.t -> op:API.vif_operations -> unit
val update_allowed_operations :
  __context:Context.t -> self:[ `VIF ] Ref.t -> unit
val dynamic_create :
  __context:Context.t -> vif:API.ref_VIF -> Locking_helpers.token -> unit
val destroy_vif :
  __context:Context.t -> xs:Xs.xsh -> 'a -> [ `VIF ] Ref.t -> 'b -> unit
val dynamic_destroy :
  __context:Context.t -> vif:[ `VIF ] Ref.t -> Locking_helpers.token -> unit
