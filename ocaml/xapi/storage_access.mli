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
(**
 * @group Storage
 *)
 
(** [rpc_of_sr __context sr] returns an Rpc.call -> Rpc.response function
    for talking to the implementation of [sr], which could be in xapi, in domain 0
    or in a driver domain. *)
val rpc_of_sr: __context:Context.t -> sr:API.ref_SR -> Rpc.call -> Rpc.response

(** [rpc_of_vbd __context vbd] returns an Rpc.call -> Rpc.response function
    for talking to the SR underlying the VDI corresponding to [vbd]. See rpc_of_sr *)
val rpc_of_vbd: __context:Context.t -> vbd:API.ref_VBD -> Rpc.call -> Rpc.response

(** RPC function for calling the main storage multiplexor *)
val rpc: Rpc.call -> Rpc.response

(** [datapath_of_vbd domid devid] returns the name of the datapath which corresponds
    to device [devid] on domain [domid] *)
val datapath_of_vbd: domid:int -> devid:int -> Storage_interface.dp

val expect_vdi: (Storage_interface.physical_device -> 'a) -> Storage_interface.result -> 'a

val expect_unit: (unit -> 'a) -> Storage_interface.result -> 'a

(** [attach_and_activate __context vbd domid f] calls [f physical_device] where
    [physical_device] is the result of attaching a VDI which is also activated.
    This should be used everywhere except the migrate code, where we want fine-grained
    control of the ordering of attach/activate/deactivate/detach *)
val attach_and_activate: __context:Context.t -> vbd:API.ref_VBD -> domid:int -> (Storage_interface.physical_device -> 'a) -> 'a

(** [deactivate_and_detach __context vbd domid] idempotent function which ensures
    that any attached or activated VDI gets properly deactivated and detached. *)
val deactivate_and_detach: __context:Context.t -> vbd:API.ref_VBD -> domid:int -> unit

(** [on_vdi __context vbd domid f] calls [f rpc dp sr vdi] which is
    useful for executing Storage_interface.Client.VDI functions, applying the
    standard convention mapping VBDs onto DPs *)
val on_vdi: __context:Context.t -> vbd:API.ref_VBD -> domid:int -> ((Rpc.call -> Rpc.response) -> Storage_interface.task -> Storage_interface.dp -> Storage_interface.sr -> Storage_interface.vdi -> 'a) -> 'a

(** [resynchronise_pbds __context pbds] sets the currently_attached state of
    each of [pbd] to match the state of the storage layer. *)
val resynchronise_pbds: __context:Context.t -> pbds:API.ref_PBD list -> unit

(** [refresh_local_vdi_activations __context] updates the VDI.sm_config fields to 
    match the state stored within the storage layer. *)
val refresh_local_vdi_activations: __context:Context.t -> unit

(** [vbd_attach_order __context vbds] returns vbds in the order which xapi should
	attempt to attach them. *)
val vbd_attach_order: __context:Context.t -> API.ref_VBD list -> API.ref_VBD list

(** [vbd_detach_order __context vbds] returns vbds in the order which xapi should
	attempt to detach them. *)
val vbd_detach_order: __context:Context.t -> API.ref_VBD list -> API.ref_VBD list

(** [diagnostics __context] returns a printable snapshot of SM system state *)
val diagnostics: __context:Context.t -> string

(** [dp_destroy __context dp allow_leak] attempts to cleanup and detach a given DP *)
val dp_destroy: __context:Context.t -> string -> bool -> unit

(** [destroy_sr __context sr] attempts to cleanup and destroy a given SR *)
val destroy_sr: __context:Context.t -> sr:API.ref_SR -> unit
