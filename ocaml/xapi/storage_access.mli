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

val start_smapiv1_servers: unit -> unit
(** start listening for requests backed by SMAPIv1-style plugins *)

val on_xapi_start: __context:Context.t -> unit
(** Synchronises the known SM plugins with the SM table *)

val start: unit -> unit
(** once [start ()] returns the storage service is listening for requests on
    its unix domain socket. *)

(** [find_vdi __context sr vdi] returns the XenAPI VDI ref associated
    	with (sr, vdi) *)
val find_vdi: __context:Context.t -> Storage_interface.sr -> Storage_interface.vdi -> API.ref_VDI * API.vDI_t

(** [find_content __context ?sr content_id] returns the XenAPI VDI ref associated
    with [content_id] *)
val find_content: __context:Context.t -> ?sr:Storage_interface.sr -> Storage_interface.content_id -> API.ref_VDI * API.vDI_t

(** [vdi_info_of_vdi_rec __context vdi_rec] constructs a vdi_info record from information in the given VDI database record. *)
val vdi_info_of_vdi_rec : Context.t -> API.vDI_t -> Storage_interface.vdi_info

(** [bind __context pbd] causes the storage_access module to choose the most
        appropriate driver implementation for the given [pbd] *)
val bind: __context:Context.t -> pbd:API.ref_PBD -> Storage_interface.query_result

(** [unbind __context pbd] causes the storage access module to forget the association
    between [pbd] and driver implementation *)
val unbind: __context:Context.t -> pbd:API.ref_PBD -> unit

(** [make_service uuid type] returns the service record for a storage driver *)
val make_service: string -> string -> System_domains.service

(** RPC function for calling the main storage multiplexor *)
val rpc: Rpc.call -> Rpc.response

(** [external_rpc queue_name uri] for calling a particular storage implementation *)
val external_rpc: string -> (unit -> string) -> Rpc.call -> Rpc.response

(** [datapath_of_vbd domid userdevice] returns the name of the datapath which corresponds
    to device [userdevice] on domain [domid] *)
val datapath_of_vbd: domid:int -> device:string -> Storage_interface.dp

(** [presentative_datapath_of_vbd vm vdi] gives a presentative datapath for a potential
    vbd. If there is such datapath established (or can be established, given
    the VM is running), the result is the same as datapath_of_vbd; otherwise,
    it's a string artificially constructed based on VM uuid and VDI uuid. *)
val presentative_datapath_of_vbd: __context:Context.t -> vm:API.ref_VM -> vdi:API.ref_VDI -> Storage_interface.dp

(** [reset __context vm] declares that [vm] has reset and if it's a driver
    domain, we expect it to lose all state. *)
val reset: __context:Context.t -> vm:API.ref_VM -> unit

(** [transform_storage_exn f] runs [f], rethrowing any storage error as a nice XenAPI error *)
val transform_storage_exn: (unit -> 'a) -> 'a

(** [attach_and_activate __context vbd domid f] calls [f attach_info] where
    [attach_info] is the result of attaching a VDI which is also activated.
    This should be used everywhere except the migrate code, where we want fine-grained
    control of the ordering of attach/activate/deactivate/detach *)
val attach_and_activate: __context:Context.t -> vbd:API.ref_VBD -> domid:int -> (Storage_interface.attach_info -> 'a) -> 'a

(** [deactivate_and_detach __context vbd domid] idempotent function which ensures
    that any attached or activated VDI gets properly deactivated and detached. *)
val deactivate_and_detach: __context:Context.t -> vbd:API.ref_VBD -> domid:int -> unit

(** [is_attached __context vbd] returns true if the [vbd] has an attached
    or activated datapath. *)
val is_attached: __context:Context.t -> vbd:API.ref_VBD -> domid:int -> bool

(** [on_vdi __context vbd domid f] calls [f rpc dp sr vdi] which is
    useful for executing Storage_interface.Client.VDI functions, applying the
    standard convention mapping VBDs onto DPs *)
val on_vdi: __context:Context.t -> vbd:API.ref_VBD -> domid:int -> ((Rpc.call -> Rpc.response) -> Storage_interface.debug_info -> Storage_interface.dp -> Storage_interface.sr -> Storage_interface.vdi -> 'a) -> 'a

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

(** [create_sr __context sr name_label name_description physical_size] attempts to create an empty SR *)
val create_sr: __context:Context.t -> sr:API.ref_SR -> name_label:string -> name_description:string -> physical_size:int64 -> (string * string) list

(** [destroy_sr __context sr] attempts to cleanup and destroy a given SR *)
val destroy_sr: __context:Context.t -> sr:API.ref_SR -> and_vdis:(API.ref_VDI list) -> unit

val event_wait: Storage_interface.debug_info -> (Storage_interface.Dynamic.id -> bool) -> unit

val task_ended : Storage_interface.debug_info -> Storage_interface.Task.id -> bool

val success_task : Storage_interface.debug_info -> Storage_interface.Task.id -> Storage_interface.Task.t

val wait_for_task : Storage_interface.debug_info -> Storage_interface.Task.id -> Storage_interface.Task.id

val vdi_of_task : Storage_interface.debug_info -> Storage_interface.Task.t -> Storage_interface.vdi_info

val mirror_of_task : Storage_interface.debug_info -> Storage_interface.Task.t -> Storage_interface.Mirror.id

val register_task : Context.t -> Storage_interface.Task.id -> Storage_interface.Task.id

val unregister_task : Context.t -> Storage_interface.Task.id -> Storage_interface.Task.id

val register_mirror : Context.t -> Storage_interface.Mirror.id -> Storage_interface.Mirror.id

val unregister_mirror : Storage_interface.Mirror.id -> Storage_interface.Mirror.id

val add_to_progress_map : (float -> float) -> Storage_interface.Task.id -> Storage_interface.Task.id

val remove_from_progress_map : Storage_interface.Task.id -> Storage_interface.Task.id

val events_from_sm : unit -> unit

val task_cancel : __context:Context.t -> self:API.ref_task -> bool
