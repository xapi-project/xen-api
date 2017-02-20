(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
(** Module that defines API functions for PGPU objects
 * @group Graphics
*)

(** Synchronise the PGPU objects in the database with the actual devices in the host.
 *  The caller must ensure that the localhost parameter is indeed the local host. *)
val update_gpus : __context:Context.t -> localhost:API.ref_host -> unit

(** Enable one of the VGPU types supported by the PGPU. *)
val add_enabled_VGPU_types : __context:Context.t ->
  self:API.ref_PGPU -> value:API.ref_VGPU_type -> unit

(** Disable one of the VGPU types supported by the PGPU. *)
val remove_enabled_VGPU_types : __context:Context.t ->
  self:API.ref_PGPU -> value:API.ref_VGPU_type -> unit

(** Enable a set of VGPU types supported by the PGPU. *)
val set_enabled_VGPU_types : __context:Context.t ->
  self:API.ref_PGPU -> value:API.ref_VGPU_type list -> unit

(** Move the PGPU to a new GPU group. *)
val set_GPU_group : __context:Context.t -> self:API.ref_PGPU ->
  value: API.ref_GPU_group -> unit

(* Return the number of VGPUs of the specified type for which capacity
 * remains on the PGPU. *)
val get_remaining_capacity : __context:Context.t ->
  self:API.ref_PGPU -> vgpu_type:API.ref_VGPU_type -> int64

(** Check whether a VGPU can run on a particular PGPU. *)
val assert_can_run_VGPU : __context:Context.t -> self:API.ref_PGPU ->
  vgpu:API.ref_VGPU -> unit

val enable_dom0_access : __context:Context.t -> self:API.ref_PGPU ->
  API.pgpu_dom0_access

val disable_dom0_access : __context:Context.t -> self:API.ref_PGPU ->
  API.pgpu_dom0_access

(* For AMD MxGPU. Acts on the local host only.
 * Ensures that the "gim" kernel module is loaded on localhost,
 * that PCI DB entries exist for the VF PCI devices reported by the module,
 * and that those entries have the "physical_function" field set correctly. *)
val mxgpu_vf_setup : __context:Context.t -> unit
