(*
 * Copyright (C) Citrix Systems Inc.
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

(** Check that the specified type of VGPU is enabled on this PGPU. *)
val assert_VGPU_type_enabled :
  __context:Context.t ->
  self:API.ref_PGPU ->
  vgpu_type:API.ref_VGPU_type -> unit

(** Check that the specified type of VGPU is supported on this PGPU. *)
val assert_VGPU_type_supported :
  __context:Context.t ->
  self:API.ref_PGPU ->
  vgpu_type:API.ref_VGPU_type -> unit

(** Any VGPUs already resident on this PGPU must be compatible with the type of
 *  the VGPUs already running on the PGPU. For now, we only allow one VGPU_type
 *  to run on a PGPU at any one time. *)
val assert_VGPU_type_allowed :
  __context:Context.t ->
  self:API.ref_PGPU ->
  vgpu_type:API.ref_VGPU_type -> unit

(** Check that no VMs resident on this PGPU have the specified type. *)
val assert_no_resident_VGPUs_of_type :
  __context:Context.t ->
  self:API.ref_PGPU ->
  vgpu_type:API.ref_VGPU_type -> unit

(* Return the number of VGPUs of the specified type for which capacity
 * remains on the PGPU, or an exception if the remaining capacity is zero. *)
val get_remaining_capacity_internal :
  __context:Context.t ->
  self:API.ref_PGPU ->
  vgpu_type:API.ref_VGPU_type -> (exn, int64) Stdext.Either.t

(* Return the number of VGPUs of the specified type for which capacity
 * remains on the PGPU. *)
val get_remaining_capacity :
  __context:Context.t ->
  self:API.ref_PGPU ->
  vgpu_type:API.ref_VGPU_type -> int64

(** Check that the PGPU has capacity to run the specified VGPU. *)
val assert_capacity_exists_for_VGPU_type :
  __context:Context.t ->
  self:API.ref_PGPU ->
  vgpu_type:API.ref_VGPU_type -> unit

(** Check that the PGPU selected is compatible with the VM VGPU.
 *  Currently checks only nvml compatibility if Nvidia VGPUs 
 *  are detected. *)
val assert_destination_pgpu_is_compatible_with_vm :
  __context:Context.t ->
  vm:API.ref_VM ->
  vgpu:API.ref_VGPU ->
  pgpu:API.ref_PGPU ->
  host:API.ref_host ->
  ?remote:(Rpc.call -> Rpc.response Client.Id.t) * [<`session] Ref.t -> unit -> unit

(** Check that the host has a PGPU compatible with the VM VGPU.
 *  Currently checks only nvml compatibility if Nvidia VGPUs 
 *  are detected. *)
val assert_destination_has_pgpu_compatible_with_vm : 
  __context:Context.t ->
  vm:API.ref_VM ->
  vgpu_map:(API.ref_VGPU * API.ref_GPU_group) list ->
  host:API.ref_host ->
  ?remote:(Rpc.call -> Rpc.response Client.Id.t) * [<`session] Ref.t -> unit -> unit
