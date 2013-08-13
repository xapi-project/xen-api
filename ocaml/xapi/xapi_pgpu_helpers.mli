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

(* Check that the specified type of VGPU is enabled on this PGPU. *)
val assert_VGPU_type_enabled : __context:Context.t ->
	self:API.ref_PGPU -> vgpu_type:API.ref_VGPU_type -> unit

(* Check that the specified type of VGPU is supported on this PGPU. *)
val assert_VGPU_type_supported : __context:Context.t ->
	self:API.ref_PGPU -> vgpu_type:API.ref_VGPU_type -> unit

(** Any VGPUs already resident on this PGPU be compatible with the type of the
 *  VGPUs already running on the PGPU. For now, we only allow one VGPU_type to
 *  run on a PGPU at any one time. *)
val assert_VGPU_type_allowed : __context:Context.t ->
	self:API.ref_PGPU -> vgpu_type:API.ref_VGPU_type -> unit

(** Check that no VMs resident on this PGPU have the specified type. *)
val assert_no_resident_VGPUs_of_type : __context:Context.t ->
	self:API.ref_PGPU -> vgpu_type:API.ref_VGPU_type -> unit

(** Check that the PGPU has capacity to run the specified VGPU. *)
val assert_capacity_exists_for_VGPU : __context:Context.t ->
	self:API.ref_PGPU -> vgpu:API.ref_VGPU -> unit
