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

val assert_VGPU_type_enabled : __context:Context.t ->
	self:API.ref_PGPU -> value:API.ref_VGPU_type -> unit

val assert_VGPU_type_supported : __context:Context.t ->
	self:API.ref_PGPU -> value:API.ref_VGPU_type -> unit

(** Any VGPUs already running on this PGPU must have the same type as the new
 *  For now, we only allow one VGPU_type to run on a PGPU at any one time. *)
val assert_VGPU_type_allowed : __context:Context.t ->
	self:API.ref_PGPU -> value:API.ref_VGPU_type -> unit

val assert_no_resident_VGPUs_of_type : __context:Context.t ->
	self:API.ref_PGPU -> vgpu_type:API.ref_VGPU_type -> unit
