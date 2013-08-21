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

open Xapi_vgpu_type

let k100 = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K100";
	framebuffer_size = 268435456L;
	max_heads = 2L;
	size = Int64.div Constants.pgpu_default_size 8L;
	internal_config = [];
}

let k140q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K140Q";
	framebuffer_size = 1006632960L;
	max_heads = 2L;
	size = Int64.div Constants.pgpu_default_size 4L;
	internal_config = [];
}

let k200 = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K200";
	framebuffer_size = 268435456L;
	max_heads = 2L;
	size = Int64.div Constants.pgpu_default_size 8L;
	internal_config = [];
}

let k240q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K240Q";
	framebuffer_size = 1006632960L;
	max_heads = 2L;
	size = Int64.div Constants.pgpu_default_size 4L;
	internal_config = [];
}

let k260q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K260Q";
	framebuffer_size = 2013265920L;
	max_heads = 4L;
	size = Int64.div Constants.pgpu_default_size 2L;
	internal_config = [];
}

let k1_vgpu_types = [
	k100;
	k140q;
	entire_gpu;
]

let k2_vgpu_types = [
	k200;
	k240q;
	k260q;
	entire_gpu;
]

(* Represents the state of a PGPU, its supported and enabled VGPU types, and
 * the types of the VGPUs running on it. *)
type pgpu_state = {
	supported_VGPU_types: vgpu_type list;
	enabled_VGPU_types: vgpu_type list;
	resident_VGPU_types: vgpu_type list;
}

let default_k1 = {
	supported_VGPU_types = k1_vgpu_types;
	enabled_VGPU_types = k1_vgpu_types;
	resident_VGPU_types = [];
}

let default_k2 = {
	supported_VGPU_types = k2_vgpu_types;
	enabled_VGPU_types = k2_vgpu_types;
	resident_VGPU_types = [];
}

let string_of_vgpu_type vgpu_type =
	vgpu_type.model_name

let string_of_pgpu_state pgpu =
	Printf.sprintf "{supported: %s; enabled: %s; resident: %s}"
		(Test_common.string_of_string_list
			(List.map string_of_vgpu_type pgpu.supported_VGPU_types))
		(Test_common.string_of_string_list
			(List.map string_of_vgpu_type pgpu.enabled_VGPU_types))
		(Test_common.string_of_string_list
			(List.map string_of_vgpu_type pgpu.resident_VGPU_types))

let make_vgpu ~__context pgpu_ref vgpu_type =
	let vgpu_type_ref = find_or_create ~__context vgpu_type in
	let vgpu_ref = Ref.make () in
	Test_common.make_vgpu ~__context
		~ref:vgpu_ref ~_type:vgpu_type_ref
		~resident_on:pgpu_ref ();
	vgpu_ref

let make_pgpu ~__context pgpu =
	let pCI = Ref.make () in
	Test_common.make_pci ~__context ~ref:pCI ();
	let supported_VGPU_types =
		List.map (find_or_create ~__context) pgpu.supported_VGPU_types
	in
	let enabled_VGPU_types =
		List.map (find_or_create ~__context) pgpu.supported_VGPU_types
	in
	let pgpu_ref = Ref.make () in
	Test_common.make_pgpu ~__context
		~ref:pgpu_ref
		~pCI
		~supported_VGPU_types
		~enabled_VGPU_types ();
	List.iter
		(fun vgpu_type ->
			let (_: API.ref_VGPU) = (make_vgpu ~__context pgpu_ref vgpu_type) in ())
		pgpu.resident_VGPU_types;
	pgpu_ref
