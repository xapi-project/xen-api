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
	max_resolution_x = 1920L;
	max_resolution_y = 1200L;
	size = Int64.div Constants.pgpu_default_size 8L;
	internal_config = [];
	implementation = `nvidia;
}

let k140q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K140Q";
	framebuffer_size = 1006632960L;
	max_heads = 2L;
	max_resolution_x = 2560L;
	max_resolution_y = 1600L;
	size = Int64.div Constants.pgpu_default_size 4L;
	internal_config = [];
	implementation = `nvidia;
}

let k200 = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K200";
	framebuffer_size = 268435456L;
	max_heads = 2L;
	max_resolution_x = 1920L;
	max_resolution_y = 1200L;
	size = Int64.div Constants.pgpu_default_size 8L;
	internal_config = [];
	implementation = `nvidia;
}

let k240q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K240Q";
	framebuffer_size = 1006632960L;
	max_heads = 2L;
	max_resolution_x = 2560L;
	max_resolution_y = 1600L;
	size = Int64.div Constants.pgpu_default_size 4L;
	internal_config = [];
	implementation = `nvidia;
}

let k260q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K260Q";
	framebuffer_size = 2013265920L;
	max_heads = 4L;
	max_resolution_x = 2560L;
	max_resolution_y = 1600L;
	size = Int64.div Constants.pgpu_default_size 2L;
	internal_config = [];
	implementation = `nvidia;
}

let k1_vgpu_types = [
	k100;
	k140q;
	passthrough_gpu;
]

let k2_vgpu_types = [
	k200;
	k240q;
	k260q;
	passthrough_gpu;
]

(* Represents the state of a PGPU, its supported and enabled VGPU types, and
 * the types of the VGPUs running and scheduled to run on it. *)
type pgpu_state = {
	supported_VGPU_types: vgpu_type list;
	enabled_VGPU_types: vgpu_type list;
	resident_VGPU_types: vgpu_type list;
	scheduled_VGPU_types: vgpu_type list;
}

let default_k1 = {
	supported_VGPU_types = k1_vgpu_types;
	enabled_VGPU_types = k1_vgpu_types;
	resident_VGPU_types = [];
	scheduled_VGPU_types = [];
}

let default_k2 = {
	supported_VGPU_types = k2_vgpu_types;
	enabled_VGPU_types = k2_vgpu_types;
	resident_VGPU_types = [];
	scheduled_VGPU_types = [];
}

let string_of_vgpu_type vgpu_type =
	vgpu_type.model_name

let string_of_pgpu_state pgpu =
	Printf.sprintf "{supported: %s; enabled: %s; resident: %s; scheduled: %s}"
		(Test_common.string_of_string_list
			(List.map string_of_vgpu_type pgpu.supported_VGPU_types))
		(Test_common.string_of_string_list
			(List.map string_of_vgpu_type pgpu.enabled_VGPU_types))
		(Test_common.string_of_string_list
			(List.map string_of_vgpu_type pgpu.resident_VGPU_types))
		(Test_common.string_of_string_list
			(List.map string_of_vgpu_type pgpu.scheduled_VGPU_types))

let make_vgpu ~__context
		?(resident_on=Ref.null)
		?(scheduled_to_be_resident_on=Ref.null)
		vgpu_type =
	let vgpu_type_ref = find_or_create ~__context vgpu_type in
	(* For the passthrough VGPU type, create a VM and mark it as attached to the
	 * PGPU's PCI device. *)
	let vm_ref_opt =
		if (Xapi_vgpu_type.requires_passthrough ~__context ~self:vgpu_type_ref)
			&& (Db.is_valid_ref __context resident_on)
		then begin
			let vm_ref = Test_common.make_vm ~__context () in
			let pci_ref = Db.PGPU.get_PCI ~__context ~self:resident_on in
			Db.PCI.add_attached_VMs ~__context ~self:pci_ref ~value:vm_ref;
			Some vm_ref
		end else None
	in
	Test_common.make_vgpu ~__context
		~vM:(Opt.default Ref.null vm_ref_opt)
		~_type:vgpu_type_ref
		~resident_on
		~scheduled_to_be_resident_on ()

let make_pgpu ~__context ?(host=Ref.null) ?(gPU_group=Ref.null) pgpu =
	let pCI = Test_common.make_pci ~__context ~host ~functions:1L () in
	let supported_VGPU_types =
		List.map (find_or_create ~__context) pgpu.supported_VGPU_types
	in
	let enabled_VGPU_types =
		List.map (find_or_create ~__context) pgpu.supported_VGPU_types
	in
	let pgpu_ref = Test_common.make_pgpu ~__context
		~pCI
		~host
		~gPU_group
		~supported_VGPU_types
		~enabled_VGPU_types () in
	List.iter
		(fun vgpu_type ->
			let (_: API.ref_VGPU) =
				(make_vgpu ~__context ~resident_on:pgpu_ref vgpu_type) in ())
		pgpu.resident_VGPU_types;
	List.iter
		(fun vgpu_type ->
			let (_: API.ref_VGPU) =
				(make_vgpu ~__context ~scheduled_to_be_resident_on:pgpu_ref vgpu_type)
			in ())
		pgpu.scheduled_VGPU_types;
	pgpu_ref
