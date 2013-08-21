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
module D=Debug.Debugger(struct let name="xapi" end)
open D

let assert_VGPU_type_supported ~__context ~self ~vgpu_type =
	let supported_VGPU_types =
		Db.PGPU.get_supported_VGPU_types ~__context ~self
	in
	if not (List.mem vgpu_type supported_VGPU_types)
	then raise (Api_errors.Server_error
		(Api_errors.vgpu_type_not_supported,
			List.map Ref.string_of (vgpu_type :: supported_VGPU_types)))

let assert_VGPU_type_enabled ~__context ~self ~vgpu_type =
	assert_VGPU_type_supported ~__context ~self ~vgpu_type;
	let enabled_VGPU_types =
		Db.PGPU.get_enabled_VGPU_types ~__context ~self
	in
	if not (List.mem vgpu_type enabled_VGPU_types)
	then raise (Api_errors.Server_error
		(Api_errors.vgpu_type_not_enabled,
			List.map Ref.string_of (vgpu_type :: enabled_VGPU_types)))

let assert_VGPU_type_allowed ~__context ~self ~vgpu_type =
	assert_VGPU_type_enabled ~__context ~self ~vgpu_type;
	let resident_VGPUs = Db.PGPU.get_resident_VGPUs ~__context ~self in
	(match resident_VGPUs with
	| [] -> ()
	| resident_VGPU :: _ ->
		let running_type =
			Db.VGPU.get_type ~__context ~self:resident_VGPU
		in
		if running_type <> vgpu_type
		then raise (Api_errors.Server_error (
			Api_errors.vgpu_type_not_compatible_with_running_type,
			[
				Ref.string_of self;
				Ref.string_of vgpu_type;
				Ref.string_of running_type;
			])))

let assert_no_resident_VGPUs_of_type ~__context ~self ~vgpu_type =
	let open Db_filter_types in
	match Db.VGPU.get_records_where ~__context
		~expr:(And
			(Eq (Field "resident_on", Literal (Ref.string_of self)),
			Eq (Field "vgpu_type", Literal (Ref.string_of vgpu_type))))
	with
	| [] -> ()
	| vgpus_and_records ->
		let vms =
			List.map
				(fun (vgpu, _) -> Db.VGPU.get_VM ~__context ~self:vgpu)
				vgpus_and_records
		in
		raise (Api_errors.Server_error
			(Api_errors.pgpu_in_use_by_vm, List.map Ref.string_of vms))

let get_remaining_capacity_internal ~__context ~self ~vgpu_type =
	(* Assume this PGPU is OK to run this type of VGPU, but we still need to check
	 * whether there is capacity remaining. *)
	if Xapi_vgpu_type.requires_passthrough ~__context ~self:vgpu_type
	then begin
		(* For passthrough VGPUs, we just check that there are functions
		 * available. *)
		let pci = Db.PGPU.get_PCI ~__context ~self in
		Int64.of_int (Pciops.get_free_functions ~__context pci)
	end else begin
		(* For virtual VGPUs, we calculate the number of times the VGPU_type's
		 * size fits into the PGPU's (size - utilisation). *)
		let pgpu_size = Db.PGPU.get_size ~__context ~self in
		let resident_VGPUs = Db.PGPU.get_resident_VGPUs ~__context ~self in
		let utilisation =
			List.fold_left
				(fun acc vgpu ->
					let _type = Db.VGPU.get_type ~__context ~self:vgpu in
					let vgpu_size =
						Db.VGPU_type.get_size ~__context ~self:_type
					in
					Int64.add acc vgpu_size)
				0L resident_VGPUs
		in
		let new_vgpu_size =
			Db.VGPU_type.get_size ~__context ~self:vgpu_type
		in
		Int64.div (Int64.sub pgpu_size utilisation) new_vgpu_size
	end

let get_remaining_capacity ~__context ~self ~vgpu_type =
	try
		assert_VGPU_type_allowed ~__context ~self ~vgpu_type;
		get_remaining_capacity_internal ~__context ~self ~vgpu_type
	with Api_errors.Server_error (code, args) ->
		0L

let assert_capacity_exists_for_VGPU ~__context ~self ~vgpu =
	let new_type = Db.VGPU.get_type ~__context ~self:vgpu in
	assert_VGPU_type_allowed ~__context ~self ~vgpu_type:new_type;
	if get_remaining_capacity_internal ~__context ~self ~vgpu_type:new_type <= 0L
	then
		let resident_VGPUs = Db.PGPU.get_resident_VGPUs ~__context ~self in
		let resident_VMs =
			List.map
				(fun vgpu -> Db.VGPU.get_VM ~__context ~self:vgpu)
				resident_VGPUs
		in
		raise (Api_errors.Server_error (Api_errors.pgpu_in_use_by_vm,
			List.map Ref.string_of resident_VMs))
