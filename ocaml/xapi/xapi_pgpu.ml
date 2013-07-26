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
module D=Debug.Debugger(struct let name="xapi" end)
open D

open Listext
open Threadext

let create ~__context ~pCI ~gPU_group ~host ~other_config =
	let pgpu = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.PGPU.create ~__context ~ref:pgpu ~uuid ~pCI ~gPU_group ~host ~other_config;
	debug "PGPU ref='%s' created (host = '%s')" (Ref.string_of pgpu) (Ref.string_of host);
	pgpu

let update_gpus ~__context ~host =
	let existing_pgpus = List.filter (fun (rf, rc) -> rc.API.pGPU_host = host) (Db.PGPU.get_all_records ~__context) in
	let class_id = Xapi_pci.lookup_class_id Xapi_pci.Display_controller in
	let pcis = List.filter (fun self ->
		Xapi_pci.int_of_id (Db.PCI.get_class_id ~__context ~self) = class_id &&
		Db.PCI.get_host ~__context ~self = host) (Db.PCI.get_all ~__context)
	in
	let rec find_or_create cur = function
		| [] -> cur
		| pci :: remaining_pcis ->
			let pgpu =
				try
					List.find (fun (rf, rc) -> rc.API.pGPU_PCI = pci) existing_pgpus
				with Not_found ->
					let self = create ~__context ~pCI:pci ~gPU_group:(Ref.null) ~host ~other_config:[] in
					let group = Xapi_gpu_group.find_or_create ~__context self in
					Helpers.call_api_functions ~__context (fun rpc session_id ->
						Client.Client.PGPU.set_GPU_group rpc session_id self group);
					self, Db.PGPU.get_record ~__context ~self
			in
			find_or_create (pgpu :: cur) remaining_pcis
	in
	let current_pgpus = find_or_create [] pcis in
	let obsolete_pgpus = List.set_difference existing_pgpus current_pgpus in
	List.iter (fun (self, _) -> Db.PGPU.destroy ~__context ~self) obsolete_pgpus

let gpu_group_m = Mutex.create ()
let set_GPU_group ~__context ~self ~value =
	debug "Move PGPU %s -> GPU group %s" (Db.PGPU.get_uuid ~__context ~self)
		(Db.GPU_group.get_uuid ~__context ~self:value);
	Mutex.execute gpu_group_m (fun () ->
		let pci = Db.PGPU.get_PCI ~__context ~self in

		(* Precondition: PGPU not currently in use by a VM *)
		let attached_vms = Db.PCI.get_attached_VMs ~__context ~self:pci in
		if attached_vms <> [] then
			raise (Api_errors.Server_error (Api_errors.pgpu_in_use_by_vm,
				List.map Ref.string_of attached_vms));

		(* Precondition: Moving PGPU from current group can't orphan VGPU *)
		let src_g = Db.PGPU.get_GPU_group ~__context ~self in
		let pgpu_is_singleton =
			(List.length (Db.GPU_group.get_PGPUs ~__context ~self:src_g) = 1)
		and pgpu_has_vgpus =
			((Db.GPU_group.get_VGPUs ~__context ~self:src_g) <> []) in
		if (pgpu_is_singleton && pgpu_has_vgpus) then
			raise (Api_errors.Server_error
				(Api_errors.pgpu_required_by_gpu_group, [Ref.string_of src_g]));

		let check_compatibility gpu_type group_types =
			match group_types with
			| [] -> true, [gpu_type]
			| _ -> List.mem gpu_type group_types, group_types in

		let gpu_type = Xapi_pci.string_of_pci ~__context ~self:pci
		and group_types = Db.GPU_group.get_GPU_types ~__context ~self:value in
		match check_compatibility gpu_type group_types with
		| true, new_types ->
			Db.PGPU.set_GPU_group ~__context ~self ~value;
			(* Group inherits the device type *)
			Xapi_gpu_group.set_GPU_types ~__context ~self:value ~value:new_types;
			let vgpu_types = Db.GPU_group.get_supported_VGPU_types
				~__context ~self:value in
			debug "PGPU %s moved to GPU group %s. Group GPU types = [ %s ]; Supported vGPU types = [ %s ]."
				(Db.PGPU.get_uuid ~__context ~self)
				(Db.GPU_group.get_uuid ~__context ~self:value)
				(String.concat "; " new_types)
				(String.concat "; " (List.map
					(fun self -> Db.VGPU_type.get_model_name ~__context ~self)
					vgpu_types))
		| false, _ ->
			raise (Api_errors.Server_error
				(Api_errors.pgpu_not_compatible_with_gpu_group,
				[gpu_type; "[" ^ String.concat ", " group_types ^ "]"]))
	)
