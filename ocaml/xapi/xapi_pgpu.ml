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

let create ~__context ~pCI ~gPU_group ~host ~other_config ~supported_VGPU_types ~size =
	let pgpu = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.PGPU.create ~__context ~ref:pgpu ~uuid ~pCI
		~gPU_group ~host ~other_config ~size;
	Db.PGPU.set_supported_VGPU_types ~__context
		~self:pgpu ~value:supported_VGPU_types;
	Db.PGPU.set_enabled_VGPU_types ~__context
		~self:pgpu ~value:supported_VGPU_types;
	debug "PGPU ref='%s' created (host = '%s')" (Ref.string_of pgpu) (Ref.string_of host);
	pgpu

let update_gpus ~__context ~host =
	let pci_id_blacklist =
		match Xapi_pci.get_system_display_device () with
		| Some device -> [device]
		| None -> []
	in
	let existing_pgpus = List.filter (fun (rf, rc) -> rc.API.pGPU_host = host) (Db.PGPU.get_all_records ~__context) in
	let class_id = Xapi_pci.lookup_class_id Xapi_pci.Display_controller in
	let pcis = List.filter (fun self ->
		Xapi_pci.int_of_id (Db.PCI.get_class_id ~__context ~self) = class_id &&
		Db.PCI.get_host ~__context ~self = host) (Db.PCI.get_all ~__context)
	in
	let pci_db = Pci_db.of_file Pci_db.pci_ids_path in
	let rec find_or_create cur = function
		| [] -> cur
		| pci :: remaining_pcis ->
			let pci_id = Db.PCI.get_pci_id ~__context ~self:pci in
			if List.mem pci_id pci_id_blacklist
			then find_or_create cur remaining_pcis
			else begin
				let supported_VGPU_types =
					Xapi_vgpu_type.find_or_create_supported_types ~__context
					~pci_db pci in
				let pgpu =
					try
						let (rf, rc) = List.find (fun (_, rc) -> rc.API.pGPU_PCI = pci) existing_pgpus in
						(* Pick up any new supported vGPU configs on the host *)
						Db.PGPU.set_supported_VGPU_types ~__context ~self:rf ~value:supported_VGPU_types;
						let pruned_enabled_types = List.filter
							(fun t -> List.mem t supported_VGPU_types)
							(Db.PGPU.get_enabled_VGPU_types ~__context ~self:rf) in
						Db.PGPU.set_enabled_VGPU_types ~__context ~self:rf ~value:pruned_enabled_types;
						(rf, rc)
					with Not_found ->
						let self = create ~__context ~pCI:pci
								~gPU_group:(Ref.null) ~host ~other_config:[]
								~supported_VGPU_types
								~size:Constants.pgpu_default_size
						in
						let group = Xapi_gpu_group.find_or_create ~__context self in
						Helpers.call_api_functions ~__context (fun rpc session_id ->
							Client.Client.PGPU.set_GPU_group rpc session_id self group);
						self, Db.PGPU.get_record ~__context ~self
				in
				find_or_create (pgpu :: cur) remaining_pcis
			end
	in
	let current_pgpus = find_or_create [] pcis in
	let obsolete_pgpus = List.set_difference existing_pgpus current_pgpus in
	List.iter (fun (self, _) -> Db.PGPU.destroy ~__context ~self) obsolete_pgpus

let add_enabled_VGPU_types ~__context ~self ~value =
	Xapi_pgpu_helpers.assert_VGPU_type_supported ~__context
		~self ~vgpu_type:value;
	Db.PGPU.add_enabled_VGPU_types ~__context ~self ~value

let remove_enabled_VGPU_types ~__context ~self ~value =
	Xapi_pgpu_helpers.assert_no_resident_VGPUs_of_type ~__context
		~self ~vgpu_type:value;
	Db.PGPU.remove_enabled_VGPU_types ~__context ~self ~value

let set_enabled_VGPU_types ~__context ~self ~value =
	List.iter
		(fun vgpu_type ->
			Xapi_pgpu_helpers.assert_VGPU_type_supported ~__context ~self ~vgpu_type)
		value;
	Db.PGPU.set_enabled_VGPU_types ~__context ~self ~value

let gpu_group_m = Mutex.create ()
let set_GPU_group ~__context ~self ~value =
	debug "Move PGPU %s -> GPU group %s" (Db.PGPU.get_uuid ~__context ~self)
		(Db.GPU_group.get_uuid ~__context ~self:value);
	Mutex.execute gpu_group_m (fun () ->
		(* Precondition: PGPU has no resident VGPUs *)
		let resident_vgpus = Db.PGPU.get_resident_VGPUs ~__context ~self in
		if resident_vgpus <> [] then begin
			let resident_vms = List.map
				(fun self -> Db.VGPU.get_VM ~__context ~self) resident_vgpus in
			raise (Api_errors.Server_error (Api_errors.pgpu_in_use_by_vm,
				List.map Ref.string_of resident_vms))
		end;

		let check_compatibility gpu_type group_types =
			match group_types with
			| [] -> true, [gpu_type]
			| _ -> List.mem gpu_type group_types, group_types in

		let pci = Db.PGPU.get_PCI ~__context ~self in
		let gpu_type = Xapi_pci.string_of_pci ~__context ~self:pci
		and group_types = Db.GPU_group.get_GPU_types ~__context ~self:value in
		match check_compatibility gpu_type group_types with
		| true, new_types ->
			Db.PGPU.set_GPU_group ~__context ~self ~value;
			(* Group inherits the device type *)
			Db.GPU_group.set_GPU_types ~__context ~self:value ~value:new_types;
			debug "PGPU %s moved to GPU group %s. Group GPU types = [ %s ]."
				(Db.PGPU.get_uuid ~__context ~self)
				(Db.GPU_group.get_uuid ~__context ~self:value)
				(String.concat "; " new_types)
		| false, _ ->
			raise (Api_errors.Server_error
				(Api_errors.pgpu_not_compatible_with_gpu_group,
				[gpu_type; "[" ^ String.concat ", " group_types ^ "]"]))
	)

let get_remaining_capacity ~__context ~self ~vgpu_type =
	match Xapi_pgpu_helpers.get_remaining_capacity_internal ~__context ~self ~vgpu_type with
	| Either.Left _ -> 0L
	| Either.Right capacity -> capacity

let assert_can_run_VGPU ~__context ~self ~vgpu =
	let vgpu_type = Db.VGPU.get_type ~__context ~self:vgpu in
	Xapi_pgpu_helpers.assert_capacity_exists_for_VGPU_type ~__context ~self ~vgpu_type
