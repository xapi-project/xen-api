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

let calculate_max_capacities ~__context ~pCI ~size ~supported_VGPU_types =
	List.map
		(fun vgpu_type ->
			let max_capacity =
				if Xapi_vgpu_type.requires_passthrough ~__context ~self:vgpu_type
				then Db.PCI.get_functions ~__context ~self:pCI
				else Int64.div size (Db.VGPU_type.get_size ~__context ~self:vgpu_type)
			in
			vgpu_type, max_capacity)
		supported_VGPU_types

let create ~__context ~pCI ~gPU_group ~host ~other_config ~supported_VGPU_types ~size =
	let pgpu = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let supported_VGPU_max_capacities =
		calculate_max_capacities ~__context ~pCI ~size ~supported_VGPU_types
	in
	Db.PGPU.create ~__context ~ref:pgpu ~uuid ~pCI
		~gPU_group ~host ~other_config ~size
		~supported_VGPU_max_capacities;
	Db.PGPU.set_supported_VGPU_types ~__context
		~self:pgpu ~value:supported_VGPU_types;
	Db.PGPU.set_enabled_VGPU_types ~__context
		~self:pgpu ~value:supported_VGPU_types;
	debug "PGPU ref='%s' created (host = '%s')" (Ref.string_of pgpu) (Ref.string_of host);
	pgpu

let update_gpus ~__context ~host =
	let system_display_device = Xapi_pci.get_system_display_device () in
	let existing_pgpus = List.filter (fun (rf, rc) -> rc.API.pGPU_host = host) (Db.PGPU.get_all_records ~__context) in
	let class_id = Xapi_pci.lookup_class_id Xapi_pci.Display_controller in
	let pcis = List.filter (fun self ->
		Xapi_pci.int_of_id (Db.PCI.get_class_id ~__context ~self) = class_id &&
		Db.PCI.get_host ~__context ~self = host) (Db.PCI.get_all ~__context)
	in
	let pci_db = Pci_db.open_default () in
	let rec find_or_create cur = function
		| [] -> cur
		| pci :: remaining_pcis ->
			let pci_id = Db.PCI.get_pci_id ~__context ~self:pci in
			let supported_VGPU_types =
				if system_display_device = (Some pci_id)
				then []
				else Xapi_vgpu_type.find_or_create_supported_types ~__context ~pci_db pci
			in
			let pgpu =
				try
					let (rf, rc) = List.find (fun (_, rc) -> rc.API.pGPU_PCI = pci) existing_pgpus in
					let old_supported_VGPU_types =
						Db.PGPU.get_supported_VGPU_types ~__context ~self:rf in
					let old_enabled_VGPU_types =
						Db.PGPU.get_enabled_VGPU_types ~__context ~self:rf in
					(* Pick up any new supported vGPU configs on the host *)
					Db.PGPU.set_supported_VGPU_types ~__context ~self:rf ~value:supported_VGPU_types;
					(* Calculate the maximum capacities of the supported types. *)
					let max_capacities =
						calculate_max_capacities
							~__context
							~pCI:pci
							~size:(Db.PGPU.get_size ~__context ~self:rf)
							~supported_VGPU_types
					in
					Db.PGPU.set_supported_VGPU_max_capacities ~__context
						~self:rf ~value:max_capacities;
					(* Enable any new supported types. *)
					let new_types_to_enable =
						List.filter
							(fun t -> not (List.mem t old_supported_VGPU_types))
							supported_VGPU_types
					in
					(* Disable any types which are no longer supported. *)
					let pruned_enabled_types =
						List.filter
							(fun t -> List.mem t supported_VGPU_types)
							old_enabled_VGPU_types
					in
					Db.PGPU.set_enabled_VGPU_types ~__context
						~self:rf
						~value:(pruned_enabled_types @ new_types_to_enable);
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
	in
	let current_pgpus = find_or_create [] pcis in
	let obsolete_pgpus = List.set_difference existing_pgpus current_pgpus in
	List.iter (fun (self, _) -> Db.PGPU.destroy ~__context ~self) obsolete_pgpus;
	(* Update the supported/enabled VGPU types on any affected GPU groups. *)
	let groups_to_update = List.setify
		(List.map
			(fun (_, pgpu_rec) -> pgpu_rec.API.pGPU_GPU_group)
			(current_pgpus @ obsolete_pgpus))
	in
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			List.iter
				(fun gpu_group ->
					let open Client in
					Client.GPU_group.update_enabled_VGPU_types
						~rpc ~session_id ~self:gpu_group;
					Client.GPU_group.update_supported_VGPU_types
						~rpc ~session_id ~self:gpu_group)
				groups_to_update)

let update_group_enabled_VGPU_types ~__context ~self =
	let group = Db.PGPU.get_GPU_group ~__context ~self in
	if Db.is_valid_ref __context group
	then Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:group

let pgpu_m = Mutex.create ()

let add_enabled_VGPU_types ~__context ~self ~value =
	Mutex.execute pgpu_m (fun () ->
		Xapi_pgpu_helpers.assert_VGPU_type_supported ~__context
			~self ~vgpu_type:value;
		Db.PGPU.add_enabled_VGPU_types ~__context ~self ~value;
		update_group_enabled_VGPU_types ~__context ~self
	)

let remove_enabled_VGPU_types ~__context ~self ~value =
	Mutex.execute pgpu_m (fun () ->
		Xapi_pgpu_helpers.assert_no_resident_VGPUs_of_type ~__context
			~self ~vgpu_type:value;
		Db.PGPU.remove_enabled_VGPU_types ~__context ~self ~value;
		update_group_enabled_VGPU_types ~__context ~self
	)

let set_enabled_VGPU_types ~__context ~self ~value =
	Mutex.execute pgpu_m (fun () ->
		let current_types = Db.PGPU.get_enabled_VGPU_types ~__context ~self in
		let to_enable = List.set_difference value current_types
		and to_disable = List.set_difference current_types value in
		List.iter (fun vgpu_type ->
			Xapi_pgpu_helpers.assert_VGPU_type_supported ~__context ~self ~vgpu_type)
			to_enable;
		List.iter (fun vgpu_type ->
			Xapi_pgpu_helpers.assert_no_resident_VGPUs_of_type ~__context ~self ~vgpu_type)
			to_disable;
		Db.PGPU.set_enabled_VGPU_types ~__context ~self ~value;
		update_group_enabled_VGPU_types ~__context ~self
	)

let set_GPU_group ~__context ~self ~value =
	debug "Move PGPU %s -> GPU group %s" (Db.PGPU.get_uuid ~__context ~self)
		(Db.GPU_group.get_uuid ~__context ~self:value);
	Mutex.execute pgpu_m (fun () ->
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
			let old_group = Db.PGPU.get_GPU_group ~__context ~self in
			Db.PGPU.set_GPU_group ~__context ~self ~value;
			(* Group inherits the device type *)
			Db.GPU_group.set_GPU_types ~__context ~self:value ~value:new_types;
			debug "PGPU %s moved to GPU group %s. Group GPU types = [ %s ]."
				(Db.PGPU.get_uuid ~__context ~self)
				(Db.GPU_group.get_uuid ~__context ~self:value)
				(String.concat "; " new_types);
			(* Update the old and new groups' cached lists of VGPU_types. *)
			if Db.is_valid_ref __context old_group
			then begin
				Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:old_group;
				Xapi_gpu_group.update_supported_VGPU_types ~__context ~self:old_group;
			end;
			Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:value;
			Xapi_gpu_group.update_supported_VGPU_types ~__context ~self:value
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
