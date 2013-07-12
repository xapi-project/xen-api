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
module D=Debug.Make(struct let name="xapi" end)
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
	let pci_id_blacklist =
		match Xapi_pci.get_system_display_device () with
		| Some device -> [device]
		| None -> []
	in
	let existing_pgpus = List.filter (fun (rf, rc) -> rc.API.pGPU_host = host) (Db.PGPU.get_all_records ~__context) in
	let class_id = Xapi_pci.find_class_id Xapi_pci.Display_controller in
	let pcis = List.filter (fun self ->
		Db.PCI.get_class_id ~__context ~self = class_id &&
		Db.PCI.get_host ~__context ~self = host) (Db.PCI.get_all ~__context)
	in
	let rec find_or_create cur = function
		| [] -> cur
		| pci :: remaining_pcis ->
			let pci_id = Db.PCI.get_pci_id ~__context ~self:pci in
			if List.mem pci_id pci_id_blacklist
			then find_or_create cur remaining_pcis
			else begin
				let pgpu =
					try
						List.find (fun (rf, rc) -> rc.API.pGPU_PCI = pci) existing_pgpus
					with Not_found ->
						let self = create ~__context ~pCI:pci ~gPU_group:(Ref.null) ~host ~other_config:[] in
						let group = Xapi_gpu_group.find_or_create ~__context self in
						Db.PGPU.set_GPU_group ~__context ~self ~value:group;
						self, Db.PGPU.get_record ~__context ~self
				in
				find_or_create (pgpu :: cur) remaining_pcis
			end
	in
	let current_pgpus = find_or_create [] pcis in
	let obsolete_pgpus = List.set_difference existing_pgpus current_pgpus in
	List.iter (fun (self, _) -> Db.PGPU.destroy ~__context ~self) obsolete_pgpus

let gpu_group_m = Mutex.create ()
let set_GPU_group ~__context ~self ~value =
	Mutex.execute gpu_group_m (fun () ->
		let pci = Db.PGPU.get_PCI ~__context ~self in

		(* Precondition: PGPU not currently in use by a VM *)
		if Db.PCI.get_attached_VMs ~__context ~self:pci <> []
		then failwith "PGPU currently in use";

		(* Precondition: Moving PGPU from current group can't orphan VGPU *)
		let src_g = Db.PGPU.get_GPU_group ~__context ~self in
		let pgpu_is_singleton =
			(List.length (Db.GPU_group.get_PGPUs ~__context ~self:src_g) = 1)
		and pgpu_has_vgpus =
			((Db.GPU_group.get_VGPUs ~__context ~self:src_g) <> []) in
		if (pgpu_is_singleton && pgpu_has_vgpus)
		then failwith "Moving this PGPU would leave VGPUs without a PGPU";

		let check_compatibility gpu_type group_types =
			match group_types with
			| [] -> true, [gpu_type]
			| _ -> List.mem gpu_type group_types, group_types in

		let gpu_type = Xapi_pci.get_device_id ~__context ~self:pci
		and group_types = Db.GPU_group.get_GPU_types ~__context ~self:value in
		match check_compatibility gpu_type group_types with
		| true, new_types ->
			Db.PGPU.set_GPU_group ~__context ~self ~value;
			(* Group inherits the device type *)
			Db.GPU_group.set_GPU_types ~__context ~self:value ~value:new_types
		| false, _ ->
			failwith "PGPU type not compatible with destination group"
	)
