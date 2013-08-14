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
module D=Debug.Debugger(struct let name="vgpuops" end)
open D

open Listext
open Stringext

type vgpu = {
	vgpu_ref: API.ref_VGPU;
	gpu_group_ref: API.ref_GPU_group;
	devid: int;
	other_config: (string * string) list;
	type_ref: API.ref_VGPU_type;
}

let vgpu_of_vgpu ~__context vm_r vgpu =
	let vgpu_r = Db.VGPU.get_record ~__context ~self:vgpu in
	{
		vgpu_ref = vgpu;
		gpu_group_ref = vgpu_r.API.vGPU_GPU_group;
		devid = int_of_string vgpu_r.API.vGPU_device;
		other_config = vgpu_r.API.vGPU_other_config;
		type_ref = vgpu_r.API.vGPU_type;
	}

let vgpus_of_vm ~__context vm_r =
	List.map (vgpu_of_vgpu ~__context vm_r) vm_r.API.vM_VGPUs

let create_passthrough_vgpu ~__context ~vm vgpu available_pgpus pcis =
	debug "Create vGPUs";
	let compatible_pgpus = Db.GPU_group.get_PGPUs ~__context ~self:vgpu.gpu_group_ref in
	let pgpus = List.intersect compatible_pgpus available_pgpus in
	let rec reserve_one = function
		| [] -> None
		| pgpu :: remaining ->
			let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
			if Pciops.reserve ~__context pci then
				Some (pgpu, pci)
			else
				reserve_one remaining
	in
	match reserve_one pgpus with
	| None ->
		raise (Api_errors.Server_error (Api_errors.vm_requires_gpu, [
			Ref.string_of vm;
			Ref.string_of vgpu.gpu_group_ref
		]))
	| Some (pgpu, pci) ->
		List.filter (fun g -> g <> pgpu) available_pgpus,
		pci :: pcis

let add_pcis_to_vm ~__context vm passthru_vgpus =
	let pcis =
		if passthru_vgpus <> [] then begin
			let host = Helpers.get_localhost ~__context in
			let pgpus = Db.Host.get_PGPUs ~__context ~self:host in
			let _, pcis =
				List.fold_left
					(fun (pgpus, pcis) passthru_vgpu ->
						create_passthrough_vgpu ~__context ~vm passthru_vgpu pgpus pcis)
					(pgpus, []) passthru_vgpus
			in
			pcis
		end else
			[] in
	(* The GPU PCI devices which xapi manages may have dependencies: *)
	let dependent_pcis = List.setify (List.flatten
		(List.map (fun pci -> Db.PCI.get_dependencies ~__context ~self:pci) pcis)) in
	let devs : (int * int * int * int) list = List.sort compare (List.map (Pciops.pcidev_of_pci ~__context) (pcis @ dependent_pcis)) in
	(* Add a hotplug ordering (see pcidevs_of_pci) *)
	let devs : ((int * (int * int * int * int))) list = List.rev (snd (List.fold_left (fun (i, acc) pci -> i + 1, (i, pci) :: acc) (0, []) devs)) in
	(* Update VM other_config for PCI passthrough *)
	(try Db.VM.remove_from_other_config ~__context ~self:vm ~key:Xapi_globs.vgpu_pci with _ -> ());
	let value = String.concat "," (List.map Pciops.to_string devs) in
	Db.VM.add_to_other_config ~__context ~self:vm ~key:Xapi_globs.vgpu_pci ~value

let m = Mutex.create ()
let create_virtual_vgpu ~__context vm vgpu =
	let host = Helpers.get_localhost ~__context in
	let available_pgpus = Db.Host.get_PGPUs ~__context ~self:host in
	let compatible_pgpus = Db.GPU_group.get_PGPUs ~__context ~self:vgpu.gpu_group_ref in
	let pgpus = List.intersect compatible_pgpus available_pgpus in
	let rec allocate_vgpu vgpu_type = function
		| [] -> None
		| pgpu :: remaining_pgpus ->
			let open Xapi_pgpu_helpers in
			try
				assert_VGPU_type_allowed ~__context ~self:pgpu ~vgpu_type;
				assert_capacity_exists_for_VGPU ~__context ~self:pgpu ~vgpu:vgpu.vgpu_ref;
				Some pgpu
			with _ -> allocate_vgpu vgpu_type remaining_pgpus
	in
	Threadext.Mutex.execute m (fun () ->
		match allocate_vgpu vgpu.type_ref pgpus with
		| None ->
			raise (Api_errors.Server_error (Api_errors.vm_requires_vgpu, [
				Ref.string_of vm;
				Ref.string_of vgpu.gpu_group_ref;
				Ref.string_of vgpu.type_ref
			]))
		| Some pgpu ->
			Db.VGPU.set_resident_on ~__context ~self:vgpu.vgpu_ref ~value:pgpu;
			Db.PGPU.get_PCI ~__context ~self:pgpu
	)

let add_vgpus_to_vm ~__context vm vgpus =
	(* Update VM platform for xenops to use *)
	List.iter
		(fun key ->
			try Db.VM.remove_from_platform ~__context ~self:vm ~key with _ -> ())
		[Xapi_globs.vgpu_vga_key; Xapi_globs.vgpu_pci_key; Xapi_globs.vgpu_config_key];
	(* Only support a maximum of one virtual GPU per VM for now. *)
	match vgpus with
	| [] -> ()
	| vgpu :: _ ->
		let vgpu_type = Db.VGPU_type.get_record_internal ~__context ~self:vgpu.type_ref in
		let internal_config = vgpu_type.Db_actions.vGPU_type_internal_config in
		let config_path = List.assoc Xapi_globs.vgpu_config_key internal_config in
		let vgpu_pci = create_virtual_vgpu ~__context vm vgpu in
		let pci_id = Db.PCI.get_pci_id ~__context ~self:vgpu_pci in
		Db.VM.add_to_platform ~__context ~self:vm ~key:Xapi_globs.vgpu_vga_key ~value:Xapi_globs.vgpu_vga_value;
		Db.VM.add_to_platform ~__context ~self:vm ~key:Xapi_globs.vgpu_config_key ~value:config_path;
		Db.VM.add_to_platform ~__context ~self:vm ~key:Xapi_globs.vgpu_pci_key ~value:pci_id

let create_vgpus ~__context (vm, vm_r) hvm =
	let vgpus = vgpus_of_vm ~__context vm_r in
	if vgpus <> [] then begin
		if not hvm then
			raise (Api_errors.Server_error (Api_errors.feature_requires_hvm, ["vGPU- and GPU-passthrough needs HVM"]))
	end;
	let (passthru_vgpus, virtual_vgpus) =
		List.partition
			(fun v -> Xapi_vgpu.requires_passthrough ~__context ~self:v.vgpu_ref)
			vgpus
	in
	add_pcis_to_vm ~__context vm passthru_vgpus;
	add_vgpus_to_vm ~__context vm virtual_vgpus

let list_pcis_for_passthrough ~__context ~vm =
	try
		let value = List.assoc Xapi_globs.vgpu_pci (Db.VM.get_other_config ~__context ~self:vm) in
		List.map Pciops.of_string (String.split ',' value)
	with _ -> []
