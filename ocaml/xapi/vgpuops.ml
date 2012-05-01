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
}

let vgpu_of_vgpu ~__context vm_r vgpu =
	let vgpu_r = Db.VGPU.get_record ~__context ~self:vgpu in
	{
		vgpu_ref = vgpu;
		gpu_group_ref = vgpu_r.API.vGPU_GPU_group;
		devid = int_of_string vgpu_r.API.vGPU_device;
		other_config = vgpu_r.API.vGPU_other_config
	}

let vgpus_of_vm ~__context vm_r =
	List.map (vgpu_of_vgpu ~__context vm_r) vm_r.API.vM_VGPUs

let create_vgpu ~__context ~vm vgpu available_pgpus pcis =
	debug "Create vGPUs";
	let compatible_pgpus = Db.GPU_group.get_PGPUs ~__context ~self:vgpu.gpu_group_ref in
	let pgpus = List.intersect compatible_pgpus available_pgpus in
	let free_pgpus = List.filter_map (fun pgpu ->
		let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
		if Pciops.get_free_functions ~__context pci <= 0
		then None
		else Some (pgpu, pci)
	) pgpus in
	match free_pgpus with
	| [] ->
		raise (Api_errors.Server_error (Api_errors.vm_requires_gpu, [
			Ref.string_of vm;
			Ref.string_of vgpu.gpu_group_ref
		]))
	| (pgpu, pci) :: _ ->
		List.filter (fun g -> g <> pgpu) available_pgpus,
		pci :: pcis

let create_vgpus ~__context (vm, vm_r) hvm =
	let vgpus = vgpus_of_vm ~__context vm_r in
	let pcis =
		if vgpus <> [] then begin
			if not hvm then
				raise (Api_errors.Server_error (Api_errors.feature_requires_hvm, ["GPU passthrough needs HVM"]));
			let host = Helpers.get_localhost ~__context in
			let pgpus = Db.Host.get_PGPUs ~__context ~self:host in
			let _, pcis =
				List.fold_left (fun (pgpus, pcis) vgpu -> create_vgpu ~__context ~vm vgpu pgpus pcis)
					(pgpus, []) vgpus
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

	(try Db.VM.remove_from_other_config ~__context ~self:vm ~key:Xapi_globs.vgpu_pci with _ -> ());
	let value = String.concat "," (List.map Pciops.to_string devs) in
	Db.VM.add_to_other_config ~__context ~self:vm ~key:Xapi_globs.vgpu_pci ~value

let list_vgpus ~__context ~vm =
	try
		let value = List.assoc Xapi_globs.vgpu_pci (Db.VM.get_other_config ~__context ~self:vm) in
		List.map Pciops.of_string (String.split ',' value)
	with _ -> []

let clear_vgpus ~__context ~vm =
	let vgpus = Db.VM.get_VGPUs ~__context ~self:vm in
	List.iter (fun self -> Db.VGPU.set_currently_attached ~__context ~self ~value:false) vgpus

