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

type vgpu = {
	domid: int; (** current domain id of the VM *)
	vgpu_ref: API.ref_VGPU;
	gpu_group_ref: API.ref_GPU_group;
	devid: int;
	other_config: (string * string) list;
}

let vgpu_of_vgpu ~__context vm_r domid vgpu =
	let vgpu_r = Db.VGPU.get_record ~__context ~self:vgpu in
	{
		domid = domid;
		vgpu_ref = vgpu;
		gpu_group_ref = vgpu_r.API.vGPU_GPU_group;
		devid = int_of_string vgpu_r.API.vGPU_device;
		other_config = vgpu_r.API.vGPU_other_config
	}

let vgpus_of_vm ~__context ~vm domid =
	let vm_r = Db.VM.get_record ~__context ~self:vm in
	List.map (vgpu_of_vgpu ~__context vm_r domid) vm_r.API.vM_VGPUs

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

let create_vgpus ~__context ~vm domid hvm =
	let vgpus = vgpus_of_vm ~__context ~vm domid in
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
		[]

let clear_vgpus ~__context ~vm =
	let vgpus = Db.VM.get_VGPUs ~__context ~self:vm in
	List.iter (fun self -> Db.VGPU.set_currently_attached ~__context ~self ~value:false) vgpus

