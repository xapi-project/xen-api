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

let create ~__context ~name_label ~name_description ~other_config =
	let group = Ref.make () in
	let uuid = Uuid.to_string (Uuid.insecure ()) in
	Db.GPU_group.create ~__context ~ref:group ~uuid ~name_label ~name_description ~gPU_types:[] ~other_config;
	group

let destroy ~__context ~self =
	let vgpus = Db.GPU_group.get_VGPUs ~__context ~self in
	let connected = List.filter (fun self ->
		Db.VGPU.get_currently_attached ~__context ~self
	) vgpus in
	if connected <> [] then
		raise (Api_errors.Server_error (Api_errors.gpu_group_contains_vgpu, List.map Ref.string_of connected));

	let pgpus = Db.GPU_group.get_PGPUs ~__context ~self in
	if pgpus <> [] then
		raise (Api_errors.Server_error (Api_errors.gpu_group_contains_pgpu, List.map Ref.string_of pgpus));

	(* Destroy all vGPUs *)
	List.iter (fun vgpu ->
		Helpers.log_exn_continue (Printf.sprintf "destroying VGPU: %s" (Ref.string_of vgpu))
		(fun vgpu -> Db.VGPU.destroy ~__context ~self:vgpu) vgpu) vgpus;

	Db.GPU_group.destroy ~__context ~self

let find_or_create ~__context pgpu =
	let pci = Db.PCI.get_record_internal ~__context ~self:(Db.PGPU.get_PCI ~__context ~self:pgpu) in
	let gpu_type = pci.Db_actions.pCI_vendor_id ^ "/" ^ pci.Db_actions.pCI_device_id in
	try
		List.find (fun rf->
			let rc = Db.GPU_group.get_record_internal ~__context ~self:rf in
			rc.Db_actions.gPU_group_GPU_types = [gpu_type]
		)
		(Db.GPU_group.get_all ~__context)
	with Not_found ->
		let name_label = "Group of " ^ pci.Db_actions.pCI_vendor_name ^ " " ^ pci.Db_actions.pCI_device_name ^ " GPUs" in
		let group = create ~__context ~name_label ~name_description:"" ~other_config:[] in
		Db.GPU_group.set_GPU_types ~__context ~self:group ~value:[gpu_type];
		group

