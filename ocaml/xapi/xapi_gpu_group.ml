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
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.GPU_group.create ~__context ~ref:group ~uuid ~name_label ~name_description
		~gPU_types:[] ~other_config ~allocation_algorithm:`depth_first;
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
	let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
	let pci_rec = Db.PCI.get_record_internal ~__context ~self:pci in
	let gpu_type = Xapi_pci.string_of_pci ~__context ~self:pci in
	try
		List.find (fun rf->
			let rc = Db.GPU_group.get_record_internal ~__context ~self:rf in
			rc.Db_actions.gPU_group_GPU_types = [gpu_type]
		)
		(Db.GPU_group.get_all ~__context)
	with Not_found ->
		let name_label = "Group of " ^ pci_rec.Db_actions.pCI_vendor_name ^ " " ^ pci_rec.Db_actions.pCI_device_name ^ " GPUs" in
		let group = create ~__context ~name_label ~name_description:"" ~other_config:[] in
		group

module VGPU_type_set = Set.Make(struct type t = API.ref_VGPU_type let compare = compare end)
let union_type_lists ~type_lists =
	(* Fold each item of each list into a set,
	 * then return the elements of the set as a list. *)
	let union_set = List.fold_left
		(fun acc type_list ->
			List.fold_left
				(fun acc vgpu_type -> VGPU_type_set.add vgpu_type acc)
				acc type_list)
		VGPU_type_set.empty type_lists
	in
	VGPU_type_set.elements union_set

let get_enabled_VGPU_types ~__context ~self =
	let pgpus = Db.GPU_group.get_PGPUs ~__context ~self in
	union_type_lists
		(List.map
			(fun pgpu -> Db.PGPU.get_enabled_VGPU_types ~__context ~self:pgpu)
			pgpus)

let get_supported_VGPU_types ~__context ~self =
	let pgpus = Db.GPU_group.get_PGPUs ~__context ~self in
	union_type_lists
		(List.map
			(fun pgpu -> Db.PGPU.get_supported_VGPU_types ~__context ~self:pgpu)
			pgpus)
