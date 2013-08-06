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

module D = Debug.Debugger(struct let name = "xapi" end)
open D

open Stringext

exception Parse_error of exn

let nvidia_conf_dir = "/usr/share/nvidia/vgx"
let nvidia_vendor_id = 0x10deL

type vgpu_conf = {
	pdev_id : int64;
	vdev_id : int64;
	vsubdev_id : int64;
	framebufferlength : int64;
}

type vgpu_type = {
	vendor_name : string;
	model_name : string;
	framebuffer_size : int64;
}

let entire_gpu = {vendor_name = ""; model_name = "passthrough"; framebuffer_size = 0L}

let create ~__context ~vendor_name ~model_name ~framebuffer_size =
	let ref = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.VGPU_type.create ~__context ~ref ~uuid ~vendor_name ~model_name ~framebuffer_size;
	debug "VGPU_type ref='%s' created (vendor_name = '%s'; model_name = '%s')"
		(Ref.string_of ref) vendor_name model_name;
	ref

let find_or_create ~__context vgpu_type =
	let open Db_filter_types in
	let existing_types =
		Db.VGPU_type.get_records_where ~__context
			~expr:(And
				(Eq (Field "vendor_name", Literal vgpu_type.vendor_name),
				 And(
				  Eq (Field "model_name", Literal vgpu_type.model_name),
				  Eq (Field "framebuffer_size",
				      Literal (Int64.to_string vgpu_type.framebuffer_size)))))
	in
	match existing_types with
	| [vgpu_type, rc] ->
		vgpu_type
	| [] ->
		create ~__context ~vendor_name:vgpu_type.vendor_name
			~model_name:vgpu_type.model_name
			~framebuffer_size:vgpu_type.framebuffer_size
	| _ ->
		failwith "Error: Multiple vGPU types exist with the same configuration."

let of_conf_file file_path =
	try
		let conf = Unixext.read_lines file_path in
		let args = List.filter
			(fun s -> not (String.startswith "#" s || s = "")) conf in
		(* Expeciting space separated key value entries *)
		let args = List.map
			(fun s ->
				match (String.split ' ' s ~limit:2) with
				| k :: [v] -> (k, v)
				| _ -> ("", "")
			) args in
		Scanf.sscanf (List.assoc "plugin0.pdev_id" args) "\"0x%Lx\"" (fun pdev_id ->
			(* NVIDIA key is "device_id:subdevice_id", N.B. not subvendor id *)
			Scanf.sscanf (List.assoc "plugin0.vdev_id" args) "\"0x%Lx:0x%Lx\""
				(fun vdev_id vsubdev_id ->
					let framebufferlength = Int64.of_string
						(List.assoc "plugin0.framebufferlength" args) in
					{pdev_id; vdev_id; vsubdev_id; framebufferlength}))
	with e ->
		raise (Parse_error e)

let read_config_dir conf_dir =
	let rec read_configs ac = function
		| [] -> ac
		| conf_file::tl ->
			try
				read_configs (of_conf_file conf_file :: ac) tl
			with Parse_error e ->
				error "Ignoring error parsing %s: %s\n%s\n" conf_file
					(Printexc.to_string e) (Printexc.get_backtrace ());
				read_configs ac tl
	in
	let conf_files = Array.to_list (Sys.readdir conf_dir) in
	debug "Reading NVIDIA vGPU config files %s/{%s}"
		conf_dir (String.concat ", " conf_files);
	read_configs []
		(List.map (fun conf -> String.concat "/" [conf_dir; conf]) conf_files)

let relevant_vgpu_types pci_dev_ids =
	let vgpu_confs = try read_config_dir nvidia_conf_dir with _ -> [] in
	let relevant_vgpu_confs =
		List.filter
			(fun c -> List.mem c.pdev_id pci_dev_ids)
			vgpu_confs
	in
	debug "Relevant confs = [ %s ]" (String.concat "; " (List.map (fun c ->
		Printf.sprintf "{pdev_id:%04Lx; vdev_id:%04Lx; vsubdev_id:%04Lx; framebufferlength:0x%Lx}"
		c.pdev_id c.vdev_id c.vsubdev_id c.framebufferlength) relevant_vgpu_confs));
	let rec build_vgpu_types pci_db ac = function
		| [] -> ac
		| conf::tl ->
			debug "Pci_db lookup: get_sub_device_names vendor=%04Lx device=%04Lx subdev=%04Lx"
				nvidia_vendor_id conf.vdev_id conf.vsubdev_id;
			let vendor_name = Pci_db.get_vendor_name pci_db nvidia_vendor_id in
			let model_name = List.hd
				(Pci_db.get_subdevice_names_by_id pci_db nvidia_vendor_id
					conf.vdev_id conf.vsubdev_id)
			and framebuffer_size = conf.framebufferlength in
			let vgpu_type = {vendor_name; model_name; framebuffer_size} in
			build_vgpu_types pci_db (vgpu_type :: ac) tl
	in
	let pci_db = Pci_db.of_file Pci_db.pci_ids_path in
	build_vgpu_types pci_db [] relevant_vgpu_confs

let find_or_create_supported_types ~__context pci =
	(* let pci_recs = List.map (fun self -> Db.PCI.get_record_internal ~__context ~self) pcis in *)
	(* let dev_ids = List.map (fun p -> p.Db_actions.pCI_device_id) pci_recs in *)
	let dev_id = Xapi_pci.int_of_id (Db.PCI.get_device_id ~__context ~self:pci) in
	debug "dev_ids = [ %s ]" (Printf.sprintf "%04Lx" dev_id);
	let relevant_types = relevant_vgpu_types [dev_id] in
	debug "Relevant vGPU configurations for pgpu = [ %s ]"
		(String.concat "; "
			(List.map (fun vt -> vt.model_name) relevant_types));
	let vgpu_types = List.map
		(fun v -> find_or_create ~__context v) relevant_types in
	let entire_gpu_type = find_or_create ~__context entire_gpu in
	entire_gpu_type :: vgpu_types
