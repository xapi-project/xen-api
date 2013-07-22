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

module D = Debug.Make(struct let name = "xapi" end)
open D

open Stringext

exception Parse_error of exn

let nvidia_conf_dir = "/usr/share/nvidia/vgx"
let nvidia_vendor_id = "10ed"

type vgpu_conf = {
	pdev_id : string;
	vdev_id : string;
	framebufferlength : int64;
}

type vgpu_type = {
	model_name : string;
	framebuffer_size : int64;
}

let entire_gpu = {model_name = "whole"; framebuffer_size = 0L}

let create ~__context ~model_name ~framebuffer_size =
	let ref = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.VGPU_type.create ~__context ~ref ~uuid ~model_name ~framebuffer_size;
	ref

let find_or_create ~__context vgpu_type =
	let open Db_filter_types in
	let existing_types =
		Db.VGPU_type.get_records_where ~__context
			~expr:(And
				(Eq (Field "model_name", Literal vgpu_type.model_name),
				 Eq (Field "framebuffer_size",
					 Literal (Int64.to_string vgpu_type.framebuffer_size))))
	in
	match existing_types with
	| (vgpu_type, _) :: _ -> vgpu_type
	| [] ->
		create ~__context ~model_name:vgpu_type.model_name
			~framebuffer_size:vgpu_type.framebuffer_size

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
		let pdev_id = List.assoc "plugin0.pdev_id" args in
		let vdev_id = List.assoc "plugin0.vdev_id" args in
		let framebufferlength = Int64.of_string
			(List.assoc "plugin0.framebufferlength" args) in
		{pdev_id; vdev_id; framebufferlength}
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
	read_configs []
		(List.map (fun conf -> String.concat "/" [conf_dir; conf]) conf_files)

let relevant_vgpu_types pci_dev_ids =
	let vgpu_confs = read_config_dir nvidia_conf_dir in
	let relevant_vgpu_confs =
		List.filter
			(fun c -> List.mem c.pdev_id pci_dev_ids)
			vgpu_confs
	in
	let rec build_vgpu_types pci_db ac = function
		| [] -> ac
		| conf::tl ->
			(* NVIDIA key is "device_id:subdevice_id", N.B. not subvendor id *)
			let key = String.filter_chars conf.vdev_id ((<>) '"') in
			begin match String.split ':' key ~limit:2 with
			| [dev_id; subdev_id] ->
				let model_name = List.hd
					(Pci_db.get_subdevice_names_by_id pci_db nvidia_vendor_id
						dev_id subdev_id)
				and framebuffer_size = conf.framebufferlength in
				let vgpu_type = {model_name; framebuffer_size} in
				build_vgpu_types pci_db (vgpu_type :: ac) tl
			| _ -> failwith "Malformed vdev_id in NVIDIA config"
			end
	in
	let pci_db = Pci_db.of_file Pci_db.pci_ids_path in
	build_vgpu_types pci_db [] relevant_vgpu_confs

let find_or_create_supported_types ~__context gpu_group =
	let gpu_types = Db.GPU_group.get_GPU_types ~__context ~self:gpu_group in
	let gpu_dev_ids = List.map (fun s -> String.sub s 0 4) gpu_types in
	let relevant_vgpu_types = relevant_vgpu_types gpu_dev_ids in
	let vgpu_types = List.map
		(fun v -> find_or_create ~__context v) relevant_vgpu_types in
	let entire_gpu_type = find_or_create ~__context entire_gpu in
	entire_gpu_type :: vgpu_types
	(* TODO: Garbage collect types not in use by any group *)
