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

open Xstringext

exception Parse_error of exn

type vgpu_type = {
	vendor_name : string;
	model_name : string;
	framebuffer_size : int64;
	max_heads : int64;
	max_resolution_x : int64;
	max_resolution_y : int64;
	size : int64;
	internal_config : (string * string) list;
	implementation : API.vgpu_type_implementation;
}

let passthrough_gpu = {
	vendor_name = "";
	model_name = "passthrough";
	framebuffer_size = 0L;
	max_heads = 0L;
	max_resolution_x = 0L;
	max_resolution_y = 0L;
	size = 0L;
	internal_config = [];
	implementation = `passthrough;
}

let create ~__context ~vendor_name ~model_name ~framebuffer_size ~max_heads
		~max_resolution_x ~max_resolution_y ~size ~internal_config ~implementation =
	let ref = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.VGPU_type.create ~__context ~ref ~uuid ~vendor_name ~model_name
		~framebuffer_size ~max_heads ~max_resolution_x ~max_resolution_y
		~size ~internal_config ~implementation;
	debug "VGPU_type ref='%s' created (vendor_name = '%s'; model_name = '%s')"
		(Ref.string_of ref) vendor_name model_name;
	ref

let find_or_create ~__context vgpu_type =
	let open Db_filter_types in
	let existing_types =
		Db.VGPU_type.get_internal_records_where ~__context
			~expr:(And
				(Eq (Field "vendor_name", Literal vgpu_type.vendor_name),
				(Eq (Field "model_name", Literal vgpu_type.model_name))))
	in
	match existing_types with
	| [vgpu_type_ref, rc] ->
		(* Update anything about the VGPU type which might have changed since we
		 * last read the config file. *)
		if vgpu_type.framebuffer_size <> rc.Db_actions.vGPU_type_framebuffer_size then
			Db.VGPU_type.set_framebuffer_size ~__context
				~self:vgpu_type_ref
				~value:vgpu_type.framebuffer_size;
		if vgpu_type.max_heads <> rc.Db_actions.vGPU_type_max_heads then
			Db.VGPU_type.set_max_heads ~__context
				~self:vgpu_type_ref
				~value:vgpu_type.max_heads;
		if vgpu_type.max_resolution_x <> rc.Db_actions.vGPU_type_max_resolution_x then
			Db.VGPU_type.set_max_resolution_x ~__context
				~self:vgpu_type_ref
				~value:vgpu_type.max_resolution_x;
		if vgpu_type.max_resolution_y <> rc.Db_actions.vGPU_type_max_resolution_x then
			Db.VGPU_type.set_max_resolution_y ~__context
				~self:vgpu_type_ref
				~value:vgpu_type.max_resolution_y;
		if vgpu_type.size <> rc.Db_actions.vGPU_type_size then
			Db.VGPU_type.set_size ~__context
				~self:vgpu_type_ref
				~value:vgpu_type.size;
		if vgpu_type.internal_config <> rc.Db_actions.vGPU_type_internal_config then
			Db.VGPU_type.set_internal_config ~__context
				~self:vgpu_type_ref
				~value:vgpu_type.internal_config;
		if vgpu_type.implementation <> rc.Db_actions.vGPU_type_implementation then
			Db.VGPU_type.set_implementation ~__context
				~self:vgpu_type_ref
				~value:vgpu_type.implementation;
		vgpu_type_ref
	| [] ->
		create ~__context ~vendor_name:vgpu_type.vendor_name
			~model_name:vgpu_type.model_name
			~framebuffer_size:vgpu_type.framebuffer_size
			~max_heads:vgpu_type.max_heads
			~max_resolution_x:vgpu_type.max_resolution_x
			~max_resolution_y:vgpu_type.max_resolution_y
			~size:vgpu_type.size
			~internal_config:vgpu_type.internal_config
			~implementation:vgpu_type.implementation
	| _ ->
		failwith "Error: Multiple vGPU types exist with the same configuration."

module Nvidia = struct
	let nvidia_conf_dir = "/usr/share/nvidia/vgx"
	let nvidia_vendor_id = 0x10de

	type vgpu_conf = {
		pdev_id : int;
		psubdev_id : int option;
		vdev_id : int;
		vsubdev_id : int;
		framebufferlength : int64;
		num_heads : int64;
		max_instance : int64;
		max_x : int64;
		max_y : int64;
		file_path : string;
	}

	let of_conf_file file_path =
		try
			let conf = Unixext.read_lines file_path in
			let args = List.filter
				(fun s -> not (String.startswith "#" s || s = "")) conf in
			let args = List.map (String.strip String.isspace) args in
			(* Expecting space separated key value entries *)
			let args = List.map
				(fun s ->
					match (String.split ' ' s ~limit:2) with
					| k :: [v] -> (k, v)
					| _ -> ("", "")
				) args in
			(* plugin0.pdev_id will either be just the physical device id, or of the
			 * form "device_id:subdevice_id" *)
			let pdev_id, psubdev_id =
				let pdev_id_data = (List.assoc "plugin0.pdev_id" args) in
				try
					Scanf.sscanf pdev_id_data "\"0x%x:0x%x\""
						(fun pdev_id psubdev_id -> pdev_id, Some psubdev_id)
				with Scanf.Scan_failure _ ->
					Scanf.sscanf pdev_id_data "\"0x%x\""
						(fun pdev_id -> pdev_id, None)
			in
			(* NVIDIA key is "device_id:subdevice_id", N.B. not subvendor id *)
			Scanf.sscanf (List.assoc "plugin0.vdev_id" args) "\"0x%x:0x%x\"" (fun vdev_id vsubdev_id ->
				Scanf.sscanf (List.assoc "plugin0.max_resolution" args) "%Ldx%Ld" (fun max_x max_y ->
					let framebufferlength = Int64.of_string
						(List.assoc "plugin0.framebufferlength" args) in
					let num_heads = Int64.of_string
						(List.assoc "plugin0.num_heads" args) in
					let max_instance = Int64.of_string
						(List.assoc "plugin0.max_instance" args) in
					{pdev_id; psubdev_id; vdev_id; vsubdev_id; framebufferlength;
							 num_heads; max_instance; max_x; max_y; file_path}
					)
				)
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

	let relevant_vgpu_types pci_dev_id subsystem_device_id =
		let vgpu_confs = try read_config_dir nvidia_conf_dir with _ -> [] in
		let relevant_vgpu_confs =
			List.filter
				(fun c ->
					let device_id_matches = (c.pdev_id = pci_dev_id) in
					let subsystem_device_id_matches =
						(* If the config file doesn't specify a physical subdevice ID, then
						 * the config file is valid for this device no matter the device's
						 * subsystem device ID.
						 *
						 * If the config file does specify a physical subdevice ID, then the
						 * corresponding ID of the card must match. *)
						match subsystem_device_id, c.psubdev_id with
						| _, None -> true
						| None, Some _ -> false
						| Some device_id, Some conf_id -> device_id = conf_id
					in
					device_id_matches && subsystem_device_id_matches)
				vgpu_confs
		in
		debug "Relevant confs = [ %s ]"
			(String.concat "; " (List.map (fun c ->
				Printf.sprintf
					"{pdev_id:%04x; psubdev_id:%s; vdev_id:%04x; vsubdev_id:%04x; framebufferlength:0x%Lx}"
					c.pdev_id
					(match c.psubdev_id with
						| None -> "Any"
						| Some id -> Printf.sprintf "%04x" id)
					c.vdev_id
					c.vsubdev_id
					c.framebufferlength)
				relevant_vgpu_confs));
		let rec build_vgpu_types pci_access ac = function
			| [] -> ac
			| conf::tl ->
				debug "Pci.lookup_subsystem_device_name: vendor=%04x device=%04x subdev=%04x"
					nvidia_vendor_id conf.vdev_id conf.vsubdev_id;
				let vendor_name = Pci.lookup_vendor_name pci_access nvidia_vendor_id
				and model_name =
					Pci.lookup_subsystem_device_name pci_access nvidia_vendor_id
						conf.vdev_id nvidia_vendor_id conf.vsubdev_id
				and framebuffer_size = conf.framebufferlength
				and max_heads = conf.num_heads
				and max_resolution_x = conf.max_x
				and max_resolution_y = conf.max_y
				and size = Int64.div Constants.pgpu_default_size conf.max_instance
				and internal_config = [Xapi_globs.vgpu_config_key, conf.file_path]
				and implementation = `nvidia in
				let vgpu_type = {
					vendor_name; model_name; framebuffer_size; max_heads;
					max_resolution_x; max_resolution_y; size; internal_config;
					implementation}
				in
				build_vgpu_types pci_access (vgpu_type :: ac) tl
		in
		Pci.with_access (fun a -> build_vgpu_types a [] relevant_vgpu_confs)

	let find_or_create_supported_types ~__context ~pci =
		let dev_id = Xapi_pci.int_of_id (Db.PCI.get_device_id ~__context ~self:pci) in
		let subsystem_dev_id =
			match Db.PCI.get_subsystem_device_id ~__context ~self:pci with
			| "" -> None
			| id_string -> Some (Xapi_pci.int_of_id id_string)
		in
		debug "dev_id = %s" (Printf.sprintf "%04x" dev_id);
		let relevant_types = relevant_vgpu_types dev_id subsystem_dev_id in
		debug "Relevant vGPU configurations for pgpu = [ %s ]"
			(String.concat "; "
				(List.map (fun vt -> vt.model_name) relevant_types));
		let vgpu_types = List.map
			(fun v -> find_or_create ~__context v) relevant_types in
		let passthrough_gpu_type = find_or_create ~__context passthrough_gpu in
		passthrough_gpu_type :: vgpu_types
end

module Intel = struct
	let intel_vendor_id = 0x8086

	let ( *** ) = Int64.mul
	let ( /// ) = Int64.div
	let ( +++ ) = Int64.add
	let ( --- ) = Int64.sub
	let mib x = List.fold_left Int64.mul x [1024L; 1024L]

	let make_gvt_g_256 ~__context ~pci =
		let open Xenops_interface.Pci in
		let address =
			Db.PCI.get_pci_id ~__context ~self:pci
			|> address_of_string
		in
		let device, device_name =
			Pci.(with_access (fun access ->
				let device =
					List.find
						(fun device ->
							(device.Pci_dev.domain = address.domain) &&
							(device.Pci_dev.bus = address.bus) &&
							(device.Pci_dev.dev = address.dev) &&
							(device.Pci_dev.func = address.fn))
						(get_devices access)
				in
				let device_name =
					lookup_device_name access
						device.Pci_dev.vendor_id
						device.Pci_dev.device_id
				in
				device, device_name))
		in
		let bar_size =
			List.nth device.Pci.Pci_dev.size 2
			|> Int64.of_nativeint
		in
		let vgpus_per_pgpu = bar_size /// 1024L /// 1024L /// 128L --- 1L in
		let vgpu_size = Constants.pgpu_default_size /// vgpus_per_pgpu in
		{
			vendor_name = "Intel";
			model_name = "Intel GVT-g on " ^ device_name;
			framebuffer_size = mib 256L;
			max_heads = 1L;
			max_resolution_x = 2560L;
			max_resolution_y = 1600L;
			size = vgpu_size;
			internal_config = [
				Xapi_globs.vgt_low_gm_sz, Int64.to_string 128L;
				Xapi_globs.vgt_high_gm_sz, Int64.to_string 384L;
				Xapi_globs.vgt_fence_sz, Int64.to_string 4L;
			];
			implementation = `gvt_g;
		}

	let find_or_create_supported_types ~__context ~pci
			~is_system_display_device
			~is_host_display_enabled
			~is_pci_hidden =
		let types =
			if is_system_display_device
			then begin
				match is_host_display_enabled, is_pci_hidden with
				| false, true -> [passthrough_gpu]
				| true, true -> []
				| _, false -> [make_gvt_g_256 ~__context ~pci]
			end else [passthrough_gpu; make_gvt_g_256 ~__context ~pci]
		in
		List.map (find_or_create ~__context) types
end

let find_or_create_supported_types ~__context ~pci
		~is_system_display_device
		~is_host_display_enabled
		~is_pci_hidden =
	let vendor_id =
		Db.PCI.get_vendor_id ~__context ~self:pci
		|> Xapi_pci.int_of_id
	in
	if vendor_id = Nvidia.nvidia_vendor_id
	then begin
		if is_system_display_device then []
		else Nvidia.find_or_create_supported_types ~__context ~pci
	end
	else if vendor_id = Intel.intel_vendor_id
	then
		Intel.find_or_create_supported_types ~__context ~pci
			~is_system_display_device
			~is_host_display_enabled
			~is_pci_hidden
	else begin
		if is_system_display_device then []
		else [find_or_create ~__context passthrough_gpu]
	end

let requires_passthrough ~__context ~self =
	Db.VGPU_type.get_implementation ~__context ~self = `passthrough
