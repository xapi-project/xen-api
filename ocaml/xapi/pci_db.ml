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

open Stringext

let pci_ids_path = "/usr/share/hwdata/pci.ids"

type class_id = int64
type subclass_id = int64
type vendor_id = int64
type subvendor_id = int64
type device_id = int64
type subdevice_id = int64

type pci_class = {
	c_name : string;
	subclass_names : (subclass_id, string) Hashtbl.t
}

type device = {
	d_name : string;
	subdevice_names : ((subvendor_id * subdevice_id), string) Hashtbl.t
}

type vendor = {
	v_name : string;
	devices : (device_id, device) Hashtbl.t
}

type t = {
	classes : (class_id, pci_class) Hashtbl.t;
	vendors : (vendor_id, vendor) Hashtbl.t
}

let make c_size v_size =
	{classes = Hashtbl.create c_size; vendors = Hashtbl.create v_size}

let add_class t c_id c = Hashtbl.add t.classes c_id c
let get_class t c_id = Hashtbl.find t.classes c_id

let add_subclass t c_id sc_id name =
	let c = get_class t c_id in
	Hashtbl.add c.subclass_names sc_id name
let get_subclass t c_id sc_id =
	let c = get_class t c_id in
	Hashtbl.find c.subclass_names sc_id

let add_vendor t v_id v = Hashtbl.add t.vendors v_id v
let get_vendor t v_id = Hashtbl.find t.vendors v_id

let add_device t v_id d_id d =
	let v = get_vendor t v_id in
	Hashtbl.add v.devices d_id d
let get_device t v_id d_id =
	let v = get_vendor t v_id in
	Hashtbl.find v.devices d_id

let add_subdevice t v_id d_id sv_id sd_id name =
	let d = get_device t v_id d_id in
	Hashtbl.add d.subdevice_names (sv_id, sd_id) name
let get_subdevice t v_id d_id sv_id sd_id =
	let d = get_device t v_id d_id in
	Hashtbl.find d.subdevice_names (sv_id, sd_id)
let get_subdevice_names_by_id t v_id d_id id =
	let d = get_device t v_id d_id in
	Hashtbl.fold
		(fun (sv_id, sd_id) sd_name res ->
			(if sd_id = id then sd_name :: res else res))
		d.subdevice_names []

let strings_of_subclasses pci_class =
	Hashtbl.fold
		(fun sc_id sc_name lines ->
			let line = Printf.sprintf "\t%04Lx %s\n" sc_id sc_name in
			line :: lines
		) pci_class.subclass_names []

let strings_of_classes t =
	Hashtbl.fold
		(fun c_id pci_class class_lines ->
			let subclass_lines = strings_of_subclasses pci_class in
			let class_line = Printf.sprintf "C %02Lx %s\n" c_id pci_class.c_name in
			class_line :: subclass_lines @ class_lines
		) t.classes []

let strings_of_subdevices device =
	Hashtbl.fold
		(fun (sv_id, sd_id) sd_name lines ->
			let line = Printf.sprintf "\t\t%04Lx %04Lx  %s\n" sv_id sd_id sd_name in
			line :: lines
		) device.subdevice_names []

let strings_of_devices vendor =
	Hashtbl.fold
		(fun d_id device device_lines ->
			let subdevice_lines = strings_of_subdevices device in
			let device_line = Printf.sprintf "\t%04Lx  %s\n" d_id device.d_name in
			device_line :: subdevice_lines @ device_lines
		) vendor.devices []

let strings_of_vendors t =
	Hashtbl.fold
		(fun v_id vendor vendor_lines ->
			let device_lines = strings_of_devices vendor in
			let vendor_line = Printf.sprintf "%04Lx  %s\n" v_id vendor.v_name in
			vendor_line :: device_lines @ vendor_lines
		) t.vendors []

let strings_of_t t =
	strings_of_vendors t @ strings_of_classes t

let print t =
	List.iter (fun s -> print_string s) (strings_of_t t)

let of_file path =
	let lines = Unixext.read_lines ~path in
	let lines =
		List.filter (fun s -> (not (String.startswith "#" s || s = ""))) lines in
	let strip = String.strip String.isspace in
	let int_of_hex_str = fun s -> Scanf.sscanf s "%Lx" (fun x -> x) in
	let cur_class, cur_vendor, cur_device = ref None, ref None, ref None in
	let parse_one_line line pci_db =
		match String.explode (String.sub line 0 2) with
		| 'C' :: _ -> (* this is a class definition *)
			cur_vendor := None; cur_device := None;
			let line = String.sub_to_end line 2 in
			begin match String.split ' ' line ~limit:2 with
			| [c_id; name] ->
				let pci_class =
					{c_name = strip name; subclass_names = Hashtbl.create 8} in
				let c_id = int_of_hex_str c_id in
				add_class pci_db c_id pci_class;
				cur_class := Some c_id
			| _ -> failwith "Malformed class definition"
			end
		| ['\t'; '\t'] ->
			let line = String.sub_to_end line 2 in
			begin match !cur_device with
			| Some d_id -> (* this is a subdevice definition *)
				begin match String.split ' ' line ~limit:3 with
				| [sv_id; sd_id; name] ->
					let sv_id = int_of_hex_str sv_id
					and sd_id = int_of_hex_str sd_id in
					add_subdevice pci_db (Opt.unbox !cur_vendor) d_id sv_id sd_id (strip name)
				| _ -> failwith "Malformed subdevice definition"
				end
			| None -> ()
			end
		| '\t' :: _ ->
			let line = String.sub_to_end line 1 in
			begin match !cur_vendor with
			| Some v -> (* this is a device definition *)
				begin match String.split ' ' line ~limit:2 with
				| [d_id; name] ->
					let device =
						{d_name = strip name; subdevice_names = Hashtbl.create 0} in
					let d_id = int_of_hex_str d_id in
					add_device pci_db (Opt.unbox !cur_vendor) d_id device;
					cur_device := Some d_id
				| _ -> failwith "Malformed device definition"
				end
			| None -> (* this is a subclass definition *)
				begin match String.split ' ' line ~limit:2 with
				| [sc_id; name] ->
					let sc_id = int_of_hex_str sc_id in
					add_subclass pci_db (Opt.unbox !cur_class) sc_id (strip name)
				| _ -> failwith "Malformed subclass definition"
				end
			end
		| _ -> (* this is a vendor definition *)
			cur_class := None; cur_device := None;
			begin match String.split ' ' line ~limit:2 with
			| [v_id; name] ->
				let vendor =
					{v_name = strip name; devices = Hashtbl.create 8} in
				let v_id = int_of_hex_str v_id in
				add_vendor pci_db v_id vendor;
				cur_vendor := Some v_id
			| _ -> failwith "Malformed vendor definition"
			end
	in
	let rec parse_lines lines pci_db =
		match lines with
		| [] -> pci_db
		| line :: remaining ->
			parse_one_line line pci_db;
			parse_lines remaining pci_db
	in
	let pci_db = make 24 2048 in
	parse_lines lines pci_db
