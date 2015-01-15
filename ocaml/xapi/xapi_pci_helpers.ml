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
open Opt

type pci_property = {
	id: int64;
	name: string;
}

type pci = {
	pci_id: string;
	vendor: pci_property;
	device: pci_property;
	pci_class: pci_property;
	subsystem_vendor: pci_property option;
	subsystem_device: pci_property option;
	related: string list;
}

let wrap_lookup f id =
	try f id
	with Not_found -> Printf.sprintf "Unknown (%04Lx)" id

let parse_lspci_line pci_db line =
	let fields = String.split ' ' line in
	let fields = List.filter (fun s -> not (String.startswith "-" s)) fields in
	Scanf.sscanf (String.concat " " fields)
		"%s \"%s@\" \"%Lx\" \"%Lx\" \"%s@\" \"%s@\""
		(fun pci_id class_subclass vendor_id device_id
				subsystem_vendor_id subsystem_device_id ->
			let int_of_hex_str = fun s -> Scanf.sscanf s "%Lx" (fun x -> x) in
			let class_id = int_of_hex_str (String.sub class_subclass 0 2) in
			let open Pci_db in
			let vendor_name = wrap_lookup (fun vendor_id ->
				(Pci_db.get_vendor pci_db vendor_id).v_name) vendor_id in
			let device_name = wrap_lookup (fun device_id ->
				(Pci_db.get_device pci_db vendor_id device_id).d_name) device_id in
			let class_name = (Pci_db.get_class pci_db class_id).c_name in
			let subsystem_vendor = match subsystem_vendor_id with
			| "" -> None
			| id_str ->
				let id = int_of_hex_str id_str in
				let name =
					wrap_lookup
						(fun subsystem_vendor_id ->
							(Pci_db.get_vendor pci_db subsystem_vendor_id).v_name) id in
				Some {id; name} in
			let subsystem_device = match subsystem_vendor, subsystem_device_id with
			| _, ""
			| None, _ -> None
			| Some subsystem_vendor, id_str ->
				let id = int_of_hex_str id_str in
				let name =
					wrap_lookup
						(fun subsystem_device_id ->
							Pci_db.get_subdevice pci_db vendor_id device_id subsystem_vendor.id subsystem_device_id) id in
				Some {id; name} in
			(* we'll fill in the related field when we've finished parsing *)
			let related = [] in
			{
				pci_id;
				vendor = {id = vendor_id; name = vendor_name};
				device = {id = device_id; name = device_name};
				subsystem_vendor;
				subsystem_device;
				pci_class = {id = class_id; name = class_name};
				related
			})

let find_related_ids pci other_pcis =
	let slot id = String.sub id 0 (String.index id '.') in
	List.map
		(fun p -> p.pci_id)
		(List.filter
			(fun p -> p.pci_id <> pci.pci_id && slot p.pci_id = slot pci.pci_id) other_pcis)

let get_host_pcis pci_db =
	let lspci_out, _ = Forkhelpers.execute_command_get_output "/sbin/lspci" ["-mnD"] in
	let lspci_lines = String.split '\n' lspci_out in

	let rec parse_lspci_lines pci_db ac = function
		| [] -> ac
		| hd :: tl ->
			try
				let pci = parse_lspci_line pci_db hd in
				parse_lspci_lines pci_db (pci :: ac) tl
			with _ -> parse_lspci_lines pci_db ac tl
	in
	let pcis = parse_lspci_lines pci_db [] lspci_lines in
	let rec link_related_pcis ac = function
		| [] -> ac
		| hd :: tl ->
			let related = find_related_ids hd pcis in
			let pci = {hd with related} in
			link_related_pcis (pci :: ac) tl
	in
	link_related_pcis [] pcis

let is_hidden_from_dom0 pci = true

let is_igd_whitelisted pci = true
