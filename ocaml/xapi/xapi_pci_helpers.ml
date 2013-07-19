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

open Xapi_pci

let parse_lspci_line pci_db line =
	let fields = String.split ' ' line in
	let fields = List.filter (fun s -> not (String.startswith "-" s)) fields in
	match fields with
	| "" :: _ -> failwith "Empty record"
	| [id; class_subclass; vendor_id; device_id; subvendor_id; subdevice_id] ->
		let class_id = String.sub class_subclass 0 2 in
		let open Pci_db in
		let vendor_name = (Pci_db.get_vendor pci_db vendor_id).v_name in
		let device_name = (Pci_db.get_device pci_db vendor_id device_id).d_name in
		let class_name = (Pci_db.get_class pci_db class_id).c_name in
		(* we'll fill in the related field when we've finished parsing *)
		let related = [] in
		{id; vendor_id; vendor_name; device_id; device_name;
			class_id; class_name; related}
	| _ -> failwith "Malformed record"

let find_related_ids pci other_pcis =
	let slot id = String.sub id 0 (String.index id '.') in
	List.map
		(fun p -> p.id)
		(List.filter
			(fun p -> p.id <> pci.id && slot p.id = slot pci.id) other_pcis)

let get_host_pcis pci_db =
	let lspci_out, _ = Forkhelpers.execute_command_get_output "/sbin/lspci" ["-mnD"] in
	let lspci_lines = String.split '\n' lspci_out in
	let lspci_lines =
		List.map (fun s -> String.filter_chars s ((<>) '"')) lspci_lines in

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

let () =
	(* For testing: prints info on hosts pci devices *)
	try
		let db = Pci_db.of_file "/usr/share/hwdata/pci.ids" in
		print_string "===== Host PCIs =====\n\n";
		let pcis = get_host_pcis db in
		List.iter
			(fun p ->
					List.iter
						(fun s -> print_string (s ^ " "))
						[p.id; p.vendor_id; p.vendor_name; p.device_id;
						 p.device_name; p.class_id; p.class_name];
					List.iter (fun s -> print_string (s ^ ", ")) p.related;
					print_newline ())
			pcis
	with e ->
		print_string (Printexc.to_string e);
		failwith "Failed to parse pci database"
