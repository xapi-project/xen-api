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

open OUnit
open Test_highlevel
open Xapi_pci_helpers

(* This test generates a lot of print --- set skip to false to enable *)
let skip = true

let print_host_pcis () =
	skip_if skip "Generates lots of text...";
	try
		let db = Pci_db.of_file Pci_db.base_pci_ids_path in
		print_string "===== Host PCIs =====\n\n";
		let pcis = get_host_pcis db in
		List.iter
			(fun p ->
				let x_to_str = Printf.sprintf "%04Lx" in
				Printf.printf "%s " (String.concat " "
					[
						p.pci_id;
						x_to_str p.vendor.id;
						p.vendor.name;
						x_to_str p.device.id;
						p.device.name;
						x_to_str p.pci_class.id;
						p.pci_class.name
					]);
				List.iter (fun s -> print_string (s ^ ", ")) p.related;
				print_newline ())
			pcis
	with e ->
		print_string (Printexc.to_string e);
		assert_equal 0 1

let pci_db_cache = ref None
let get_cached_pci_db () =
	match !pci_db_cache with
	| Some pci_db -> pci_db
	| None ->
		let pci_db = Pci_db.of_file "ocaml/test/data/base-pci.ids" in
		pci_db_cache := Some pci_db;
		pci_db

module ParseLspciLine = Generic.Make(struct
	module Io = struct
		type input_t = string
		type output_t = Xapi_pci_helpers.pci

		let string_of_input_t str = str
		let string_of_output_t pci = Xapi_pci_helpers.(
			Printf.sprintf
				"{%s; %Lx; %s; %Lx; %s; %Lx; %s; %s; %s; [%s]}"
				pci.pci_id
				pci.vendor.id
				pci.vendor.name
				pci.device.id
				pci.device.name
				pci.pci_class.id
				pci.pci_class.name
				(match pci.subsystem_vendor with
					| Some vendor -> Printf.sprintf "Some {%Lx; %s}" vendor.id vendor.name
					| None -> "None")
				(match pci.subsystem_device with
					| Some device -> Printf.sprintf "Some {%Lx; %s}" device.id device.name
					| None -> "None")
				(String.concat "; " pci.related))
	end

	let transform line =
		let pci_db = get_cached_pci_db () in
		parse_lspci_line pci_db line

	(* n.b. these tests might start failing if the copy of pci.ids in the chroot
	 * changes. *)
	let tests = [
		(* Test that a device present in pci.ids and with subdevice and
		 * subvendor IDs can be parsed. *)
		"0000:44:00.0 \"0001\" \"0003\" \"0001\" -ra1 \"0001\" \"0002\"",
		Xapi_pci_helpers.({
			pci_id = "0000:44:00.0";
			vendor = {id = 0x0003L; name = "SimpleVendorName3"};
			device = {id = 0x0001L; name = "SimpleDeviceName-3-1"};
			pci_class = {id = 0x00L; name = "Class Name"};
			subsystem_vendor = Some {id = 0x0001L; name = "SimpleVendorName1"};
			subsystem_device = Some {id = 0x0002L; name = "SimpleSubDeviceName-3-1-1-2"};
			related = []
		});
		(* Test that a device present in pci.ids without subdevice or
		 * subvendor IDs can be parsed. *)
		"0000:45:00.0 \"0001\" \"0003\" \"0001\" -ra1 \"\" \"\"",
		Xapi_pci_helpers.({
			pci_id = "0000:45:00.0";
			vendor = {id = 0x0003L; name = "SimpleVendorName3"};
			device = {id = 0x0001L; name = "SimpleDeviceName-3-1"};
			pci_class = {id = 0x00L; name = "Class Name"};
			subsystem_vendor = None;
			subsystem_device = None;
			related = []
		});
		(* Test that a device present in pci.ids with an unknown subsystem device
		 * ID can be parsed. *)
		"0000:0a:00.0 \"0001\" \"0003\" \"0001\" -ra1 \"0001\" \"0123\"",
		Xapi_pci_helpers.({
			pci_id = "0000:0a:00.0";
			vendor = {id = 0x0003L; name = "SimpleVendorName3"};
			device = {id = 0x0001L; name = "SimpleDeviceName-3-1"};
			pci_class = {id = 0x00L; name = "Class Name"};
			subsystem_vendor = Some {id = 0x0001L; name = "SimpleVendorName1"};
			subsystem_device = Some {id = 0x0123L; name = "Unknown (0123)"};
			related = [];
		});
		(* Test that a device present in pci.ids with an unknown subsystem vendor
		 * ID and an unknown subsystem device ID can be parsed. *)
		"0000:0a:00.0 \"0001\" \"0003\" \"0001\" -ra1 \"0123\" \"0123\"",
		Xapi_pci_helpers.({
			pci_id = "0000:0a:00.0";
			vendor = {id = 0x0003L; name = "SimpleVendorName3"};
			device = {id = 0x0001L; name = "SimpleDeviceName-3-1"};
			pci_class = {id = 0x00L; name = "Class Name"};
			subsystem_vendor = Some {id = 0x0123L; name = "Unknown (0123)"};
			subsystem_device = Some {id = 0x0123L; name = "Unknown (0123)"};
			related = [];
		});
		(* Test that a device not preset in pci.ids can be parsed. *)
		"0000:46:00.0 \"0001\" \"0055\" \"abcd\" -ra1 \"\" \"\"",
		Xapi_pci_helpers.({
			pci_id = "0000:46:00.0";
			vendor = {id = 0x0055L; name = "Unknown (0055)"};
			device = {id = 0xabcdL; name = "Unknown (abcd)"};
			pci_class = {id = 0x00L; name = "Class Name"};
			subsystem_vendor = None;
			subsystem_device = None;
			related = [];
		});
		(* Test that an unknown device from a known vendor can be parsed. *)
		"0000:47:00.0 \"0001\" \"0003\" \"abcd\" -ra1 \"\" \"\"",
		Xapi_pci_helpers.({
			pci_id = "0000:47:00.0";
			vendor = {id = 0x0003L; name = "SimpleVendorName3"};
			device = {id = 0xabcdL; name = "Unknown (abcd)"};
			pci_class = {id = 0x00L; name = "Class Name"};
			subsystem_vendor = None;
			subsystem_device = None;
			related = [];
		});
	]
end)

let test =
	"test_pci_helpers" >:::
		[
			"print_host_pcis" >:: print_host_pcis;
			"parse_lspci_line" >:: ParseLspciLine.test;
		]
