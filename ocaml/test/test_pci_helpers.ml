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
					[p.id; x_to_str p.vendor_id; p.vendor_name; x_to_str p.device_id;
						p.device_name; x_to_str p.class_id; p.class_name]);
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
		let pci_db = Pci_db.of_file Pci_db.base_pci_ids_path in
		pci_db_cache := Some pci_db;
		pci_db

module ParseLspciLine = Generic.Make(struct
	module Io = struct
		type input_t = string
		type output_t = Xapi_pci_helpers.pci

		let string_of_input_t str = str
		let string_of_output_t pci = Xapi_pci_helpers.(
			Printf.sprintf
				"{%s; %Lx; %s; %Lx; %s; %Lx; %s; [%s]}"
				pci.id
				pci.vendor_id
				pci.vendor_name
				pci.device_id
				pci.device_name
				pci.class_id
				pci.class_name
				(String.concat "; " pci.related))
	end

	let transform line =
		let pci_db = get_cached_pci_db () in
		parse_lspci_line pci_db line

	(* n.b. these tests might start failing if the copy of pci.ids in the chroot
	 * changes. *)
	let tests = [
		(* Test that a display device present in pci.ids and with subdevice and
		 * subvendor IDs can be parsed. *)
		"0000:44:00.0 \"0300\" \"10de\" \"014e\" -ra1 \"10de\" \"100a\"",
		Xapi_pci_helpers.({
			id = "0000:44:00.0";
			vendor_id = 0x10deL;
			vendor_name = "NVIDIA Corporation";
			device_id = 0x014eL;
			device_name = "NV43GL [Quadro FX 540]";
			class_id = 0x03L;
			class_name = "Display controller";
			related = []
		});
		(* Test that a display device present in pci.ids without subdevice or
		 * subvendor IDs can be parsed. *)
		"0000:44:00.0 \"0300\" \"10de\" \"014e\" -ra1 \"\" \"\"",
		Xapi_pci_helpers.({
			id = "0000:44:00.0";
			vendor_id = 0x10deL;
			vendor_name = "NVIDIA Corporation";
			device_id = 0x014eL;
			device_name = "NV43GL [Quadro FX 540]";
			class_id = 0x03L;
			class_name = "Display controller";
			related = []
		});
		(* Test that a display device not preset in pci.ids can be parsed. *)
		"0000:44:00.0 \"0300\" \"0055\" \"abcd\" -ra1 \"\" \"\"",
		Xapi_pci_helpers.({
			id = "0000:44:00.0";
			vendor_id = 0x0055L;
			vendor_name = "Unknown (0055)";
			device_id = 0xabcdL;
			device_name = "Unknown (abcd)";
			class_id = 0x03L;
			class_name = "Display controller";
			related = [];
		});
		(* Test that an unknown device from a known vendor can be parsed. *)
		"0000:44:00.0 \"0300\" \"10de\" \"abcd\" -ra1 \"\" \"\"",
		Xapi_pci_helpers.({
			id = "0000:44:00.0";
			vendor_id = 0x10deL;
			vendor_name = "NVIDIA Corporation";
			device_id = 0xabcdL;
			device_name = "Unknown (abcd)";
			class_id = 0x03L;
			class_name = "Display controller";
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
