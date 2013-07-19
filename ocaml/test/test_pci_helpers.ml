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
open Xapi_pci_helpers

(* This test generates a lot of print --- set skip to false to enable *)
let skip = true

let print_host_pcis () =
	skip_if skip "Generates lots of text...";
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
		assert_equal 0 1

let test =
	"test_pci_helpers" >:::
		["print_host_pcis" >:: print_host_pcis;]
