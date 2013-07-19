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

(* This test generates a lot of print --- set skip to false to enable *)
let skip = true

let print_pci_db () =
	skip_if skip "Generates lots of text...";
	try
		let db = Pci_db.of_file "/usr/share/hwdata/pci.ids" in
		Pci_db.print db
	with e ->
		print_string (Printf.sprintf "%s\n" (Printexc.to_string e));
		assert false (* fail *)

let test =
	"test_pci_db" >:::
		["print_pci_db" >:: print_pci_db;]
