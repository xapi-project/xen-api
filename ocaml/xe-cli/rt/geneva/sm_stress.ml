(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

open Utils
open Cli_utils

(* -----------------------------------------------------------------------
    ENTRY POINT:
   ----------------------------------------------------------------------- *)

(* Run on-host *)
let cli = cli_onhost

let base_size1 = ref 0
let base_size2 = ref 0
let size_inc   = ref 0

(* Read cmd-line args *)
let _ =
  Arg.parse [
(*	      "-host", Arg.Set_string host, "hostname of test XE host"; *)
	      "-xe", Arg.Set_string xe, "path to XE CLI executable";
	      "-base_size1", Arg.Set_int base_size1, "base-size1 for disk create";
	      "-base_size2", Arg.Set_int base_size2, "base-size2 for disk create";
	      "-size_inc", Arg.Set_int size_inc, "size-increment for disk resizing"
	    ]
    (fun x -> Printf.printf "Warning, ignoring unknown argument: %s" x)
    "Regression test for XE CLI"

exception InvalidArgument
let _ =
  if !xe = "" then
    begin
      print_line "Must specify -xe option. Run with --help for more info";
      raise InvalidArgument
    end;
(*
  if !host = "" then
    begin
      print_line "Must specify -host option. Run with --help for more info";
      raise InvalidArgument
    end;
*)
  if !base_size1<=0 || !base_size2<=0 || !size_inc<=0 then
    begin
      print_line "Must specify int (>0) values for base_size1, base_size2, size_inc";
      raise InvalidArgument
    end

(* Start by licensing server *)
let _ = apply_license_to_server cli

(* Uninstall all VMs *)
let _ = uninstall_all_vms cli

(* Install HVM guests *)
let hvm_guests = [
		  ("Windows XP Service Pack 2","sr_stress_hvm1");
		  ("Windows XP Service Pack 2","sr_stress_hvm2");
		  ("Windows XP Service Pack 2","sr_stress_hvm3");
		  ("Windows XP Service Pack 2","sr_stress_hvm4")
		 ]

let uuids = List.map (install_guest cli) hvm_guests

let addtostring s i =
  string_of_int ((int_of_string s)+i)

let disk_add_remove size1 size2 vmid =
  print_line "Add/remove disk running";
  for i=1 to 100
  do
    sync_add_disk cli vmid ("hdb",string_of_int size1);
    sync_add_disk cli vmid ("hdc",string_of_int size2);
    resize_disk cli vmid "hdb" (string_of_int (size1 + !size_inc));
    resize_disk cli vmid "hdb" (string_of_int (size1 + !size_inc + !size_inc));
    resize_disk cli vmid "hdc" (string_of_int (size2 + !size_inc));
    resize_disk cli vmid "hdb" (string_of_int (size1 + !size_inc + !size_inc));
    sync_remove_disk cli vmid "hdb";
    sync_remove_disk cli vmid "hdc"
  done

let threads =
  List.map (Thread.create (disk_add_remove !base_size1 !base_size2)) uuids

let _ = List.iter Thread.join threads


(* Install some PV guests *)
let pv_guests = [("Debian Sarge 3.1","reg_pv1");
		 ("Debian Sarge 3.1","reg_pv2");
		 ("Debian Sarge 3.1","reg_pv3");
		 ("Debian Sarge 3.1","reg_v4")]

let pv_uuids = List.map (install_guest cli) pv_guests

(* Start them all up sequentially *)
let sync_startup vmid =
    let param = [("vm-id",vmid)] in
      ignore (expect_success (fun ()->cli "vm-start" param));
      waitstate cli vmid "UP"
let _ = List.iter sync_startup pv_uuids

(* And suspend them all concurrently *)
let suspend vmid =
  let param = [("vm-id",vmid)] in
    ignore (expect_success (fun ()->cli "vm-suspend" param))
let _ = List.iter suspend pv_uuids


(*    
(* And then uninstall everything again *)
      let _ = uninstall_all_vms cli
*)
