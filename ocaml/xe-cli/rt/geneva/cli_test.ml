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


let exportdir = ref ""

let bridge_name = ref "breth0"
      
let cd_test cli vmid =
  cd_attach_remove cli vmid "w2k3sesp1.iso" None;
  cd_attach_remove cli vmid "winxpsp2.iso" None;
(*  cd_attach_remove cli vmid "/dev/hda" None; *)
  cd_attach_remove cli vmid "xswindrivers.iso" None

(* -------------------------------------------------------------------------
    SCRIPT THE TESTS
   ------------------------------------------------------------------------- *)
    
(* Definitions used in tests: *)
let pv_guests = [("Debian Sarge 3.1","reg_pv1");
		 ("Debian Sarge 3.1","reg_pv2");
		 ("Debian Sarge 3.1","reg_pv3");
		 ("Debian Sarge 3.1","reg_v4")]
  
let pv_disks = [("sdc","512");
		("sdd","1024");
		("sde","2000");
		("sdf","512");
		("sdg","900")]

let hvm_guests = [
		  ("Windows XP SP2","reg_hvm1");
		  ("Windows XP SP2","reg_hvm2");
		  ("Windows XP SP2","reg_hvm3");
		  ("Windows XP SP2","reg_hvm4")
		 ]

let hvm_disks = [("hdb","512");
		 ("hdc","1024")]

let pv_nics () = [("eth5","00:00:00:00:55",!bridge_name);
		  ("eth6","00:00:00:00:66",!bridge_name)]
  
let hvm_nics () = [("nic5","00:00:00:00:55",!bridge_name);
		("nic6","00:00:00:00:66",!bridge_name)]

let vbridges = ["vbridge2";"vbridge3";"vbridge4"]

let patchfilename = "myfirstpatch.asc"

(* -------------------------------------------------------------------------
    Host test cycle
   ------------------------------------------------------------------------- *)

let run_host_test_cycle cli vbridges =

  (* Remove all existing vbridges from host *)
  let existing_vbridges = get_vbridges cli in
  let _ = List.iter (sync_remove_vbridge cli) existing_vbridges in

  (* Remove all existing patches from host (and check they're gone) *)
  let existing_patches = get_patches cli in
  let _ = List.iter (sync_remove_patch cli) existing_patches in

  (* Add vbridges to host and verify they're there *)
  let _ = List.iter (sync_add_vbridge cli) vbridges in

  (* Check loglevel calls *)
  let _ = loglevel_test cli in

  (* Generate host output report *)
  let _ =
    print_host_output cli in
    
  (* Remove vbridges from host and verify they're gone *)
  let _ = List.iter (sync_remove_vbridge cli) vbridges in

  (* Remove all existing patches from host and verify they're gone *)
  let existing_patches = get_patches cli in
  let _ = List.iter (sync_remove_patch cli) existing_patches in

    print_line "\n---- HOST TEST CYCLE FINISHED ----"


(* -------------------------------------------------------------------------
   VM test cycle
   ------------------------------------------------------------------------- *)

let run_vm_test_cycle is_hvm cli vms disks nics =

  (* Defs used for parameter get/set testing *)
  let vm_get_params =
    ["name";"description";"distribution";"distribution_vsn";
     "os";"vcpus";"memory_set";"auto_poweron";"force_hvm";"boot_params"] in

  let vm_set_params = [("name","testnameset");
		       ("description","testdescriptionset");
		       ("vcpus","1");
		       ("memory_set","300");
		       ("auto_poweron","false");
		       ("auto_poweron","true");
(*		       ("force_hvm","false");
		       ("force_hvm","true");*)
		       ("boot_params","boottest");
		       ("on_crash","destroy");
		       ("on_crash","restart")] in
  
  (* Force shutdown and uninstall all vms from host *)
  let _ = uninstall_all_vms cli in  
    
  (* Install new debian guests and remember their uuids *)
  let uuids =
    List.map
      (install_guest cli) vms in

  (* Arbitrarily single out first VM in list for per-VM tests... *)
  let test_vm = List.hd uuids in

  (* Add pv_disks to all VMs *)
  let _ =
    List.iter
      (fun vmid ->
	 List.iter (sync_add_disk cli vmid) disks) uuids in

  (* Add nics to all VMs *)
  let _ =
    List.iter
      (fun vmid ->
	 List.iter (sync_add_nic cli vmid) nics) uuids in

  (* Add/Remove/List CDs if HVM *)
  let _ =
    if is_hvm then cd_test cli test_vm in

  (* Generate output report *)
  let _ =
    print_vm_output cli test_vm in

  (* Remove nics we just added from all Vms *)
  let _ =
    List.iter
      (fun vmid ->
	 List.iter (sync_remove_nic cli vmid) 
	   (List.map (fun (n,_,_)->n) nics)) uuids in

  (* Resize disks on all Vms *)
  let _ =
    List.iter
      (fun (disk,size) ->
	 List.iter (fun vmid->resize_disk cli vmid disk size) uuids)
      (List.map
	 (fun (d,s)->(d,string_of_int ((int_of_string s)+500))) disks) in

  (* Remove disks from all VMs *)
  let _ =
    List.iter
      (fun vmid ->
	 List.iter (sync_remove_disk cli vmid)
	   (List.map fst disks)) uuids in
    
  (* Move VM between states in PV case *)
  let _ = if not is_hvm then state_test cli test_vm in

  (* Clone new VM *)
  let _ = clone_test cli test_vm "cloned-vm" in

  (* Test get params *)
  let _ =
    List.map
      (fun paramname -> getparam cli test_vm paramname)
      vm_get_params in
    
  (* Test set params *)
  let _ =
    List.iter
      (fun (p,s) -> set_and_check_param cli test_vm p s)
      vm_set_params in

  (* Uninstall everything *)
  let _ = uninstall_all_vms cli in    
    print_line "\n---- VM TEST CYCLE FINISHED ----"

let installuninstall cli =
  let vm_uuid =
    install_guest cli (List.hd hvm_guests) in
    sync_uninstall cli vm_uuid

(* import/export test *)
let export_checks cli =
  let vmid = install_guest cli (List.hd pv_guests) in
  let params =
    [("vm-id",vmid);
     ("dir-name",!exportdir)] in
  let _ = expect_success (fun()->cli "vm-export" params) in
  let _ = sync_uninstall cli vmid in
  let _ = expect_success (fun()->cli "vm-import" params) in
    ()      

(* -----------------------------------------------------------------------
    ENTRY POINT:
   ----------------------------------------------------------------------- *)

(* Read cmd-line args *)
let _ =
  Arg.parse [
	      "-host", Arg.Set_string host, "hostname of test XE host";
	      "-xe", Arg.Set_string xe, "path to XE CLI executable";
	      "-exportdir", Arg.Set_string exportdir, "path to export VM to";
	      "-bridge", Arg.Set_string bridge_name, 
	      Printf.sprintf "bridge to attach VIFs to (default %s)" !bridge_name;
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
  if !host = "" then
    begin
      print_line "Must specify -host option. Run with --help for more info";
      raise InvalidArgument
    end

let cli =
  if !host="onhost"
  then (print_line "TEST RUNNING IN ONHOST MODE\n"; cli_onhost)
  else (print_line "TEST RUNNING IN OFFHOST MODE\n"; cli_offhost)
    
(* Start by licensing server *)
(*
let _ = apply_license_to_server cli
*)

(* Run host test cycle using off-host CLI *)
let _ = run_host_test_cycle cli vbridges

(* Run VM test cycle for PV guests using off-host CLI *)
let _ = run_vm_test_cycle false cli pv_guests pv_disks (pv_nics ())

(* Run VM test cycle for HVM guests using off-host CLI *)
let _ = run_vm_test_cycle true cli hvm_guests hvm_disks (hvm_nics ())

(* Add some patches -- actually add the same one twice for now :) *)
let _ = expect_success (fun()->cli "host-patch-upload"
			  ["patch-file",patchfilename]) 

let _ = expect_success (fun()->cli "host-patch-upload"
			  ["patch-file",patchfilename]) 

let _ = expect_success (fun()-> cli "host-patch-apply"
			  ["patch-name",patchfilename]) 

let _ = if !exportdir<>"" then export_checks cli_offhost 

(* Off-host checks to run *)
let offhost_checks() =
    
  (* Check unix password file by running through hvm install/uninstall *)
  let _ = installuninstall (cli_offhost_with_pwf pwf_unix) in
    (* .. and same for windows password file [i.e. with CR/LFs] *)
  let _ = installuninstall (cli_offhost_with_pwf pwf_windows) in

  (* Test setting/resetting host password *)
  let _ = test_password_set cli_offhost_with_pwd in
  
  ()      

(* Run off-host checks if off-host *)
let _ = if !host<>"onhost" then offhost_checks()
