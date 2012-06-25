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
(* Now for some tests! *)
   
open Cliops
open Util
open Log
open Stringext

let log = Testlog.log
let set_ignore_errors = Testlog.set_ignore_errors

let fatal_error = ref false

let runtest (cli : Util.t_cli) test_type (name,func,clas,desc) =
  Testlog.reset_log ();
  reset_xapi_log cli;
  let pic = 
    begin
      Printf.fprintf stderr "Running test %s\n" name;
      flush_all ();
      try 
	func (); 
	if !Cliops.pic <> "" then Some !Cliops.pic else None
      with 
	  CliOpFailed cmdlog -> 
	    log Error "Failure of cli command. Command output follows:";
	    List.iter (fun s -> log Error "%s" s) cmdlog;
	    Some !Cliops.pic
	| OpFailed msg ->
	    log Error "Operation failed! msg=%s" msg;
	    Some !Cliops.pic
	| e ->
	    log Error "Uncaught exception! %s" (Printexc.to_string e);
	    Some !Cliops.pic
    end
  in
  Cliops.pic := "";
  let pic = match pic with Some "" -> None | Some x -> Some x | None -> None in
  let xapi_log = get_xapi_log cli in
  Testlog.register_test name test_type clas desc xapi_log pic



(* Power state checks
 *
 * Start and stop the VM, checking that it comes up correctly,
 * and also make sure that inappropriate state changes are  
 * prevented by xapi *)

let powerstate (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in

(* WARNING: very dumb thing here of checking whether a guest has PV drivers or not *)
(*          by checking whether the VM name has 'pv' in it!  *)

  let is_pv = String.endswith "pv" domainname in
  
  let ps_init () =
    log Info "Test: VM NAME='%s'" domainname;
    log Info "Test: Initial powerstate test";
    log Info "Make sure VM is currently down:";
    ignore (ensure_vm_down cli vmid 0)
  in

  let ps_stopped_statechange_failures () =
    log Info "Checking for failure of some powerstate operations";
    fail_if_state_isnt cli vmid "halted";
    change_vm_state_fail cli vmid Shutdown;
    change_vm_state_fail cli vmid Resume;
    change_vm_state_fail cli vmid Suspend;
  in
  
  let ps_start () =
    log Info "Starting VM";
    fail_if_state_isnt cli vmid "halted";
    change_vm_state cli vmid Start;
  in
	  
  let ps_started_statechange_failures () =
    log Info "Checking for failure of some powerstate operations";
    fail_if_state_isnt cli vmid "running";
    change_vm_state_fail cli vmid Start;
    change_vm_state_fail cli vmid Resume;
  in

  let ps_suspend () =
    log Info "Suspending VM";
    fail_if_state_isnt cli vmid "running";
    change_vm_state cli vmid Suspend 
  in

  let ps_suspended_statechange_failures () =
    log Info "Checking for failure of some powerstate operations";
    fail_if_state_isnt cli vmid "suspended";
    change_vm_state_fail cli vmid Start;
    change_vm_state_fail cli vmid Shutdown;
    change_vm_state_fail cli vmid Suspend;
  in

  let ps_resume () =
    log Info "Resuming VM";
    fail_if_state_isnt cli vmid "suspended";
    change_vm_state cli vmid Resume;
  in

  let ps_reboot () =
    log Info "Rebooting VM";
    fail_if_state_isnt cli vmid "running";
    change_vm_state cli vmid Reboot; 
  in

  let ps_shutdown () =
    log Info "Shutting VM down";
    fail_if_state_isnt cli vmid "running";
    change_vm_state cli vmid Shutdown
  in

  let ps_shutdown_no_agent () =
    log Info "Shutting VM down without the guest agent";
    change_vm_state cli vmid Start;
    fail_if_state_isnt cli vmid "running";
    Cliops.use_gt := false;
    try
      change_vm_state cli vmid Shutdown;
      Cliops.use_gt := true
    with e -> 
      Cliops.use_gt := true; (* Make sure that whatever happens, we reset this flag *)
      raise e      
  in


  let start_tests = 
    [("ps_init__________________________",ps_init,"powerstate",
      "Initial setup of the powerstate tests. The VM's status \
       is queried and everything possible is done to make sure it's down. The \
       behaviours on reboot and shutdown are also set");

     ("ps_stopped_statechange_failures__",ps_stopped_statechange_failures,"powerstate",
      "Check that when the VM is currently stopped, the operations \
       shutdown, resume and suspend all fail");

     ("ps_start_________________________",ps_start,"powerstate",
      "Start the VM. This test includes waiting for the VM to report to us that \
       it has started OK, and hence network breakage could cause this test to fail");

     ("ps_started_statechange_failures__",ps_started_statechange_failures,"powerstate",
      "Check that when the VM is started, the operations start and resume both fail") ] in

  let suspend_tests =
    [("ps_suspend_______________________",ps_suspend,"powerstate-nonhvm","Suspend the VM");

     ("ps_suspended_statechange_failures",ps_suspended_statechange_failures,"powerstate-nonhvm",
      "Check that when the VM is suspended, the operations start, shutdown and \
       suspend all fail");

     ("ps_resume________________________",ps_resume,"powerstate-nonhvm","Resume the VM")] in

  let end_tests = 
    [("ps_reboot________________________",ps_reboot,"powerstate","Reboot the VM. This test uses the guest agent");

     ("ps_shutdown______________________",ps_shutdown,"powerstate","Shutdown the VM. This test uses the guest agent")] in
  
  let noagent_tests =
    [("ps_shutdown_no_agent_____________",ps_shutdown_no_agent,"powerstate","Startup and shutdown the VM purely using the cli")] in

  let tests = 
    if is_suspendable cli vmid 
    then start_tests @ suspend_tests @ end_tests
    else start_tests @ end_tests in

  let tests = 
    if is_pv
    then tests @ noagent_tests
    else tests in
 
  List.iter (runtest cli (Testlog.OnlineVM domainname)) tests

let clone_test (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in
  log Info "Test: VM NAME='%s'" domainname;
  log Info "Test: Testing clone operation";
  log Info "Uninstalling any clones";
  (try
    let cloneuuid = get_uuid cli "clone" in
    ignore(uninstall cli cloneuuid)
  with _ -> ());
  ensure_vm_down cli vmid 0;
  let runclone () =
    log Info "Cloning VM";
    let (_: string list) = clone cli vmid "clone" in
    let newvmid = get_uuid cli "clone" in
    log Info "Starting VM";
    begin
      try
	change_vm_state cli newvmid Start;
	ignore (get_client_ip newvmid) (* Will throw exception if the VM has failed to register *)
      with _ -> 
	log Error "VM failed to start correctly!"
    end;
    log Info "Shutting down VM";
    set_ignore_errors true; (* Error if the VM fails to start *)
    begin
      try
	change_vm_state cli newvmid Shutdown;
	set_ignore_errors false
      with e ->
	set_ignore_errors false;
	raise e
    end;
    log Info "Uninstalling VM";
    ignore(uninstall cli newvmid)
  in

  let tests =
    [("vm_clone_________________________",runclone,"clone","Clone the VM and ensure it still starts")] in

  List.iter (runtest cli (Testlog.GuestVerified domainname)) tests  

let cd_guest_verified (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in
  log Info "Test: VM NAME='%s'" domainname;
  log Info "Test: Testing CD operations";
  ensure_vm_down cli vmid 0;
  let cd1 = ("xs-tools.iso",get_vdi_uuid_from_name cli "xs-tools.iso","2","xenlegacy.exe") in
  let cd2 = ("xs-tools.iso",get_vdi_uuid_from_name cli "xs-tools.iso","3","xenlegacy.exe") in

  let checkcdset cdset =
    let missingcds = List.filter (fun (name,uuid,device,file) -> uuid="") cdset in
    List.iter (fun (name,_,_,_) -> failwith (Printf.sprintf "Cannot find cd '%s'" name)) missingcds
  in

  let cd_test cdset =
    log Info "Starting test of CDs";
    checkcdset cdset;
    log Info "Removing all currently attached CDs";
    remove_all_cds cli vmid;
    let cdattach (name,uuid,device,file) =
      ignore (attach_cd cli vmid uuid device) in
    List.iter cdattach cdset;
    log Info "Attached cd(s)";
    let cduuids = List.map (fun (a,b,c,d) -> b) cdset in
    check_attached_cds cli vmid cduuids;
    log Info "Attached cd(s) verified. Starting VM";
    set_ignore_errors false; (* Error if the VM fails to start *)
    change_vm_state cli vmid Start;
    let cddevices = List.map (fun (a,b,c,d) -> c) cdset in
    log Info "Sleeping for 30 secs to allow windows to notice CDs";
    Unix.sleep 30;
    log Info "Checking readability of attached cd(s)";
    let gacmd = "checkcd " ^ (String.concat " " cddevices) in
    let ip = get_client_ip vmid in
    let lines = run_ga_command ip gacmd in
    let lines = List.map String.lowercase lines in
    let results = List.map (fun (a,b,c,d) -> grep lines d) cdset in
    let results = List.flatten results in
    if List.length results <> List.length cdset 
    then
      begin
	log Error "CD test failed";
	log Error "Expected results containing: %s" (String.concat "," (List.map (fun (a,b,c,d) -> d) cdset));
      end;
    log Info "Stopping VM and detaching all cds";
    set_ignore_errors true;
    change_vm_state cli vmid Shutdown;
    set_ignore_errors false;
    remove_all_cds cli vmid
  in
  
  let cd_hotplug cdset =
    log Info "Starting CD hotplug test";
    checkcdset cdset;
    log Info "Removing all currently attached CDs";
    remove_all_cds cli vmid;
    let cdattach (name,uuid,device,file) =
      ignore (attach_cd cli vmid uuid device) in
    set_ignore_errors false; (* Catch errors on startup *)
    change_vm_state cli vmid Start;
    (*    set_ignore_errors false;*)
    try
      for i = 1 to 100 do
	List.iter cdattach cdset;
	log Info "Attached cd(s)";
	let cduuids = List.map (fun (a,b,c,d) -> b) cdset in
	check_attached_cds cli vmid cduuids;
	log Info "Attached cd(s) verified by xapi";
	let cddevices = List.map (fun (a,b,c,d) -> c) cdset in
	let gacmd = "checkcd " ^ (String.concat " " cddevices) in
	let gacmdfail = "checkcdfail " ^ (String.concat " " cddevices) in
	let ip = get_client_ip vmid in
	let lines = run_ga_command ip gacmd in
	let lines = List.map String.lowercase lines in
	let results = List.map (fun (a,b,c,d) -> grep lines d) cdset in
	let results = List.flatten results in
	if List.length results <> List.length cdset 
	then
	  begin
	    log Error "CD test failed";
	    log Error "Expected results containing: %s" (String.concat "," (List.map (fun (a,b,c,d) -> d) cdset));
	  end;
	log Info "Test succeeded! Detaching all cds";
	remove_all_cds cli vmid;
	let lines = run_ga_command ip gacmdfail in
	let lines = List.map String.lowercase lines in
	let results = List.map (fun (a,b,c,d) -> grep lines d) cdset in
	let results = List.flatten results in
	if List.length results <> 0 then
	  begin
	    log Error "CD test failed";
	    log Error "Found non-null results when nothing was expected!"
	  end
      done;
      remove_all_cds cli vmid;
      log Info "Test succeeded! Shutting VM down";
      set_ignore_errors true;
      change_vm_state cli vmid Shutdown ;
      set_ignore_errors false
    with e -> 	remove_all_cds cli vmid; raise e
  in
  
  let tests = 
    [("cd_test_3________________________",(fun () -> cd_test [cd1]),"cd",
     "Attaching cd to device 3 while VM is stopped, booting, verifying the guest can read it, shutting down "^
       "and detaching the cd");
(*     ("cd_test_3_4",(fun () -> cd_test [cd1;cd2]),"cd",
     "Attaching cds to devices 3 and 4 while VM is stopped, booting, verifying the guest can read it, shutting down "^
       "and detaching the cds");
*)
     ("cd_test_4________________________",(fun () -> cd_test [cd2]),"cd",
     "Attaching cd to device 4 while VM is stopped, booting, verifying the guest can read it, shutting down "^
       "and detaching the cd")
] in
  let hotplug_tests =
    [("cd_hotplug_3_____________________",(fun () -> cd_hotplug [cd1]),"cd",
     "Attaching cd to device 3 while VM is started then verifying the guest can read it, repeated 100 times. ");
(*
     ("cd_hotplug_3_4",(fun () -> cd_hotplug [cd1;cd2]),"cd",
     "Attaching cd to device 3 while VM is started then verifying the guest can read it, repeated 100 times. ");
*)
     ("cd_hotplug_4_____________________",(fun () -> cd_hotplug [cd2]),"cd",
     "Attaching cd to device 3 while VM is started then verifying the guest can read it, repeated 100 times. ")
] in
  
  let tests = 
    if is_suspendable cli vmid    (* Only PV Linux can hotplug cds *)
    then tests @ hotplug_tests
    else tests in
  
  List.iter (runtest cli (Testlog.GuestVerified domainname)) tests


let importexport (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in
  log Info "Test: VM NAME='%s'" domainname;
  log Info "Test: Testing import/export operations (guest verified)";
  ensure_vm_down cli vmid 0;
  let test () =
    log Info "Exporting VM";
    let (_: int) = Sys.command "rm -f /mnt/export" in
    let (_: string list) = export cli vmid "/mnt/export" in
    log Info "Renaming current VM";
    try
      let (_: string list) = set_param cli vmid "name-label" "tmp" in
      log Info "Importing VM";
      let (_: string list) = import cli "/mnt/export" in
      let newname=domainname in
      let newvmid=get_uuid cli newname in
      change_vm_state cli newvmid Start;
      begin
	try
	  ignore(get_client_ip newvmid) (* Will throw exception if the VM has failed to register *)
	with
	    Not_found -> 
	      log Error "VM Failed to start correctly"
      end;
      log Info "Shutting down VM";
      set_ignore_errors true; (* Don't error if the VM fails to stop *)
      begin
	try
	  change_vm_state cli newvmid Shutdown;
	  set_ignore_errors false
	with e ->
	  set_ignore_errors false;
	  raise e
      end;
      log Info "Uninstalling VM";
      ignore(uninstall cli newvmid);
      ignore(set_param cli vmid "name-label" domainname);
    with e ->
      let (_: string list) = set_param cli vmid "name-label" domainname in
      raise e
  in
  
  let tests =
    [("vm_importexport__________________",test,"importexport","Export the VM, import it, and check that it still starts")] in
  
  List.iter (runtest cli (Testlog.GuestVerified domainname)) tests


    
(* NB. this test requires a vm called 'debian-pv' which has dosfstools installed *)
let disk_guest_verified (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in
  let debianuuid = get_uuid cli "debian-pv" in
  log Info "Test: VM NAME='%s'" domainname;
  log Info "Test: Testing disk operations (guest verified)";
  ensure_vm_down cli vmid 0;
  ensure_vm_down cli debianuuid 0;
  
  let disk_test () =
    log Info "Setting up test disk";
    let (vdi_uuid,vbd_uuid) = add_disk cli debianuuid ("hdd","20MiB") in
    
    log Info "Disk added. Starting debian-pv";
    change_vm_state cli debianuuid Start;
    let (_: string list) = run_ga_command (get_client_ip debianuuid) "setuptestdisk hdd" in
    log Info "Disk setup. Stopping debian-pv";
    change_vm_state cli debianuuid Shutdown;
    log Info "Removing disk from debian-pv";
    detach_disk cli debianuuid vbd_uuid;
    log Info "Starting test of disk";
    log Info "Ensuring nothing attached to device hdd";
    ignore(detach_disk_if_present cli vmid "hdd");
    ignore(detach_disk_if_present cli vmid "3");
    let newvbd = attach_disk cli vmid vdi_uuid "hdd" in
    log Info "Attached disk. Starting VM";
    set_ignore_errors false; (* Error if the VM fails to start *)
    change_vm_state cli vmid Start;
    log Info "Checking readability of attached disk";
    let rec get_err_string err n =
      if n>30 then err else begin
	Unix.sleep 1;
	let gacmd = "checkmountdisk hdd1" in
	let ip = get_client_ip vmid in
	let lines = run_ga_command ip gacmd in
	let lines = List.map String.lowercase lines in
	let results = grep lines "testing" in
	if List.length results <> 1 then
	  get_err_string (Some lines) (n+1)
	else
	  None
      end
    in
    match get_err_string None 0 with 
      | Some lines ->
	  log Error "Disk test failed!";
	  log Error "Expected results containing: 'testing'";
	  log Error "Returned strings:";
	  List.iter (fun line -> log Error "%s" line) lines;
	  fatal_error:=true;	
      | None -> 
	  log Info "Stopping VM and detaching disk";
	  set_ignore_errors true;
	  change_vm_state cli vmid Shutdown;
	  set_ignore_errors false;
	  ignore(destroy_disk cli (vdi_uuid,newvbd))
  in
  
(*
  let disk_hotplug cdset =
    log Info "Starting CD hotplug test";
    log Info "Removing all currently attached CDs";
    remove_all_cds cli vmid;
    let cdattach (name,device,file) =
      ignore (attach_cd cli vmid name device false) in
    set_ignore_errors false; (* Catch errors on startup *)
    change_vm_state cli vmid Start;
(*    set_ignore_errors false;*)
    let rec doit n =
      if n=0 then () 
      else begin
	List.iter cdattach cdset;
	log Info "Attached cd(s)";
	let cdnames = List.map (fun (a,b,c) -> a) cdset in
	check_attached_cds cli vmid cdnames;
	log Info "Attached cd(s) verified by xapi";
	let cddevices = List.map (fun (a,b,c) -> b) cdset in
	let gacmd = "checkcd " ^ (String.concat " " cddevices) in
	let ip = get_client_ip vmid in
	let lines = run_ga_command ip gacmd in
	let lines = List.map String.lowercase lines in
	let results = List.map (fun (a,b,c) -> grep lines c) cdset in
	let results = List.flatten results in
	if List.length results <> List.length cdset 
	then
	  begin
	    log Error "CD test failed";
	    log Error "Expected results containing: %s" (String.concat "," (List.map (fun (a,b,c) -> c) cdset));
	  end;
	log Info "Test succeeded! Detaching all cds";
	remove_all_cds cli vmid;
	let lines = run_ga_command ip gacmd in
	let lines = List.map String.lowercase lines in
	let results = List.map (fun (a,b,c) -> grep lines c) cdset in
	let results = List.flatten results in
	if List.length results <> 0 then
	  begin
	    log Error "CD test failed";
	    log Error "Found non-null results when nothing was expected!"
	  end
      end
    in doit 100;
    log Info "Test succeeded! Shutting VM down";
    set_ignore_errors true;
    change_vm_state cli vmid Shutdown ;
    set_ignore_errors false
  in
*)
  
  let tests = 
    [("disk_test________________________",disk_test,"disk",
     "Attaches a formatted disk to a VM and checks that it can be read")] in
  
  List.iter (runtest cli (Testlog.GuestVerified domainname)) tests
    
    
let offline_disk (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in
  let test () =
    let disks = [("2","1"); (* CA-25864: trigger the backend to always round up *)
		 ("3","41943040");
		 ("4","83886080")] in
    let disks = List.filter (fun (d,_) -> (check_disk_ok cli vmid d)) disks in
    log Info "Test: VM NAME='%s'" domainname;
    log Info "Test: Adding/removing disks from stopped VM";
    log Info "Disk list: %s" (String.concat "," (List.map fst disks)); 
    let vdivbds = List.map (fun d -> add_disk cli vmid d) disks in
    List.iter (fun (vdi,vbd) -> log Info "Added vdi uuid=%s vbd uuid=%s" vdi vbd) vdivbds;
    let results = List.map (check_disk_size cli vmid) disks in
    let alltrue = List.fold_left (fun a b -> a && b) true results in
    if not alltrue then raise (Failure "Error adding disks!") else
      log Info "Disks all added correctly. Removing them!";
    List.iter (fun d -> ignore (destroy_disk cli d)) vdivbds;
    let results = List.map (check_disk_size cli vmid) disks in
    let allfalse = not (List.fold_left (fun a b -> a || b) false results) in
    if not allfalse then 
      log Error "Error removing disks!: %s" (String.concat " " (List.map (fun b -> if b then "t" else "f") results))
    else
      log Info "Disk test succeeded!"
    in
    let tests = 
    [("offline_disk_____________________",test,"disk",
     "Attaches up to 4 disks to the VM while offline, checks that they're reported to be attached, then removes them")] in
  List.iter (runtest cli (Testlog.OfflineVM domainname)) tests
    
let vif (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in
  log Info "Test: VM NAME='%s'" domainname;
  log Info "Test: Adding/removing VIFs from stopped VM";
  let nets = Cliops.get_networks cli in
  let test_net net = 
    log Info "Testing network %s" net;
    let vifs = [("1","11:22:33:44:55:66",net);
		("2","12:34:56:78:9A:BC",net);
		("3","98:76:54:32:10:00",net)] in
    let testfunc (name,mac,network) =
      log Info "Adding VIF to VM";
      let vifuuid = add_nic cli vmid (name,mac,network) in
      let (name',mac',network') = get_nic_params cli vifuuid in
      (if mac' <> mac || network' <> network then raise (Failure (Printf.sprintf "VIF test: expected (%s,%s) got (%s,%s)" network mac network' mac')));
      log Info "VIF added correctly. Removing";
      ignore(remove_nic cli vifuuid);
      (try let (_: string * string * string) = get_nic_params cli vifuuid in raise (Failure ("VIF "^name^" still present!")) with Failure x -> raise (Failure x));
    in
    List.iter testfunc vifs
  in 
  List.iter test_net nets;
  log Info "VIF test succeeded!"	

let online_vif (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in
  log Info "Test: VM NAME='%s'" domainname;
  log Info "Test: Online VIF test";
  let nets = Cliops.get_networks cli in
  let test_net net =
    log Info "Testing network %s" net;
    let vifs = [("1","11:22:33:44:55:66",net);
		("2","12:34:56:78:9A:BC",net);
		("3","98:76:54:32:10:00",net)] in
    let testfunc (name,mac,net) =
      log Info "Making sure VM is currently down";
      ensure_vm_down cli vmid 0;
      log Info "Adding VIF";
      let vifuuid = add_nic cli vmid (name,mac,net) in
      log Info "Powering up VM";
      change_vm_state cli vmid Start;      
      let result = run_ga_command (get_client_ip vmid) "checkvif" in
      let lines = grep result mac in
      if List.length lines <> 1 
      then raise (Failure ("Error, MAC not found in result! result was:"^(String.concat "\n" result)));
      change_vm_state cli vmid Shutdown;
      log Info "Removing VIF";
      ignore(remove_nic cli vifuuid)
    in
    List.iter testfunc vifs
  in
  List.iter test_net nets;
  log Info "Online VIF test succeeded!"

let offline_network (cli : Util.t_cli) vmid = 
  let domainname = get_param cli vmid "name-label" in
  if domainname="debian-pv" then
    begin
      log Info "Beginning network tests using debian-pv";
      
      Cliops.change_vm_state cli vmid Start;
      
      let network_create_destroy _ = 
	log Info "Test: Offline network test";
	Networks.network_create_destroy 100 cli vmid;
	log Info "Offline Network test succeeded!" in
      let vlan_create_destroy _ = 
	log Info "Test: Offline VLAN test";
	Networks.vlan_create_destroy 100 cli;
	log Info "Offline VLAN test succeeded!" in
      
      let tests = 
	[("net_create_destroy",network_create_destroy,"net",
	 "Repeatedly creates and destroys networks, checking that bridges are created and destroyed in dom0");
	 ("vlan_create_destroy", vlan_create_destroy, "net",
	 "Repeatedly creates and destroys PIFs with VLAN tags, checking that the right interfaces are being created and destroyed in dom0");
	] in
      
      List.iter (runtest cli Testlog.Other) tests
    end


let param (cli : Util.t_cli) vmid =
  let domainname = get_param cli vmid "name-label" in
  let test () =
    let params = [("name-description","Testing testing!");
		  ("user-version","100");
		  ("VCPUs-max","2");
		  ("PV-kernel","whatever");
		] in
    log Info "Test: VM NAME='%s'" domainname;
    log Info "Testing setting/resetting parameters on stopped VM";
    let testfunc (param,value) = 
      let before = get_param cli vmid param in
      let (_: string list) = set_param cli vmid param value in
      let after = get_param cli vmid param in
      let (_: string list) = set_param cli vmid param before in
      let before2 = get_param cli vmid param in
      (if after <> value then raise (Failure ("setting "^param^" to '"^value^"' failed - got '"^after^"'")));
      log Info "Param: %s - before='%s' after='%s' reset to '%s'" param before after before2
    in
    List.iter testfunc params;
    log Info "Parameter test succeeded!"
  in
  let tests = 
    [("offline_param____________________",test,"param",
      "Reads, sets, checks, resets, and verifies VM parameters")]
  in
  List.iter (runtest cli (Testlog.OfflineVM domainname)) tests



