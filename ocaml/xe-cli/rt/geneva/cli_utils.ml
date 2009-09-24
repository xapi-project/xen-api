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

(* constants *)
let user = "root"
let password = "xenroot"

let pwf_unix = "pwfile_unix.txt"
let pwf_windows = "pwfile_windows.txt"
let license_file = "license.txt"

let host = ref ""
let xe = ref "xe"

(* Time intervals in seconds: *)
let wait_timeout:float = 200.0 (* seconds *)
let poll_interval:int  = 2

(* Util functions for running CLI commands and collecting result codes
   and output *)

let print s =
  print_string s;
  flush(stdout)

let print_line s =
  print (s^"\n")

let run_command cmd =
  let ic = Unix.open_process_in cmd in
  let result : (string list) ref = ref [] in
  let read_str () =
    try
      while true do
	result := (!result) @ [(input_line ic)^"\n"];
      done
    with _ -> () in
  let _ = read_str() in
  let rc = Unix.close_process_in ic in
    (!result,rc)

type pwspec =
    | Password of string
    | PasswordFile of string

let cli_with_pwspec is_offhost cmd params pwspec =
  let rec mk_params l =
    match l with
	[] -> ""
      | ((k,v)::kvs) -> k^"=\""^v^"\""^" "^(mk_params kvs) in
  let param_str = mk_params params in
  let cli_base_string =
    (!xe)^" "^cmd
    ^(if is_offhost then
	  " -h "^(!host)
	  ^" "
	  ^(match pwspec with
		Password s -> "-u "^user^" -pw "^s
	      | PasswordFile s -> "-pwf "^s)
      else " -u "^user)
    ^" "^param_str in
    print_line ("Executing: "^cli_base_string);
    run_command cli_base_string

let cli_offhost_with_pwspec cmd params pwspec =
  cli_with_pwspec true cmd params pwspec

let cli_onhost cmd params =
  cli_with_pwspec false cmd params (Password "ignore")

    
let cli_offhost_with_pwd pwd cmd params =
  cli_offhost_with_pwspec cmd params (Password pwd)

let cli_offhost_with_pwf pwf cmd params =
  cli_offhost_with_pwspec cmd params (PasswordFile pwf)

let cli_offhost cmd params =
  cli_offhost_with_pwd password cmd params

exception TestFailed of string

let print_lines l = List.iter print l

let report_failure msg =
  print_line ("----------------------- FAILURE ---------------------\n");
  raise (TestFailed msg)

let is_success rc s =
  match rc with
      Unix.WEXITED i ->
	if i<>0 then
	  report_failure ("Expected rc==0; actual rc=="^(string_of_int i)
			  ^". cmd returned: "^(String.concat "" s))
    | _ -> ()
	
let expect_success f =
  let (s,rc) = f() in
    is_success rc s; s
      
let getval v line =
  let tks = tokenize line in
  let vs  = tokenize v in
  let rec domatch tokens values =
    match (tokens,values) with
	(t::_,[]) -> Some t
      | (t::ts,v::vs) ->
	  if t=v then domatch ts vs
	  else None
      | ([],_) -> None in
    domatch tks vs
	  
let rec mapopt f l =
  match l with
      [] -> []
    | (x::xs) ->
	match (f x) with
	    None -> mapopt f xs
	  | (Some y) -> y::(mapopt f xs)

exception OptionFailure
let rec getoptval x =
  match x with
      None -> raise OptionFailure
    | (Some x) -> x

exception VMNotFound of string
exception CLIOutputFormatError of string

let rec getstate cli vmid =
  let lines = expect_success (fun()->cli "host-vm-list" []) in
  let rec findstate ls =
    match ls with
	[] -> raise (VMNotFound vmid)
      | (l::ls) ->
	  begin
	    match (ls,getval "uuid:" l) with
		([],None) -> raise (VMNotFound vmid)
	      | ([],Some _) -> raise (CLIOutputFormatError "host-vm-list")
	      | (_,None) -> findstate ls
	      | (nextline::_,Some v) ->
		  if v=vmid then
		    begin
		      let s = getval "state:" nextline in
			if s=None then
			  raise (CLIOutputFormatError "host-vm-list")
			else getoptval s
		    end
		  else findstate ls
	  end in
    findstate lines

exception TimeOut

let poll f =
  let start_time = Unix.time() in
  let rec retry() =
    let current_time = Unix.time() in
      if (current_time -. start_time > wait_timeout) then
	raise TimeOut
      else
	if not (f()) then
	begin
	  Unix.sleep poll_interval;
	  retry()
	end in
    retry()

let startswith s1 s2 =
  (String.length s1)>=(String.length s2) &&
  (String.sub s1 0 (String.length s2))=s2

exception Last
let rec last l =
  match l with
      [] -> raise Last
    | [x] -> x
    | (x::xs) -> last xs

let read_end_from_output line_start lines =
  let new_uuids = mapopt
    (fun l->
       if (startswith l line_start) then Some (last (tokenize l))
       else None) lines in
    begin
      match new_uuids with
	  [x] -> x
	| _ ->
	    raise
	      (CLIOutputFormatError "can't find required parameter")
    end

(* Wait for specified vm to get into specified state *)      
let waitstate cli vmid state =
  print_line ("Waiting for vm "^vmid^" to get into state "^state);
  poll (fun ()->(getstate cli vmid)=state)

(* Get all vm uuids on host *)
let get_vm_uuids cli =
  let lines = expect_success (fun()->cli "host-vm-list" []) in
    mapopt (getval "uuid:") lines

(* Get all vm names on host *)
let get_vm_names cli =
  let lines = expect_success (fun()->cli "host-vm-list" []) in
    mapopt (getval "NAME:") lines

(* Get all disk names for specified vm *)
let get_disks cli vmid =
  let lines = expect_success (fun()->cli "vm-disk-list" [("vm-id",vmid)]) in
    mapopt (getval "name:") lines

(* Get all NICs for specified vm *)
let get_nics cli vmid =
  let lines = expect_success (fun()->cli "vm-vif-list" [("vm-id",vmid)]) in
    mapopt (getval "name:") lines

(* Get all vbridges on host *)
let get_vbridges cli =
  let lines = expect_success (fun()->cli "host-vbridge-list" []) in
  List.filter (fun s -> s <> "xapi0") (mapopt (getval "Virtual bridge:") lines)

(* Get all patches on host *)
let get_patches cli =
  let lines = expect_success (fun()->cli "host-patch-list" []) in
    mapopt (getval "uuid:") lines

(* Remove specified patch and check its gone *)
let sync_remove_patch cli uuid =
  ignore (expect_success (fun ()->cli "host-patch-remove" ["patch-id",uuid]));
  poll (fun ()->not (List.mem uuid (get_patches cli)))

(* Shutdown VM, returning when state=DOWN *)
let sync_shutdown cli force vmid =
  let params = if force then [("force","true")] else [] in
  let params = ("vm-id",vmid)::params in
    ignore (cli "vm-shutdown" params);
    waitstate cli vmid "DOWN"

(* Uninstall VM, returning when VM removed from host-vm-list *)
let sync_uninstall cli vmid =
  ignore (expect_success (fun ()->cli "vm-uninstall" [("vm-id",vmid)]));
  poll (fun ()->not (List.mem vmid (get_vm_uuids cli)))

(* Uninstall all VMs from unknown state, shutting them down first *)
let uninstall_all_vms cli =
  let uuids = get_vm_uuids cli in
    List.iter (sync_shutdown cli true) uuids;
    List.iter (sync_uninstall cli) uuids

(* Install guest, using specified template and wait until state==down *)
let install_guest cli (template, name) =
  let params = [("template-name",template);
		("name", name);
		("auto_poweron", "true");
		("vcpus", "1");
		("memory_set", "256")] in
  let lines = expect_success (fun ()->cli "vm-install" params) in
  let new_uuid = read_end_from_output "New VM" lines in
    waitstate cli new_uuid "DOWN";
    new_uuid

(* Add disk to VM and wait until it appears in list *)
let sync_add_disk cli vmid (diskname,disksize) =
  let params = [("vm-id",vmid);
		("disk-name",diskname);
		("disk-size",disksize)] in
    ignore (expect_success (fun ()->cli "vm-disk-add" params));
    poll (fun ()->
	    let vmdisks = get_disks cli vmid in
	      List.mem diskname vmdisks)

(* Remove disk from VM and wait until its gone from list *)
let sync_remove_disk cli vmid diskname =
  let params = [("vm-id",vmid);
		("disk-name",diskname)] in
    ignore (expect_success (fun ()->cli "vm-disk-remove" params));
    poll (fun ()->
	    let vmdisks = get_disks cli vmid in
	      not (List.mem diskname vmdisks))

(* Add NIC to VM and wait until it appears in list *)
let sync_add_nic cli vmid (nicname,mac,bridge) =
  let params = [("vm-id",vmid);
		("vif-name",nicname);
		("mac",mac);
		("bridge-name",bridge)] in
    ignore (expect_success (fun ()->cli "vm-vif-add" params));
    poll (fun ()->
	    let vmnics = get_nics cli vmid in
	      List.mem nicname vmnics)

(* Remove NIC from VM and wait until it has gone from list *)
let sync_remove_nic cli vmid nicname =
  let params = [("vm-id",vmid);
		("vif-name",nicname)] in
    ignore (expect_success (fun ()->cli "vm-vif-remove" params));
    poll (fun ()->
	    let vmnics = get_nics cli vmid in
	      not (List.mem nicname vmnics))

(* Add vbridge, returning when it appears in vbridge-list *)
let sync_add_vbridge cli bridge =
  let params = [("vbridge-name",bridge);
	        ("auto-vm-add","false")] in
    ignore (expect_success (fun ()->cli "host-vbridge-add" params));
    poll (fun ()->
	    let vbridges = get_vbridges cli in
	      List.mem bridge vbridges)

(* Remove vbridge, returning when it has gone from vbridge-list *)
let sync_remove_vbridge cli bridge =
  let params = [("vbridge-name",bridge)] in
    ignore (expect_success (fun ()->cli "host-vbridge-remove" params));
    poll (fun ()->
	    let vbridges = get_vbridges cli in
	      not (List.mem bridge vbridges))


(* Print report from *-list commands: *)
let print_report_output report_spec =
  List.iter
    (fun (sec, content) ->
       print_line (String.make 78 '*');
       print_line ("SECTION: "^sec);
       print_lines content)
    report_spec;
  print_line (String.make 78 '*');
  print_line ""

(* Collate info from VM list commands: *)
let print_vm_output cli vmid =
  let param  = [("vm-id",vmid)] in
  let output =
    [("disks",expect_success (fun ()->cli "vm-disk-list" param));
     ("vifs",expect_success (fun ()->cli "vm-vif-list" param));
     ("cds",expect_success (fun ()->cli "vm-cd-list" param));
     ("params",expect_success (fun ()->cli "vm-param-list" param))] in
    print_report_output output

(* Collate info from Host list commands: *)
let print_host_output cli =
  let output =
    [("licenses",expect_success (fun ()->cli "host-license-list" []));
     ("pifs",expect_success (fun ()->cli "host-pif-list" []));
     ("vbridges",expect_success (fun ()->cli "host-vbridge-list" []));
     ("pbridges",expect_success (fun ()->cli "host-pbridge-list" []));
     ("cpus",expect_success (fun ()->cli "host-cpu-list" []));
     ("srs",expect_success (fun ()->cli "host-sr-list" []));
     ("templates", expect_success (fun ()->cli "host-template-list" []));
     ("vms",expect_success (fun ()->cli "host-vm-list" []));
     ("params",expect_success (fun ()->cli "host-param-list" []));
     ("cds",expect_success (fun ()->cli "host-cd-list" []));
     ("patches",expect_success (fun ()->cli "host-patch-list" []))] in
    print_report_output output

let state_test cli vmid =
  let wait_print time =
    print_line ("Waiting for "^(string_of_int time)^" seconds...");
    Unix.sleep 50 in
  let param = [("vm-id",vmid)] in
  let move_state cmd rstate =
    ignore (expect_success (fun ()->cli cmd param));
    waitstate cli vmid rstate in
    begin
      move_state "vm-start" "UP";
      move_state "vm-suspend" "SUSPENDED";
      move_state "vm-resume" "UP"; 
      wait_print 5; (* there is a window where a VM doesn't see the shutdown signal *)
      move_state "vm-shutdown" "DOWN";
      move_state "vm-start" "UP";
      (* reboot and wait for VM to come back up *)
      ignore (expect_success
		(fun ()->cli "vm-reboot" param));
      waitstate cli vmid "UP";
      (* and try force shutdown... *)
      ignore (expect_success
		(fun ()->cli "vm-shutdown" (("force","true")::param)));
      waitstate cli vmid "DOWN"
    end

(* Clone VM and return new uuid *)
let clone_test cli vmid new_name =
  let params = [("vm-id",vmid);
	       ("new-name",new_name);
	       ("new-description","cloned with CLI regression test")] in
  let lines = expect_success (fun ()->cli "vm-clone" params) in
  let new_uuid = read_end_from_output "Cloned VM" lines in
    waitstate cli new_uuid "DOWN";
    new_uuid

(* Check that loglevel calls succeed *)
let loglevel_test cli =
  List.iter
    (fun x->
       ignore (expect_success
		 (fun ()->cli "host-loglevel-set" [("log-level",x)])))
    ["1";"2";"3";"4";"2"]    

(* Read specified param from specified vm, returning it as string *)
let getparam cli vmid param_name =
  let params = [("vm-id",vmid);
		("param-name",param_name)] in
  let lines = expect_success (fun ()->cli "vm-param-get" params) in
    match (mapopt (getval (param_name^":")) lines) with
	[] -> raise (CLIOutputFormatError "param get failure")
      | [x] -> x
      | _ -> raise (CLIOutputFormatError "multiple param get candidates")

(* Set specified parameter, read back and check it was set *)
let set_and_check_param cli vmid param_name param_value =
  let params = [("vm-id",vmid);
		("param-name",param_name);
		("param-value",param_value)] in
  let _ = ignore (expect_success (fun ()->cli "vm-param-set" params)) in
    poll (fun ()->(getparam cli vmid param_name)=param_value)

(* Resize disks *)
let resize_disk cli vmid disk_name new_size =
  let params = [("vm-id",vmid);
		("disk-name",disk_name);
		("disk-size",new_size)] in
    ignore (expect_success (fun ()->cli "vm-disk-resize" params))

(* Test host password *)
let test_password_set pswdcli =
  let setpwd params password =
    ignore
      (expect_success
	 (fun()->pswdcli password "host-password-set" params)) in
  let change_params = [("new-password","testpwd")] in
  let change_back_params = [("new-password",password)] in
    setpwd change_params password;
    setpwd change_back_params "testpwd"


(* Build param string from vmid, cdnname, cdlocation *)
let build_cd_params vmid cdname cdlocation =
  let params = [("vm-id",vmid);
		("cd-name",cdname)] in
    match cdlocation with
	(Some x) -> ("cd-location",x)::params
      | None -> params

(* Attach CD and check its there *)
let verify_attach_cd cli vmid cdname cdlocation =
  let params = build_cd_params vmid cdname cdlocation in
  let _ = expect_success (fun ()->cli "vm-cd-add" params) in
    poll
      (fun ()->
	 let lines = expect_success (fun ()->cli "vm-cd-list" params) in
	 let name = mapopt (getval "name:") lines in
	   match name with
	       [] -> false
	     | [x] ->
		 print_line x;
		 if cdname="empty" then (x="<empty")
		 else (x=cdname)
	     | _ -> raise (CLIOutputFormatError "cd list")
      )

(* Remove CD and check _NO CDs LEFT_ *)
let verify_remove_cd cli vmid cdname cdlocation =
  let params = build_cd_params vmid cdname cdlocation in
  let _ = expect_success (fun ()->cli "vm-cd-remove" params) in
    poll
      (fun ()->
	 let lines = expect_success (fun ()->cli "vm-cd-list" params) in
	 let names = mapopt (getval "name:") lines in
	   names=[])

(* CD: Attach, check, Remove, check *)
let cd_attach_remove cli vmid cdname cdlocation =
  verify_attach_cd cli vmid cdname cdlocation;
  verify_remove_cd cli vmid cdname cdlocation
      
let apply_license_to_server cli =
  expect_success (fun()->cli "host-license-add"
		    [("license-file",license_file)])
