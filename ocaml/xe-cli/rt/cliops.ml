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
(* Operations using the CLI *)

(* All of these should throw an exception if the operation fails.  *)
(* CliOpFailed reports the failed log of the cli command *)
exception CliOpFailed of string list
exception OpFailed of string

open Util
open Parsers
open Xstringext

let log=Testlog.log
let pic=ref ""

let is_success rc s =
  match rc with
      Unix.WEXITED i -> i=0
    | _ -> false

let expect_success f =
  let (s,rc) = f() in
  if is_success rc s then s else raise (CliOpFailed s)

let expect_success_retry f =
  let (s,rc) = f() in
  if is_success rc s then s else expect_success f

let expect_failure f =
  let (s,rc) = f() in
  if not (is_success rc s) then s else raise (CliOpFailed s)

let random_mac () =
        let macs = [0x00; 0x16; 0x3e] @ (List.map Random.int [0x80; 0x100; 0x100]) in
        String.concat ":" (List.map (Printf.sprintf "%02x") macs)
    
(* Mapping of vmid to ip *)
let ipmap = ref ( [] : (string * string) list)
let iface = ref "eth0"    
let use_gt = ref true
    
(* Operations using other programs *)
let get_client_domid vmid =
  try
    let (lines,rc) = run_command !Commands.list_domains in
    let lines=grep lines vmid in
    let line = List.hd lines in
    let domid = int_of_string (Parsers.zap_whitespace (String.sub line 0 3)) in
    domid
  with
    _ -> (-1) (* Can't find it - vm is down probably *)


let get_domid_state domid =
  try
    let (lines,rc) = run_command !Commands.list_domains in
    let dominfo = parse_ld lines in
    let domain = List.filter (fun (d,_,_) -> d=domid) dominfo in
    let (_,_,state) = List.hd domain in
    state
  with 
    _ -> "gone"
	
(* Guest operations *)
let run_ga_command ip command =
  expect_success_retry (fun () -> run_command (Printf.sprintf "%s %s %s" !Commands.guest_agent_client ip command))

let get_client_ip vmid =
  List.assoc vmid !ipmap

(* Xenstore ops *)
let read_xs_file path =
  try
    let (lines,rc) = run_command ("xenstore-read "^path) in
    lines
  with _ -> ["error"]
  
(* Specific XE things *)
let get_version (cli : Util.t_cli) =
  let host = List.hd (expect_success (fun () -> cli "pool-list" ["params","master";"minimal","true"])) in  
  let lines = expect_success (fun()->cli "host-param-get" ["param-name","software-version"; "uuid",host]) in
  List.hd lines

let get_short_version (cli : Util.t_cli) =
  let host = List.hd (expect_success (fun () -> cli "pool-list" ["params","master";"minimal","true"])) in  
  let lines = expect_success (fun()->cli "host-param-get" ["param-name","software-version"; "param-key","build_number"; "uuid",host]) in
  List.hd lines

let reset_xapi_log (cli : Util.t_cli) =
(*  (try Sys.remove "/tmp/xapi.log" with _ -> ());
  ignore (expect_success (fun () -> cli "log-set-output" ["output","nil"]));
  ignore (expect_success (fun () -> cli "log-set-output" ["output","file:/tmp/xapi.log"]))*) ()

let get_xapi_log (cli : Util.t_cli) =
  try
    let ic = open_in "/tmp/xapi.log" in
    let rec r lines =
      let nextline = 
        try Some (input_line ic) with _ -> None
      in match nextline with Some x -> r (x::lines) | None -> List.rev lines
    in r []
  with _ -> []
			     
let get_vms cli =
  let lines = expect_success (fun()->cli "vm-list" 
    ["params","name-label";
     "minimal","true"]) in
  String.split ',' (List.hd lines)

let get_networks cli =
  let lines = expect_success (fun()->cli "network-list" 
    ["params","uuid";
     "minimal","true"]) in
  String.split ',' (List.hd lines)

let get_state cli vmid =
  let lines = expect_success (fun()->cli "vm-param-get" 
    ["uuid",vmid; "param-name","power-state"]) in
  List.hd lines

let get_uuid cli name =
  let lines = expect_success (fun () -> cli "vm-list" 
    ["name-label",name; "params","uuid"; "minimal","true"]) in
  List.hd lines

let get_all_uuids cli =
  let lines = expect_success (fun () -> cli "vm-list" 
    ["params","uuid"; "minimal","true"]) in
  String.split ',' (List.hd lines)

let get_vm_names cli =
  let lines = expect_success (fun () -> cli "vm-list" 
    ["params","name-label"; "minimal","true"]) in
  String.split ',' (List.hd lines)

let get_disks cli vmid =
  let lines = expect_success (fun () -> cli "vbd-list" 
    [("vm-uuid",vmid); "params","device"; "minimal","true"]) in
  String.split ',' (List.hd lines)

let get_nics cli vmid =
  let lines = expect_success (fun () -> cli "vif-list" 
    [("vm-uuid",vmid); "params","uuid"; "minimal","true"]) in
  String.split ',' (List.hd lines)
  
let get_pifs cli =
  let lines = expect_success (fun () -> cli "pif-list" 
    ["params","uuid";"minimal","true"]) in
  String.split ',' (List.hd lines)

let get_pif_device cli uuid =
  let lines = expect_success (fun () -> cli "pif-list"
    ["params","device";"uuid",uuid;"minimal","true"]) in
  List.hd lines

let shutdown cli force vmid =
  let params = if force then [("force","true")] else [] in
  let params = ("vm",vmid)::params in
  expect_success (fun () -> cli "vm-shutdown" params)

let import cli filename =
  let params = [("filename",filename)] in
  expect_success (fun () -> cli "vm-import" params)

let export cli vmid filename =
  let params = [("vm",vmid);("filename",filename)] in
  expect_success (fun () -> cli "vm-export" params)

let clone cli vmid newname =
  let params = [("vm",vmid); ("new-name-label", newname)] in
  expect_success (fun () -> cli "vm-clone" params)
 
let uninstall cli vmid =
  expect_success (fun () -> cli "vm-uninstall" [("vm",vmid); ("force","true")])

let uninstall_all_vms cli =
  let uuids = get_all_uuids cli in
  List.iter (fun u -> ignore (shutdown cli true u)) uuids;
  List.iter (fun u -> ignore (uninstall cli u)) uuids
    
let install_guest cli (template,name) =
  let params = [("template-name",template);
		("name",name)] in
  let _ = expect_success (fun () -> cli "vm-install" params) in
  let new_uuid = get_uuid cli name in
  get_state cli new_uuid

let add_disk cli vmid (device,disksize) =
  let sruuid = List.hd (expect_success (fun () -> cli "pool-list"
    ["params","default-SR";"minimal","true"])) in
  let params = [("sr-uuid",sruuid);
		("name-label","test-disk");
		("type","user");
		("virtual-size",disksize);] in
  let vdiuuid=List.hd (expect_success (fun () -> cli "vdi-create" params)) in
  let vbduuid=List.hd (expect_success (fun () -> cli "vbd-create"
    ["vm-uuid",vmid; "vdi-uuid",vdiuuid; "device",device])) in
  (vdiuuid,vbduuid)

let attach_disk cli vmid diskuuid device =
  let params = [("vm-uuid",vmid);
		("vdi-uuid",diskuuid);
		("device",device)] in
  let uuid = List.hd (expect_success (fun () -> cli "vbd-create" params)) in
  (try ignore(cli "vbd-plug" ["uuid",uuid]) with _ -> ());
  uuid

let detach_disk cli vmid vbduuid =
  (try ignore(cli "vbd-unplug" ["uuid",vbduuid]) with _ -> ());
  ignore (expect_success (fun () -> cli "vbd-destroy" ["uuid",vbduuid]))

let attach_cd cli vmid vdiuuid device =
  let params = [("vm-uuid",vmid);
		("vdi-uuid",vdiuuid);
		("device",device);
		("mode","RO");
                ("type","CD")] in
  let uuid = List.hd (expect_success (fun () -> cli "vbd-create" params)) in
  (try ignore(cli "vbd-plug" ["uuid",uuid]) with _ -> ());
  uuid

let get_dom0 cli =
  let params = [("is-control-domain","true");
		("minimal","true")] in
  let dom0 = List.hd (expect_success (fun () -> cli "vm-list" params)) in
  dom0

let make_bootable cli vmid =
  let disks_to_try = ["hda"; "sda"] in
  let dodisk disk =
    let lines = expect_success( fun () -> cli "vbd-list"
      ["vm-uuid",vmid; "device",disk; "minimal", "true"]) in
    if List.length lines > 0 then
      let uuid = List.hd lines in
      ignore(expect_success (fun () -> cli "vbd-param-set"
	["uuid",uuid; "bootable","true"]))
  in
  List.iter dodisk disks_to_try
    
let get_attached_cds cli vmid =
  let lines = expect_success (fun () -> cli "vbd-list" [
    "vm-uuid",vmid;"type","CD";"minimal","true"]) in
  try
    let result =  String.split ',' (List.hd lines) in
    List.filter (fun s -> String.length s > 2) result
  with
      _ -> []

let detach_cd cli vbduuid =
  (try ignore(cli "vbd-unplug" ["uuid",vbduuid; "timeout","5.0"]) with _ -> ());
  ignore (expect_success (fun () -> cli "vbd-destroy" ["uuid",vbduuid]))

let get_vdi_uuid_from_name cli name =
  let lines = expect_success (fun () -> cli "vdi-list" ["name-label",name;
							"minimal","true"]) in
  List.hd lines

(* Check that the VM has each of the listed cds attached *)
let check_attached_cds cli vmid cdlist =
  let attached_cds = get_attached_cds cli vmid in
  let get_vdi uuid = List.hd (expect_success (fun () -> cli "vbd-param-get" [("uuid",uuid);("param-name","vdi-uuid")])) in
  let uuids = List.map get_vdi attached_cds in
  List.iter (fun cd -> if List.mem cd uuids then () else raise (Failure ("CD '" ^ cd ^ "' not found!"))) cdlist

let check_disk_ok cli vmid device =
  let lines = expect_success (fun () -> cli "vm-param-get" [("uuid",vmid);("param-name","allowed-VBD-devices")]) in
  let allowed_vbds = String.split ';' (List.hd lines) in
  let allowed_vbds = List.map (Parsers.zap_whitespace) allowed_vbds in
  List.mem device allowed_vbds
        
let check_disk cli vmid device =
  let lines = expect_success (fun () -> cli "vbd-list" [("vm-uuid",vmid);("userdevice",device); "minimal","true"]) in
  let lines2 = List.filter (fun x -> String.length x > 2) lines in
  List.length lines2 > 0

let detach_disk_if_present cli vmid device =
  try
    let uuid = List.hd (expect_success ( fun () -> cli "vbd-list" ["vm-uuid",vmid; "userdevice",device; "params","uuid"; "minimal","true"])) in
    ignore(detach_disk cli vmid uuid)
  with _ -> ()

let check_disk_size cli vmid (device,disksize) =
  try
    let lines = expect_success (fun () -> cli "vbd-list" [("vm-uuid",vmid); "userdevice",device;"params","vdi-uuid";"minimal","true"]) in
    let uuid = List.hd lines in
    let size = List.hd (expect_success (fun () -> cli "vdi-list" [("uuid",uuid); "params","virtual-size"; "minimal","true"])) in
    (Int64.of_string size) >= (Int64.of_string disksize)
  with
    _ -> false

let remove_all_cds cli vmid =
  let cds = get_attached_cds cli vmid in
  List.iter (detach_cd cli) cds;
  let cds = get_attached_cds cli vmid in
  if List.length cds <> 0 then raise (Failure "Cannot detach all CDs!") else ()

let destroy_disk cli (vdiuuid,vbduuid) =
  let params = [("uuid",vbduuid)] in
  ignore(expect_success (fun () -> cli "vbd-destroy" params));
  let params = [("uuid",vdiuuid)] in
  ignore(expect_success (fun () -> cli "vdi-destroy" params))

let add_network cli netname = 
  let params = [ ("name-label", netname) ] in
  List.hd (expect_success (fun () -> cli "network-create" params))
    
let remove_network cli netname = 
  let params = [ ("uuid", netname) ] in
  expect_success (fun () -> cli "network-destroy" params)
    
let add_nic cli vmid (nicname,mac,network) = 
  let params = [("vm-uuid",vmid);
		("device",nicname);
		("mac",mac);
		("network-uuid",network)] in
  List.hd (expect_success (fun () -> cli "vif-create" params))

let plug_nic cli uuid =
  expect_success (fun () -> cli "vif-plug" [("uuid",uuid)])

let unplug_nic cli uuid =
  expect_success (fun () -> cli "vif-unplug" [("uuid",uuid)])

let remove_nic cli uuid =
  let params = [("uuid",uuid)] in
  expect_success (fun () -> cli "vif-destroy" params)

let create_vlan cli (pif_uuid : string) (tag: string) (network_uuid: string) : string = 
  let params = [("pif-uuid", pif_uuid);
		("vlan", tag);
		("network-uuid", network_uuid) ] in
  List.hd (expect_success (fun () -> cli "vlan-create" params))

let remove_pif cli uuid = 
  let params = [("uuid", uuid)] in
  expect_success (fun () -> cli "vlan-destroy" params)

let get_nic_param cli uuid param =
  List.hd (expect_success (fun () -> cli "vif-param-get" [("uuid",uuid); ("param-name",param)]))

let get_nic_params cli uuid =
  let device = get_nic_param cli uuid "device" in
  let mac = get_nic_param cli uuid "MAC" in
  let network = get_nic_param cli uuid "network-uuid" in
  (device,mac,network)
		
let set_param cli vmid param_name value =
  let params = [("uuid",vmid);
		(param_name,value)] in
  expect_success (fun () -> cli "vm-param-set" params)

let get_param cli vmid param_name =
  let params = [("uuid",vmid);
		("param-name",param_name)] in
  List.nth (expect_success (fun () -> cli "vm-param-get" params)) 0

let is_currently_suspendable cli vmid =
  let allowed_ops = get_param cli vmid "allowed-operations" in
  List.length (Util.grep [allowed_ops] "suspend") = 1

let is_suspendable cli vmid =
  let param = get_param cli vmid "HVM-boot-policy" in
  param=""

let event_wait cli vmid cls params timeout =
  let params = [("class",cls); ("uuid",vmid)]@params in
  let cmdargs = String.concat " " (List.map (fun (a,b) -> a^"="^b) params) in
  match run_command_with_timeout ("xe event-wait "^cmdargs) timeout with
      Some (lines,rc) -> ()
    | None -> raise (CliOpFailed (["Timeout waiting for event";cmdargs]))

(** Higher level operations *)

type vmop = Start | Shutdown | Reboot | Resume | Suspend

(* Call this when you expect the operations to fail *)
let change_vm_state_fail cli vmid st =
  let params = [("vm",vmid)] in
  ignore (match st with
    Start ->
      expect_failure (fun () -> cli "vm-start" params)
  | Shutdown ->
      expect_failure (fun () -> cli "vm-shutdown" params)
  | Suspend ->
      expect_failure (fun () -> cli "vm-suspend" params)
  | Reboot ->
      expect_failure (fun () -> cli "vm-reboot" params)
  | Resume -> 
      expect_failure (fun () -> cli "vm-resume" params))

let get_pic cli vmid =
  let domid = get_client_domid vmid in
  let domainname = get_param cli vmid "name-label" in
  let is_pv = String.endswith "pv" domainname in
  log Log.Info "Obtaining vnc snapshot for vm %s from domain %d" domainname domid;
  (if not is_pv then
    ignore (expect_success (fun () -> run_command (Printf.sprintf "vncsnapshot localhost:%d /tmp/pic.jpg" (domid)))));
  pic := "/tmp/pic.jpg"

(** True if a vm can clean shutdown *)
let can_clean_shutdown cli vmid =
  let bootpolicy = expect_success (fun () -> cli "vm-param-get" [("uuid",vmid); ("param-name","HVM-boot-policy")]) in
  let is_pv = List.hd bootpolicy <> "BIOS order" in
  let other = expect_success (fun () -> cli "vm-param-get" [("uuid",vmid); ("param-name","other")]) in
  (is_pv) || (List.length (grep other "feature-shutdown: 1") > 1)

(* ping the guest agent *)
exception GAFailed
    
let rec ping ip attempts =
  begin
    if attempts > 500 then raise GAFailed;
    try
      ignore(run_ga_command ip "test");
    with _ -> 
      Unix.sleep 1;
      ping ip (attempts+1)
  end


(** Wait for a VM to finish booting, and get its IP address *)
let wait_for_up cli vmid =
  try
    let nic = List.hd (get_nics cli vmid) in
    let mac = get_nic_param cli nic "MAC" in
    let iface = !iface in (* for now *)
    let command = (Printf.sprintf "xgetip %s %s" iface mac) in
    let lines = 
      match run_command_with_timeout command 120.0 with
	  Some (lines,rc) -> lines
	| None ->
	    get_pic cli vmid;
	    raise (CliOpFailed ["Timeout!"]) in
    let ip = List.hd lines in
    ipmap := (vmid,ip)::!ipmap;
    ping ip 0   
  with
      CliOpFailed ls ->
	get_pic cli vmid;
	log Log.Warn "wait_for_up: IP address not obtained. Message returned was:";
	List.iter (fun l -> log Log.Warn "%s" l) ls
    | GAFailed ->
	get_pic cli vmid;
	log Log.Warn "wait_for_up: Guest agent failed to start";
    | e -> 
	get_pic cli vmid;
	log Log.Warn "wait_for_up: Failed to get IP address - got exception %s" (Printexc.to_string e)
				
(** Wait for a shutdown operation to complete. Returns true if xapi believes the domain shut down or false if it didn't *)
let shutdown_wait cli vmid =
  try
    let domid = get_client_domid vmid in
    event_wait cli vmid "vm" ["power-state","halted"] 120.0;
    let xapistate = get_state cli vmid in
    let domstate = get_domid_state domid in
    if domstate <> "gone" && xapistate <> "halted" then log Log.Error "Domain still up after shutdown! xapistate=%s xenstate=%s" xapistate domstate;
    true
  with
      _ -> false

(** Wait for a reboot operation to complete. Returns true if xapi believes the domain rebooted or false if it didn't *)
let reboot_wait cli vmid = 
  try
    let start_time = get_param cli vmid "start-time" in
    event_wait cli vmid "vm" ["start-time", "/=" ^ start_time] 120.0;
    let xapistate = get_state cli vmid in
    let domid = get_client_domid vmid in
    let domstate = get_domid_state domid in
    if domstate = "gone" || xapistate = "halted" then log Log.Error "Domain went down after reboot! xapistate=%s xenstate=%s" xapistate domstate;
    true
  with
      _ -> false

type run_command_output = string list * Unix.process_status	

(** Shutdown hard *)
let shutdown_phase3 cli vmid =
  try
    let params=[("vm",vmid);("force","true")] in
    ignore(expect_success (fun () -> cli "vm-shutdown" params))
  with
    CliOpFailed ls ->
      log Log.Warn "shutdown_phase3: Shutdown failed - cli reported:";
      List.iter (fun l -> log Log.Error "%s" l) ls;
      let (_: run_command_output) = run_command !Commands.list_domains in
      raise (OpFailed "Shutdown failed!")
  | e ->
      log Log.Warn "shutdown_phase2: Exception caught: %s" (Printexc.to_string e);
      raise (OpFailed "Shutdown failed!")

(** Shutdown clean *)
let shutdown_phase2 cli vmid =
  let params=[("vm",vmid)] in
  let next () = shutdown_phase3 cli vmid in
  try
    if can_clean_shutdown cli vmid then
      begin
	ignore(expect_success (fun () -> cli "vm-shutdown" params));
	if shutdown_wait cli vmid 
	then ()
	else next ()
      end
    else next ()
  with
    CliOpFailed ls ->
      log Log.Warn "shutdown_phase2: Shutdown failed - cli reported:";
      List.iter (fun l -> log Log.Error "%s" l) ls;
      let (_: run_command_output) = run_command !Commands.list_domains in
      next ()
  | (OpFailed x) as e ->
      raise e
  | e ->
      log Log.Warn "shutdown_phase2: Exception caught: %s" (Printexc.to_string e);
      next ()
	
(** Use the guest agent to shutdown the domain *)
let shutdown_phase1 cli vmid =
  let next () = shutdown_phase2 cli vmid in
  if not !use_gt then next () else
  try
    if List.mem_assoc vmid !ipmap 
    then 
      begin
	ignore(run_ga_command (List.assoc vmid !ipmap) "shutdown 10");
	if shutdown_wait cli vmid
	then ()
	else next ()
      end
    else
      begin
	log Log.Warn "shutdown_phase1: Use of guest agent requested, but IP address for VM is not known";
	next ()
      end	
  with 
    (OpFailed x) as e ->
      let (_: run_command_output) = run_command !Commands.list_domains in
      raise e
  | e ->
      log Log.Warn "shutdown_phase1: Exception caught: %s" (Printexc.to_string e);
      next ()

(** Try to reboot via the cli *)
let reboot_phase2 cli vmid =
  try
    let params = [("vm",vmid)] in
    ignore(expect_success (fun () -> cli "vm-reboot" params));
(*    shutdown_wait cli vmid; -- vm-reboot only returns when the old domain has been destroyed
    event_wait cli vmid "vm" ["power-state","running"] 10.0; *)
    wait_for_up cli vmid
  with
    CliOpFailed ls ->
      log Log.Error "reboot_phase2: Reboot failed: cli reported:";
      List.iter (fun l -> log Log.Error "%s" l) ls;
      let (_: run_command_output) = run_command !Commands.list_domains in
      raise (OpFailed "Failed to reboot")
  | e ->
      log Log.Error "reboot_phase2: Reboot failed: exception caught: %s" (Printexc.to_string e);
      raise (OpFailed "Failed to reboot")
	 
(** Reboot via the guest agent *)
let reboot_phase1 cli vmid =
  let next () = reboot_phase2 cli vmid in
  if not !use_gt then next () else
  try
    if List.mem_assoc vmid !ipmap 
    then 
      begin
	ignore(run_ga_command (List.assoc vmid !ipmap) "reboot 10");
	(* Guest powerstate nolonger glitches to Halted in the middle of a reboot *)
	let (_: bool) = reboot_wait cli vmid in
	wait_for_up cli vmid;
      end	
    else
      begin
	log Log.Warn "reboot_phase1: Use of guest agent requested, but no IP address for VM available!";
	next ()
      end
  with 
    | (OpFailed x) as e -> (* next might have raise this, in which case, pass it through *)
	let (_: run_command_output) = run_command !Commands.list_domains in
	raise e
    | CliOpFailed ls ->
	log Log.Warn "reboot_phase1: Cli op failed: %s" (String.concat "; " ls);
	next ()
    | e ->
	log Log.Warn "reboot_phase1: Exception caught: %s" (Printexc.to_string e);
	next ()

let change_vm_state cli vmid st =
  let params = [("vm",vmid)] in
  let domid = get_client_domid vmid in
  ignore begin
    match st with
      Start -> 
	begin
	  try
	    let (_: string list) = expect_success (fun () -> cli "vm-start" params) in
	    let domid = get_client_domid vmid in
	    log Log.Info "New domid: %d" domid;
	    log Log.Info "Waiting for VM to start...";
	    wait_for_up cli vmid;
	  with 
	    CliOpFailed ls ->
	      log Log.Error "change_vm_state: VM start failed: cli reported:";
	      List.iter (fun l -> log Log.Error "%s" l) ls;
	      raise (OpFailed "Failed to start VM")
	end
    | Shutdown -> 
	shutdown_phase1 cli vmid;
	(try ipmap := List.remove_assoc vmid !ipmap with _ -> ())
    | Suspend -> 
	begin
	  try
	    ignore (expect_success (fun () -> cli "vm-suspend" params))
	  with
	    CliOpFailed ls ->
	      log Log.Error "change_vm_state: VM suspend failed: cli reported:";
	      List.iter (fun l -> log Log.Error "%s" l) ls;
	      let (_: run_command_output) = run_command !Commands.list_domains in
	      raise (OpFailed "Failed to suspend VM")
	end
    | Reboot -> 
	(* All waiting for up etc moved to this func to allow it to try other methods if the first failed *)
	reboot_phase1 cli vmid;
    | Resume -> 
	begin
	  try
	    ignore (expect_success (fun () -> cli "vm-resume" params));
	    ping (List.assoc vmid !ipmap) 0
	  with
	      CliOpFailed ls ->
		log Log.Error "change_vm_state: VM resume failed: cli reported:";
		List.iter (fun l -> log Log.Error "%s" l) ls;
		raise (OpFailed "Failed to resume VM")
	    | GAFailed -> 
		log Log.Error "change_vm_state: VM resume failed: failed to contact guest agent";
		raise (OpFailed "Failed to resume VM")		  
	end;
  end;
  log Log.Info "new state: %s " (get_state cli vmid);
  if domid>0
  then
    log Log.Info "Xen state (domid %d): %s" domid (get_domid_state domid)



(* This does its best to shutdown a VM. Tries several things and fails after 5 times *)
(* Almost certainly overkill, but let's be cautious! *)
let rec ensure_vm_down cli vmid count =
  log Log.Info "Doing everything possible to turn off VM";
  if count > 5 then raise (OpFailed "Poweroff operation failed!") else
  let params = [("vm",vmid)] in
  match get_state cli vmid with
    "halted" -> ()
  | "paused" ->
      log Log.Warn "Host is currently paused! Unpausing and attempting shutdown";
      (try 
	ignore (expect_success (fun () -> cli "vm-unpause" params));
	change_vm_state cli vmid Shutdown;
      with _ -> ());
      ensure_vm_down cli vmid (count+1)
  | "running" -> 
      log Log.Warn "Host is currently running! Shutting down";
      (try
	change_vm_state cli vmid Shutdown;
	with _ -> ());
      ensure_vm_down cli vmid (count+1)
  | "suspended" ->
      log Log.Warn "Host is currently suspended! Resuming and shutting down!";
      (try
	ignore (expect_success (fun () -> cli "vm-resume" params));
	change_vm_state cli vmid Shutdown;
      with _ -> ());
      ensure_vm_down cli vmid (count+1)
  | "shutting down" ->
      (try
	change_vm_state cli vmid Shutdown;
      with _ -> ());
      ensure_vm_down cli vmid (count+1)
  | "migrating" ->
      raise (OpFailed "Host is corrently migrating!")
  | _ ->
      raise (OpFailed "Host is in an unknown state!")


let fail_if_state_isnt cli vmid state =
  let state2 = get_state cli vmid in
  if state2 <> state then raise (OpFailed ("VM is not in required state! (state=" ^ state2 ^ ", should be "^state^")"))

