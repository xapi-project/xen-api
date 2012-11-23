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
open Stringext
open Client
open Utils

(* --------------- iSCSI target VM helper functions --------------- *)
(* Copied and pasted largely from perftest/createVM.ml with some modifications. *)

let iscsi_vm_iso = "xenserver-iscsi-target.iso"
let iscsi_vm_template = "Other install media"
let oc_key = "perftestsetup"

let make_iscsi_ip ipbase =
  Printf.sprintf "192.168.%d.200" (ipbase+2)

let find_iscsi_iso rpc session_id =
    let vdis = Client.VDI.get_all rpc session_id in
    try
      Some (List.find (fun vdi -> Client.VDI.get_name_label rpc session_id vdi = iscsi_vm_iso) vdis)
    with _ -> None

let assert_sr_exists rpc session_id sr name =
  if sr = Ref.null then raise (Multipathrt_exceptions.Test_error (Printf.sprintf "%s is null" name));
  try let (_: API.sR_t) = Client.SR.get_record rpc session_id sr in () with _ -> raise (Multipathrt_exceptions.Test_error (Printf.sprintf "%s does not exist" name))

let make_iscsi rpc session_id iscsi_luns num_vifs sr_disk_size key network =
	let iscsi_iso = match find_iscsi_iso rpc session_id with
		| Some vdi -> vdi
		| None -> failwith "iSCSI VM iso not found" in
	let template = List.hd (Client.VM.get_by_name_label rpc session_id iscsi_vm_template) in
	let newvm = Client.VM.clone rpc session_id template "ISCSI target server" in
	try
		Client.VM.provision rpc session_id newvm;
		ignore_VBD(Client.VBD.create rpc session_id newvm iscsi_iso "0" true `RO `CD false false [] "" []);
		let realpool = List.hd (Client.Pool.get_all rpc session_id) in
		let defaultsr = Client.Pool.get_default_SR rpc session_id realpool in
		assert_sr_exists rpc session_id defaultsr "pool's default SR";

		for i = 0 to iscsi_luns - 1 do
			let storage_vdi_label = Printf.sprintf "SCSI VDI %d" i in
			let storage_vdi = Client.VDI.create rpc session_id storage_vdi_label "" defaultsr sr_disk_size `user false false [oc_key,key] [] [] [] in
			let userdevice = Printf.sprintf "%d" (i+1) in
			ignore_VBD(Client.VBD.create rpc session_id newvm storage_vdi userdevice false `RW `Disk false false [] "" [])
		done;

		Client.VM.set_PV_bootloader rpc session_id newvm "pygrub";
		Client.VM.set_HVM_boot_policy rpc session_id newvm "";

		for i = 0 to num_vifs - 1 do
			ignore (Client.VIF.create rpc session_id (string_of_int i) network newvm "" 1500L [oc_key,key] "" [] `network_default [] [] )
		done;

		Client.VM.add_to_other_config rpc session_id newvm oc_key key;
		Client.VM.start rpc session_id newvm false false;
		newvm
	with e ->
		debug "Caught exception with iscsi VM: %s" (Printexc.to_string e);
		debug "Trying to clean up iscsi VM...";
		(try Client.VM.destroy rpc session_id newvm with _ -> ());
		raise e

(* --------------- iSCSI SR probe helper functions --------------- *)
(* Copied and pasted from perftest/perfutil.ml *)

let parse_sr_probe_for_iqn (xml: string) : string list =
  match Xml.parse_string xml with
  | Xml.Element("iscsi-target-iqns", _, children) ->
      let parse_tgts = function
        | Xml.Element("TGT", _, children) ->
            let parse_kv = function
              | Xml.Element(key, _, [ Xml.PCData v ]) ->
                  key, String.strip String.isspace v (* remove whitespace at both ends *)
              | _ -> failwith "Malformed key/value pair" in
            let all = List.map parse_kv children in
            List.assoc "TargetIQN" all
        | _ -> failwith "Malformed or missing <TGT>" in
      List.map parse_tgts children
  | _ -> failwith "Missing <iscsi-target-iqns> element"

let parse_sr_probe_for_scsiids (xml : string) : string list =
  match Xml.parse_string xml with
  | Xml.Element("iscsi-target", _, children) ->
      let parse_luns = function
        | Xml.Element("LUN", _, children) ->
            let parse_kv = function
              | Xml.Element(key, _, [ Xml.PCData v ]) ->
                  key, String.strip String.isspace v (* remove whitespace at both ends *)
              | _ -> failwith "Malformed key/value pair" in
            let all = List.map parse_kv children in
            List.assoc "SCSIid" all
        | _ -> failwith "Malformed or missing <LUN>" in
      List.map parse_luns children
  | _ -> failwith "Missing <iscsi-target> element"

(* --------------- iSCSI target VM set-up functions --------------- *)

let setup_iscsi_vm rpc session iscsi_luns iscsi_vifs sr_disk_size =
  debug "Creating iSCSI target VM serving %d LUNs" iscsi_luns;
  let networks = Client.Network.get_all_records rpc session in
  let networks = List.filter (fun (_,r) -> r.API.network_bridge = "xenbr0") networks in
  match networks with
  | [] -> raise (Multipathrt_exceptions.Test_error "no networks found")
  | (network,_) :: _ -> make_iscsi rpc session iscsi_luns iscsi_vifs sr_disk_size "fred" network

let setup_iscsi_sr rpc session host iscsi_vm =
  debug "Getting one of the iSCSI VM's IP addresses";
  let vifs = Client.VM.get_VIFs rpc session iscsi_vm in
  let device = "0" in
  (* Find the VIF with device 0 (because, at present, that's the only one whose IP address is written to XenStore by the VM) *)
  let vifs = List.filter (fun vif -> Client.VIF.get_device rpc session vif = device) vifs in
  match vifs with
  | [] -> raise (Multipathrt_exceptions.Test_error "no VIFs found in iSCSI VM")
  | vif :: _ ->
      debug "Getting guest metrics for first VIF";
      let guest_metrics = Client.VM.get_guest_metrics rpc session iscsi_vm in
      if guest_metrics = Ref.null then raise (Multipathrt_exceptions.Test_error "Could not find VM_metrics object for iSCSI VM");
      debug "Reading networks from guest metrics";
      let networks = Client.VM_guest_metrics.get_networks rpc session guest_metrics in
      debug "Looking up IP address of first VIF";
      let ip = List.assoc (device^"/ip") networks in
      debug "IP address of first VIF is %s" ip;

      (* Probe it for IQNs *)
      let xml =
        try Client.SR.probe ~rpc ~session_id:session ~host ~device_config:["target",ip] ~sm_config:[] ~_type:"lvmoiscsi"
        with Api_errors.Server_error("SR_BACKEND_FAILURE_96",[a;b;xml]) -> xml
      in
      let iqns = parse_sr_probe_for_iqn xml in
      debug "Found %d IQNs" (List.length iqns);
      if iqns = [] then failwith "iSCSI target VM failed again - maybe you should fix it this time?";
      let iqn = List.hd iqns in

      (* Probe it for LUNs *)
      debug "Probing for LUNs with IQN %s" iqn;
      let xml =
        try Client.SR.probe ~rpc ~session_id:session ~host ~device_config:["target",ip; "targetIQN",iqn] ~sm_config:[] ~_type:"lvmoiscsi"
        with Api_errors.Server_error("SR_BACKEND_FAILURE_107",[a;b;xml]) -> xml
      in
    
      (* Create an SR for *only the first* LUN found *)
      let scsiids = parse_sr_probe_for_scsiids xml in
      debug "Found %d LUNs" (List.length scsiids);
      if scsiids = [] then failwith "Didn't find any SCSIids";
      let scsiid = List.hd scsiids in
    
      debug "Creating LVMoISCSI SR on LUN with SCSIid %s" scsiid;
      let sr = Client.SR.create ~rpc ~session_id:session ~host ~device_config:["target",ip; "targetIQN",iqn; "SCSIid",scsiid] ~physical_size:0L ~name_label:"iscsi" ~name_description:"" ~_type:"lvmoiscsi" ~content_type:"" ~shared:true ~sm_config:[] in
      (scsiid, sr)

let wait_for_vm_to_run rpc session ?(delay=60.0) vm =
  let session2 = Client.Session.login_with_password rpc !Globs.username !Globs.password "1.4" in
  Client.Event.register rpc session2 ["vm"];

  let finished = ref false in

  (* Create a thread to kill the session if we timeout *)
  let (_: Thread.t) = Thread.create (fun () ->
    Thread.delay delay;
    debug "Timer has expired (%.0f seconds); logging out session" delay;
    Client.Session.logout rpc session2) ()
  in

  let check record =
    let fin = record.API.vM_power_state = `Running in
    debug "Power state is now %s" (Record_util.power_to_string record.API.vM_power_state);
    finished := fin || !finished
  in

  check (Client.VM.get_record rpc session2 vm);
  debug "Entering wait loop...";

  try
    while not !finished do
      let events = Event_types.events_of_xmlrpc (Client.Event.next rpc session2) in
      debug "Got %d events..." (List.length events);
      let checkevent ev =
        match Event_helper.record_of_event ev with
        | Event_helper.VM (r,Some x) -> if r=vm then check x
        | _ -> debug "Got irrelevant event"
      in List.iter checkevent events
    done;
    debug "Finished waiting for event"
  with
  | Api_errors.Server_error("SESSION_INVALID", _) ->
      failwith (Printf.sprintf "Timed out while waiting for VM to run; maximum wait was %.1f seconds" delay)
  | e ->
      debug "Got unexpected exception: %s" (Printexc.to_string e);
      raise (Multipathrt_exceptions.Test_error (Printf.sprintf "received unexpected exception: %s" (Printexc.to_string e)))

(* Wait for a key like /local/domain/n/data/updated to appear in XenStore. This indicates that the iSCSI target VM has finished booting. *)
let wait_for_xenstore_key_to_appear rpc session ~delay vm =
  let vm_uuid = Client.VM.get_uuid rpc session vm in
  let host = Client.VM.get_resident_on rpc session vm in
  let rv = Client.Host.call_plugin ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn:"wait_for_iscsi_vm_boot" ~args:[("vm_uuid", vm_uuid); ("delay", Printf.sprintf "%f" delay)] in
  if rv <> "OK" then raise (Multipathrt_exceptions.Test_error (Printf.sprintf "Plugin failed while waiting for iSCSI target VM to boot: %s" rv))

let wait_for_iscsi_vm_to_boot rpc session ?(domain_up_delay=60.0) ?(boot_delay=180.0) ?(sleep_delay=3) vm =
  debug "Waiting for iSCSI target VM to start running...";
  wait_for_vm_to_run rpc session ~delay:domain_up_delay vm;
  debug "Waiting for iSCSI target VM to finish booting...";
  wait_for_xenstore_key_to_appear rpc session ~delay:boot_delay vm;
  debug "iSCSI target VM has booted";
  (* Give the guest agent a chance to create/update the VM_guest_metrics object *)
  debug "Sleeping for %d seconds..." sleep_delay;
  Unix.sleep sleep_delay;
  debug "Finished sleeping"
