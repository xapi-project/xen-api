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
(** 
 * @group Command-Line Interface (CLI)
 *)
 
(* Backwards compatible CLI operations *)

(* These are mostly list functions - operations that do things *)
(* are mostly implemented in Cli_operations.ml, which 'compat' *)
(* mode checks                                                 *)

open Cli_protocol
open Cli_util
open Cli_cmdtable
open Stringext
open Pervasiveext
open Unixext
open Client
open Records 

module D=Debug.Debugger(struct let name="cli" end)
open D

let failwith str = raise (Cli_util.Cli_failure str)

(* Conversion from Rio -> Geneva/Zurich *)
let powerstate vm_r =
  let co = vm_r.API.vM_current_operations in
  let s = vm_r.API.vM_power_state in
  let op_to_string (_,s) = 
    match s with 
      | `clone -> "CLONING"
      | `start -> "STARTING"
      | `clean_shutdown -> "SHUTTING_DOWN"
      | `hard_shutdown ->  "SHUTTING_DOWN"
      | `clean_reboot -> "REBOOTING"
      | `hard_reboot -> "REBOOTING"
      | `suspend -> "SUSPENDING"
      | `resume -> "RESUMING"
      | `export -> "EXPORTING"
      | _ -> ""
  in
  let state = String.concat " " (List.map op_to_string co) in
  if state <> "" then state else
    match s with
      | `Halted -> "DOWN"
      | `Paused -> "PAUSED"
      | `Running -> "UP"
      | `Suspended -> "SUSPENDED"

(* Functions to get objects from the database in convenient ways *)

let get_vm_records rpc session_id =
  let vms = Client.VM.get_all_records_where rpc session_id "true" in
  let vmrs = List.map snd vms in
  List.filter (fun vm_r -> not vm_r.API.vM_is_a_template) vmrs 

let get_template_records rpc session_id =
  let vms = Client.VM.get_all_records_where rpc session_id "true" in
  let vmrs = List.map snd vms in
  List.filter (fun vm_r -> vm_r.API.vM_is_a_template) vmrs 

let get_patch_by_name_or_id rpc session_id params =
  let patches = Client.Host_patch.get_all_records_where rpc session_id "true"in
  let filter_fun =
    if List.mem_assoc "patch-name" params 
    then 
      let name = List.assoc "patch-name" params in
      function (_, x) -> (x.API.host_patch_name_label = name)
    else
      if List.mem_assoc "patch-id" params 
      then
	let id = List.assoc "patch-id" params in
	function (_, x) -> (x.API.host_patch_uuid = id) 
    else raise (Failure "Either a patch-name or a patch-id must be specified")
  in
  let patches = List.filter filter_fun patches in
  if List.length patches = 0 then raise (Failure ("Patch not found"));
  List.nth patches 0

(* Operations *)
let not_implemented printer rpc session_id params =
  printer (Cli_printer.PList ["Not implemented."])


let host_vm_list printer rpc session_id params =
  let vms = get_vm_records rpc session_id in
  let vms = List.filter (fun vm_r -> not (vm_r.API.vM_is_a_template)) vms in
  let vms = List.filter (fun vm_r -> not vm_r.API.vM_is_control_domain) vms in
  let to_print_record vm_r =
    [("NAME",vm_r.API.vM_name_label);
     ("uuid",vm_r.API.vM_uuid);
     ("state",powerstate vm_r)] in
  printer (Cli_printer.PTable (List.map to_print_record vms))

let host_template_list printer rpc session_id params =
  let vms = get_template_records rpc session_id in
  let templates = List.filter (fun vm_r -> vm_r.API.vM_is_a_template) vms in
  let to_print_record vm_r =
    vm_r.API.vM_name_label in
  printer (Cli_printer.PList (List.map to_print_record templates))

let host_sr_list printer rpc session_id params =
  let srs = Client.SR.get_all rpc session_id in
  let srs = List.map (fun sr -> (sr,Client.SR.get_record rpc session_id sr)) srs in
  let recs = 
    List.map
      (fun (_,sr) ->
	["NAME",sr.API.sR_name_label;
	 "uuid",sr.API.sR_uuid;
	 "active","true";
	 "devices","<unknown>"]) srs
  in
  printer (Cli_printer.PTable recs)
    
let host_param_list printer rpc session_id params =
  let host = Cli_operations.get_host_from_session rpc session_id in
  let host_r = Client.Host.get_record rpc session_id host in
  let recs =
    [["Name",host_r.API.host_name_label;
      "Xen version","<unknown>";
      "Installed","<unknown>";
      "Product version/build",
     (List.assoc "product_version" host_r.API.host_software_version)^"/"^
       (List.assoc "build_number" host_r.API.host_software_version);
      "Sockets per node",string_of_int (List.length host_r.API.host_host_CPUs);
      "Cores per socket","<unknown>";
      "Threads per core","<unknown>"]] in
  printer (Cli_printer.PTable recs)

let host_password_set _ rpc session_id params = 
  let old_pwd = List.assoc "password" params
  and new_pwd = List.assoc "new-password" params in
  Client.Session.change_password rpc session_id old_pwd new_pwd

let host_shutdown = Cli_operations.host_shutdown

let host_reboot = Cli_operations.host_reboot

let host_pif_list printer rpc session_id params =
  let host = Cli_operations.get_host_from_session rpc session_id in
  let host_r = Client.Host.get_record rpc session_id host in
  let pifs = host_r.API.host_PIFs in
  let pif_rs = List.map (fun pif -> Client.PIF.get_record rpc session_id pif) pifs in
  let pif_to_record pif_r =
    ["Interface",pif_r.API.pIF_device;
     "IP address","<unknown>";
     "Subnet mask","<unknown>"] in
  printer (Cli_printer.PTable (List.map pif_to_record pif_rs))

let host_cpu_list printer rpc session_id params =
  let this_host = Cli_operations.get_host_from_session rpc session_id in
  let cpus = Client.Host.get_host_CPUs rpc session_id this_host in
  let recs = List.map
      (fun cpu ->
	let cpu = Client.Host_cpu.get_record rpc session_id cpu in
	[("CPU"^(Int64.to_string cpu.API.host_cpu_number),cpu.API.host_cpu_modelname);
	 ("Vendor",cpu.API.host_cpu_vendor);
	 ("Speed",Int64.to_string cpu.API.host_cpu_speed)]) cpus in
  printer (Cli_printer.PTable recs)

let host_cd_list printer rpc session_id params =
  let srs = Client.SR.get_all_records_where rpc session_id "true" in
  let cd_srs = List.filter (fun (sr,sr_record) -> sr_record.API.sR_content_type = "iso") srs in
  let cd_vdis = List.flatten (List.map (fun (sr,sr_record) -> Client.SR.get_VDIs rpc session_id sr) cd_srs) in
  let records = List.map
      (fun vdi ->
	let vdi_rec = Client.VDI.get_record rpc session_id vdi in
	[("CD",vdi_rec.API.vDI_name_label);
	 ("Description",vdi_rec.API.vDI_name_description);
	 ("Location","<unknown>")]) cd_vdis in
  printer (Cli_printer.PTable records)  

let host_patch_list printer rpc session_id params = 
  let patches = Client.Host_patch.get_all_records_where rpc session_id "true" in
  let records = List.map
    (fun (patch, patch_rec) ->
       [("PATCH", patch_rec.API.host_patch_name_label);
	("uuid", patch_rec.API.host_patch_uuid);
	("applied on", if patch_rec.API.host_patch_applied 
	 then Date.to_string patch_rec.API.host_patch_timestamp_applied
	 else "never")]) patches in
  printer (Cli_printer.PTable records)

let host_patch_remove printer rpc session_id params = 
  let p = get_patch_by_name_or_id rpc session_id params in
  Client.Host_patch.destroy rpc session_id (fst p)

let host_patch_upload fd printer rpc session_id params = 
  let name = List.assoc "patch-file" params in
  Cli_operations.patch_upload fd printer rpc session_id [ "file-name", name ]

let host_patch_apply printer rpc session_id params = 
  let p = get_patch_by_name_or_id rpc session_id params in
  Cli_operations.patch_apply printer rpc session_id [ "uuid", (snd p).API.host_patch_uuid ]

let get_disks rpc session_id vm =
  let vbds = Client.VM.get_VBDs rpc session_id vm in
  List.map (fun vbd -> (Client.VBD.get_record rpc session_id vbd,
		       Client.VDI.get_record rpc session_id (Client.VBD.get_VDI rpc session_id vbd))) vbds 
    
let vm_disk_list printer rpc session_id params =
  let op vm =
    let vm = vm.getref () in
    let disks = get_disks rpc session_id vm in
    (* Filter out cds *)
    let disks = List.filter (fun (vbd_r,vdi_r) -> Client.SR.get_content_type rpc session_id vdi_r.API.vDI_SR <> "iso") disks in
    let disk_to_rec (vbd_r,vdi_r)=
      [("name",vbd_r.API.vBD_userdevice);
       ("size",Int64.to_string (Int64.shift_right vdi_r.API.vDI_virtual_size 20));
       ("min_size","<unknown>");
       ("function","<unknown>");
       ("qos_value","<unknown>")] in
    printer (Cli_printer.PTable (List.map disk_to_rec disks))
  in
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])

let vm_disk_setqos printer rpc session_id params =
  let device = List.assoc "disk-name" params in
  let qos = List.assoc "disk-qos" params in
  let op vm =
    let vm = vm.getref () in
    let vbds = Client.VM.get_VBDs rpc session_id vm in
    try
      let vbd = List.find (fun vbd -> device = Client.VBD.get_device rpc session_id vbd) vbds in
      Client.VBD.set_qos_algorithm_type rpc session_id vbd "ionice";
      (* remove the key if it's already there *)
      (try
	  Client.VBD.remove_from_qos_algorithm_params rpc session_id vbd "class";
	with _ -> ());
      Client.VBD.add_to_qos_algorithm_params rpc session_id vbd "class" qos
    with
	_ -> failwith "Disk not found"
  in
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])


let vm_cd_list printer rpc session_id params =
  let op vm =
    let vm_record = vm.record () in
    let disks = List.map (fun vbd -> (Client.VBD.get_record rpc session_id vbd,Client.VDI.get_record rpc session_id (Client.VBD.get_VDI rpc session_id vbd))) vm_record.API.vM_VBDs in
    (* Filter out disks *)
    let disks = List.filter (fun (vbd_r,vdi_r) -> Client.SR.get_content_type rpc session_id vdi_r.API.vDI_SR = "iso") disks in
    let disk_to_rec (vbd_r,vdi_r)=
      [("name",vdi_r.API.vDI_name_label)]
    in
    printer (Cli_printer.PTable (List.map disk_to_rec disks))
  in
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])

let get_vm_params rpc session_id vm =
  let vm_record = Records.vm_record rpc session_id vm in
  let power_state = powerstate (Client.VM.get_record rpc session_id vm) in
  let vcpus = 
    if power_state="running" || power_state="suspended" then
      Records.safe_get_field (Records.field_lookup vm_record.Records.fields "VCPUs-number")
    else
      Records.safe_get_field (Records.field_lookup vm_record.Records.fields "VCPUs-max")
  in
  (* memory_set refers to the balloon target, memory_max is the boot-time max *)  
  let memory_set = Records.safe_get_field (Records.field_lookup vm_record.Records.fields "memory-dynamic-max") in
  let vcpu_params = 
    try 
      match (Records.field_lookup vm_record.Records.fields "VCPUs-params").Records.get_map with Some f -> f () | None -> []
    with _ -> []
  in
  let memory_set_mb = Int64.shift_right (Int64.of_string memory_set) 20 in
  [("name",Records.safe_get_field (Records.field_lookup vm_record.Records.fields "name-label"));
   ("description",Records.safe_get_field (Records.field_lookup vm_record.Records.fields "name-description"));
   ("distribution","<unknown>");
   ("distribution_vsn","<unknown>");
   ("os","<unknown>");
   ("vcpus",vcpus);
   ("memory_set",Int64.to_string memory_set_mb);
   ("boot_params",let x = Records.safe_get_field (Records.field_lookup vm_record.Records.fields "PV-args") in if x="" then "(null)" else x);
   ("sched_credit_weight",(try List.assoc "weight" vcpu_params with _ -> ""));
   ("sched_credit_cap",(try List.assoc "cap" vcpu_params with _ -> ""));
   ("vcpu_pin","<unknown>");
   ("force_hvm","false");
   ("on_crash",(String.lowercase (Record_util.on_crash_behaviour_to_string (Client.VM.get_actions_after_crash rpc session_id vm))))]

let vm_param_list printer rpc session_id params =
  let op vm =
    let vm = vm.getref () in
    let params = get_vm_params rpc session_id vm in
    printer (Cli_printer.PTable [params])
  in
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])

let vm_param_get printer rpc session_id params =
  let op vm = 
    let vm=vm.getref () in
    let allparams = get_vm_params rpc session_id vm in
    let param_name=List.assoc "param-name" params in
    let result = 
      try
	List.filter (fun (k,v) -> k=param_name) allparams
      with
	  _ -> ["error","Parameter not found"]
    in
    printer (Cli_printer.PTable [result])
  in
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])

let vm_param_set printer rpc session_id params =
  let op vm =
    let vm=vm.getref () in
    let param = List.assoc "param-name" params in
    let value = List.assoc "param-value" params in
    match param with
      | "name" -> Client.VM.set_name_label rpc session_id vm value 
      | "description" -> Client.VM.set_name_description rpc session_id vm value
      | "vcpus" -> Client.VM.set_VCPUs_at_startup rpc session_id vm (Int64.of_string value) 
      | "memory_set" -> 
	  if Client.VM.get_power_state rpc session_id vm <> `Halted
	  then failwith "Cannot modify memory_set when VM is running";
	  let bytes = Int64.shift_left (Int64.of_string value) 20 in
	  Client.VM.set_memory_dynamic_max rpc session_id vm bytes;
	  Client.VM.set_memory_dynamic_min rpc session_id vm bytes
      | "auto_poweron" -> ()
      | "boot_params" -> Client.VM.set_PV_args rpc session_id vm value
      | "on_crash" -> Client.VM.set_actions_after_crash rpc session_id vm (Record_util.string_to_on_crash_behaviour value)
      | "sched_credit_weight" -> 
	  (try Client.VM.remove_from_VCPUs_params rpc session_id vm "weight" with _ -> ());
	  Client.VM.add_to_VCPUs_params rpc session_id vm "weight" value
      | "sched_credit_cap" ->
	  (try Client.VM.remove_from_VCPUs_params rpc session_id vm "cap" with _ -> ());
	  Client.VM.add_to_VCPUs_params rpc session_id vm "cap" value
      | _ -> failwith "Unknown parameter"
  in
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])
  
let vm_install fd printer rpc session_id params =
  let template_name = List.assoc "template-name" params in
  let name = List.assoc "name" params in
  let description = try List.assoc "description" params with _ -> "" in
  let vcpus = try Some (Int64.of_string (List.assoc "vcpus" params)) with _ -> None in
  let memory_set = 
    try Some (Int64.shift_left (Int64.of_string (List.assoc "memory_set" params)) 20) 
    with _ -> None in
  let boot_params = try Some (List.assoc "boot_params" params) with _ -> None in
  let _ (* auto_poweron *) = try Some (bool_of_string (List.assoc "auto_poweron" params)) with _ -> None in
  let templates = get_template_records rpc session_id in
  let template = List.filter (fun t -> t.API.vM_name_label = template_name) templates in
  match template with 
      [t] ->
	marshal fd (Command (Print "Initiating install..."));
	let new_vm = Client.VM.clone rpc session_id (Client.VM.get_by_uuid rpc session_id (t.API.vM_uuid)) name in
	let uuid = Client.VM.get_uuid rpc session_id new_vm in
	
	(* Add VIFs to any network that has 'auto_add_to_VM' set to true *)
	let nets = Client.Network.get_all rpc session_id in
	let filtered_nets = List.filter (fun net -> try bool_of_string (List.assoc "auto_add_to_VM" (Client.Network.get_other_config rpc session_id net)) with _ -> false) nets in

	let device=ref 0 in
	let add_vif net =
	  let mac = Record_util.random_mac_local () in
	  marshal fd (Command (Print ("Adding VIF, device "^(string_of_int !device)^" to network '"^(Client.Network.get_name_label rpc session_id net)^"' mac="^mac)));
	  ignore(Client.VIF.create rpc session_id (string_of_int !device) net new_vm mac 1500L [] "" [] `network_default [] [] );
	  device := !device + 1
	in 
	List.iter add_vif filtered_nets;

	ignore(may (fun vcpus -> Client.VM.set_VCPUs_max rpc session_id new_vm vcpus) vcpus);
	ignore(may (fun vcpus -> Client.VM.set_VCPUs_at_startup rpc session_id new_vm vcpus) vcpus);
	ignore(may (fun memory_set -> Client.VM.set_memory_dynamic_max rpc session_id new_vm memory_set) memory_set);
	ignore(may (fun memory_set -> Client.VM.set_memory_static_max rpc session_id new_vm memory_set) memory_set);
	ignore(may (fun memory_set -> Client.VM.set_memory_dynamic_min rpc session_id new_vm memory_set) memory_set);
	ignore(may (fun boot_params -> Client.VM.set_PV_args rpc session_id new_vm boot_params) boot_params);
	ignore(Client.VM.set_name_description rpc session_id new_vm description);

	let sr_uuid = match get_default_sr_uuid rpc session_id with
	  | Some sr_uuid -> sr_uuid 
	  | None -> failwith "Failed to find a Pool default_SR and no override was provided" in
	rewrite_provisioning_xml rpc session_id new_vm sr_uuid;

	Client.VM.provision rpc session_id new_vm;
	(* Geneva doesn't start VMs automatically on install *)
	(*
	Client.VM.start rpc session_id new_vm false true;
	*)
	(* We wait for the PV bootloader switcheroo *)
	marshal fd (Command (Print ("New VM uuid: "^uuid)));
	let record_matches record =
	  (List.assoc "uuid" record) () = uuid &&
	  (List.assoc "PV-bootloader" record) () <> "installer" 
	in
	Cli_operations.event_wait_gen rpc session_id "vm" record_matches;
	marshal fd (Command (Print ("[DONE]")))
    | _ -> failwith "Template not found"


let host_bridge_list vbridge printer rpc session_id params =
  let filterfn = if vbridge then not else fun b -> b in
  let networks = Client.Network.get_all rpc session_id in
  let pbridges = List.filter (fun net -> filterfn (List.length (Client.Network.get_PIFs rpc session_id net) > 0)) networks in
  let bridge_to_printer_record pbridge =    
    let pifs = Client.Network.get_PIFs rpc session_id pbridge in
    let other_config = Client.Network.get_other_config rpc session_id pbridge in
    let name = try List.assoc "geneva-name" other_config with _ -> Client.Network.get_bridge rpc session_id pbridge in 
    [((if vbridge then "Virtual bridge" else "Physical bridge"),name);
     ("Description",Client.Network.get_name_description rpc session_id pbridge)] @
      (if not vbridge then [("NIC",if vbridge then "" else Client.PIF.get_device rpc session_id (List.hd pifs))] else []) @
      [("VLAN",if vbridge then "(null)" else Int64.to_string (Client.PIF.get_VLAN rpc session_id (List.hd pifs)));
       ("Auto add to VM",try List.assoc "auto_add_to_VM" other_config with _ -> "false")]
  in
  printer (Cli_printer.PTable (List.map bridge_to_printer_record pbridges))

let host_vbridge_add printer rpc session_id params =
  let name = List.assoc "vbridge-name" params in
  let autoadd = List.assoc "auto-vm-add" params in
  let desc = try List.assoc "vbridge-description" params with _ -> "" in
  ignore(Client.Network.create rpc session_id name desc 1500L
	    (if autoadd="true" then [("auto_add_to_VM",autoadd);("geneva-name",name)] else [("geneva-name",name)]) [])

let host_vbridge_remove printer rpc session_id params =
  let name = List.assoc "vbridge-name" params in
  let networks = Client.Network.get_all rpc session_id in
  let net = List.filter (fun net -> 
    let other_config = Client.Network.get_other_config rpc session_id net in
    if List.mem_assoc "geneva-name" other_config then
      List.assoc "geneva-name" other_config = name
    else
      Client.Network.get_bridge rpc session_id net = name) networks in
  match net with
      [n] -> Client.Network.destroy rpc session_id n
    | _ -> failwith "Multiple networks found!"

let vdi_param_set printer rpc session_id params =
  let vdi = List.assoc "vdi" params in
  let param_name=List.assoc "param-name" params in
  let param_value=List.assoc "param-value" params in
  let vdi_ref = Client.VDI.get_by_uuid rpc session_id vdi in
  match param_name with
      "name-label" -> Client.VDI.set_name_label rpc session_id vdi_ref param_value
    | "name-description" -> Client.VDI.set_name_description rpc session_id vdi_ref param_value
    | "read-only" -> Client.VDI.set_read_only rpc session_id vdi_ref (bool_of_string param_value)
    | "sharable" -> Client.VDI.set_sharable rpc session_id vdi_ref (bool_of_string param_value)
    | _ -> failwith ("Unknown param "^param_name)


let vm_vif_add printer rpc session_id params =
  let op vm =
    let vm=vm.getref () in
    let vif_name = List.assoc "vif-name" params in
    let bridge = List.assoc "bridge-name" params in
    let mac = List.assoc "mac" params in
    
    (* the name encodes the device - VIFs should be called 'ethX' or nicX in geneva/zurich *)
    let device =
      if String.startswith "eth" vif_name 
      then String.sub vif_name 3 (String.length vif_name - 3)
      else if String.startswith "nic" vif_name
      then String.sub vif_name 3 (String.length vif_name - 3)
      else failwith "VIF names must be of the form ethX or nicX"
    in
	
    (* find the bridge *)
    let networks = Client.Network.get_all rpc session_id in
    let filter net =
      let other_config = Client.Network.get_other_config rpc session_id net in
      try
	List.assoc "geneva-name" other_config = bridge 
      with
	  _ -> Client.Network.get_bridge rpc session_id net = bridge
    in
    let net = List.filter filter networks in
    match net with
      | [] -> failwith "Bridge not found"
      |	n::ns ->
	  begin
	    let vif = Client.VIF.create rpc session_id device n vm mac 1500L [] "" [] `network_default [] [] in
	    if List.mem_assoc "rate" params then
	      (Client.VIF.set_qos_algorithm_type rpc session_id vif "ratelimit";
	       Client.VIF.add_to_qos_algorithm_params rpc session_id vif "kbs" (List.assoc "rate" params))
	  end    
  in	  
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])  

let vm_vif_list printer rpc session_id params =
  let op vm =
    let vm=vm.getref () in
    let vifs = Client.VIF.get_all rpc session_id in
    let vifs = List.filter (fun vif -> Client.VIF.get_VM rpc session_id vif = vm) vifs in
    let is_hvm = Client.VM.get_HVM_boot_policy rpc session_id vm = "BIOS order" in
    let vif_to_record vif =
      let name = (if is_hvm then "nic" else "eth")^(Client.VIF.get_device rpc session_id vif) in
      let mac = Client.VIF.get_MAC rpc session_id vif in
      let bridge_other_config = Client.Network.get_other_config rpc session_id (Client.VIF.get_network rpc session_id vif) in
      let bridge = 
	try List.assoc "geneva-name" bridge_other_config
	with _ -> Client.Network.get_bridge rpc session_id (Client.VIF.get_network rpc session_id vif) 
      in
      let ip =
	try 
	  let networks = Client.VM_guest_metrics.get_networks rpc session_id (Client.VM.get_guest_metrics rpc session_id vm) in
	  List.assoc ((Client.VIF.get_device rpc session_id vif)^"/ip") networks
	with
	    _ -> "not available [guest must be on with XenSource tools installed]"
      in	    
      [("name",name);
       ("mac",mac);
       ("ip",ip);
       ("vbridge",bridge);
       ("rate","<unknown>")]
    in 
    printer (Cli_printer.PTable (List.map vif_to_record vifs))
  in
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])  

let vm_vif_remove printer rpc session_id params =
  let op vm =
    let vm = vm.getref () in
    let vifs = Client.VIF.get_all rpc session_id in
    let vifs = List.filter (fun vif -> Client.VIF.get_VM rpc session_id vif = vm) vifs in
    let vif_name = List.assoc "vif-name" params in
    let device =
      if String.startswith "eth" vif_name 
      then String.sub vif_name 3 (String.length vif_name - 3)
      else if String.startswith "nic" vif_name
      then String.sub vif_name 3 (String.length vif_name - 3)
      else failwith "VIF names must be of the form ethX or nicX"
    in
    let vif = List.filter (fun vif -> Client.VIF.get_device rpc session_id vif = device) vifs in
    match vif with
	v::vs -> Client.VIF.destroy rpc session_id v
      | _ -> failwith "Cannot find VIF"
  in  
  ignore(Cli_operations.do_vm_op printer rpc session_id op params [])  

(*

  let recs = 
    List.map (fun (_,v) ->
      (["NAME",v.API.vM_name_label;
	"uuid",v.API.vM_uuid;
	"state",Cli_util.power_to_string v.API.vM_power_state])) vms in
  printer (Cli_printer.PTable recs)
*)
(* XXX: rework post API 1.0 *)
(*
    [("ZURICH PARAMS","");
     ("name" , vm_record.API.vM_name_label );
     ("description" , vm_record.API.vM_name_description );
     ("vcpus", Int64.to_string (vm_record.API.vM_VCPUs_number ));
     ("memory_set", Int64.to_string (Int64.shift_right (vm_record.API.vM_memory_dynamic_max ) 20));
     ("auto_power_on", string_of_bool (vm_record.API.vM_auto_power_on))]] 
*)

    
(* Cmdtable *)

let cmdtable_data : (string*cmd_spec) list =
  [
    "sr-vm-clone",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };     
    "host-shutdown",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: Shutdown the host";
      implementation=No_fd host_shutdown;
      flags=[];
    };
    "host-reboot",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: Shutdown the host";
      implementation=No_fd host_reboot;
      flags=[];
    };
    "host-password-set",
    {
      reqd=["password"; "new-password"];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd host_password_set;
      flags=[];
    };    
    "host-license-add",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-license-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-pif-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: List the PIFs on the host";
      implementation=No_fd host_pif_list;
      flags=[];
    };    
    "host-cpu-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd host_cpu_list;
      flags=[];
    };    
    "host-vbridge-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd (host_bridge_list true);
      flags=[];
    };    
    "host-pbridge-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd (host_bridge_list false);
      flags=[];
    };    
    "host-cd-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd host_cd_list;
      flags=[];
    };    
    "host-vbridge-add",
    {
      reqd=["vbridge-name";"auto-vm-add"];
      optn=["vbridge-description"];
      help="COMPAT MODE: ";
      implementation=No_fd host_vbridge_add;
      flags=[];
    };    
    "host-vbridge-remove",
    {
      reqd=["vbridge-name"];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd host_vbridge_remove;
      flags=[];
    };    
    "host-sr-set",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-loglevel-set",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-logs-download",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-patch-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd host_patch_list;
      flags=[];
    };    

    "host-patch-remove",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd host_patch_remove;
      flags=[];
    };    
    "host-patch-upload",
    {
      reqd=["patch-file"];
      optn=[];
      help="COMPAT MODE: ";
      implementation=With_fd host_patch_upload;
      flags=[];
    };    
    "host-patch-apply",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd host_patch_apply;
      flags=[];
    };    
    "host-backup",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-restore",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-crash-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-crash-del",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-crash-upload",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-bugreport-upload",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "host-license-add",
    {
      reqd=["license-file"];
      optn=[];
      help="COMPAT MODE: ";
      implementation=With_fd Cli_operations.host_license_add;
      flags=[];
    };    
    "host-vm-list",
    { 
      reqd=[];
      optn=[];
      help="COMPAT MODE: List the hosts on the server";
      implementation=No_fd host_vm_list;
      flags=[];
    };
    "host-template-list",
    { 
      reqd=[];
      optn=[];
      help="COMPAT MODE: List the templates on the server";
      implementation=No_fd host_template_list;
      flags=[];
    };
    "host-sr-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: List the SRs on the server";
      implementation=No_fd host_sr_list;
      flags=[];
    };
    "host-param-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: List the parameters of the host";
      implementation=No_fd host_param_list;
      flags=[];
    };
    "vm-install",
    {
      reqd=["template-name"; "name"];
      optn=["description"; "vcpus"; "memory_set"; "boot_params"; "auto_poweron"];
      help="COMPAT MODE: ";
      implementation=With_fd vm_install;
      flags=[];
    };    
    "vm-uninstall",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=With_fd Cli_operations.vm_uninstall;
      flags=[];
    };    
    "vm-clone",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_clone;
      flags=[];
    };    
    "vm-shutdown",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_shutdown;
      flags=[];
    };    
    "vm-start",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_start;
      flags=[];
    };    
    "vm-suspend",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_suspend;
      flags=[];
    };    
    "vm-resume",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_resume;
      flags=[];
    };    
    "vm-reboot",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_reboot;
      flags=[];
    };    
    "vm-disk-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd vm_disk_list;
      flags=[];
    };    
    "vm-disk-add",
    {
      reqd=["disk-name";"disk-size"];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_disk_add;
      flags=[];
    };    
    "vm-disk-remove",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_disk_remove;
      flags=[];
    };    
    "vm-disk-resize",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_disk_resize;
      flags=[];
    };    
    "vm-disk-setqos",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd vm_disk_setqos;
      flags=[];
    };    
    "vm-cd-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd vm_cd_list;
      flags=[];
    };    
    "vm-cd-add",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_cd_add;
      flags=[];
    };    
    "vm-cd-remove",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd Cli_operations.vm_cd_remove;
      flags=[];
    };    
    "vm-cd-change",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "vm-vif-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd vm_vif_list;
      flags=[];
    };    
    "vm-vif-add",
    {
      reqd=["vif-name";"mac";"bridge-name"];
      optn=["rate"];
      help="COMPAT MODE: ";
      implementation=No_fd vm_vif_add;
      flags=[];
    };    
    "vm-vif-remove",
    {
      reqd=["vif-name"];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd vm_vif_remove;
      flags=[];
    };    
    "vm-param-list",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd vm_param_list;
      flags=[];
    };    
    "vm-param-get",
    {
      reqd=["param-name"];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd vm_param_get;
      flags=[];
    };    
    "vm-param-set",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd vm_param_set;
      flags=[];
    };    
    "vm-export",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };    
    "vm-import",
    {
      reqd=[];
      optn=[];
      help="COMPAT MODE: ";
      implementation=No_fd not_implemented;
      flags=[];
    };          
  ]
    


