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
(* Create a pool of SDKs *)

open Client
open Perfutil
open Stdext
open Listext
open Pervasiveext
open Xstringext
open Scenario
open Perfdebug

let master_of_pool = "master_of_pool"
let management_ip = "management_ip"

let get_network_num_from_interface pool i =
  if i < pool.bonds * 2 then
    i / 2
  else
    i - pool.bonds

(** Only storage types supporting active thin-provisioned disks allow us to
    create a 2TiB disk, clone it and attach it to a bunch of VMs without
    running out of space. In particular the hybrid thin/thick behaviour of
    LVHD won't work so we can't use LVM over iSCSI or FC. It's probably easiest
    to include a whitelist here rather than find an EQL array to test this. *)
let sr_is_suitable session_id sr =
  let t = String.lowercase_ascii (Client.SR.get_type rpc session_id sr) in
  t = "ext" || t = "nfs"

let default_sr_must_be_suitable session_id =
  let realpool = List.hd (Client.Pool.get_all rpc session_id) in
  let defaultsr = Client.Pool.get_default_SR rpc session_id realpool in
  if not (sr_is_suitable session_id defaultsr)
  then failwith "Pool's default SR is unsuitable for the local storage on the template"

let initialise session_id template pool =
  (* First, create the networks the hosts will have their interfaces on *)
  let networks_to_create = pool.interfaces_per_host - pool.bonds in
  debug "Creating %d networks..." networks_to_create;
  let networks = Array.init networks_to_create (fun i ->
      Client.Network.create ~rpc ~session_id ~name_label:(Printf.sprintf "perftestnet%d" i) ~name_description:"" ~mTU:1500L ~other_config:[oc_key,pool.key] ~bridge:"" ~managed:true ~tags:[])
  in

  (* Set up the template - create the VIFs *)
  debug "Setting up the template. Creating VIFs on networks";
  let interfaces = Array.init pool.interfaces_per_host (fun i ->
      let net = networks.(get_network_num_from_interface pool i) in
      Client.VIF.create ~rpc ~session_id ~device:(string_of_int i) ~network:net ~vM:template ~mAC:"" ~mTU:1500L
        ~other_config:[oc_key,pool.key] ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~locking_mode:`network_default ~ipv4_allowed:[] ~ipv6_allowed:[])
  in

  (* Create a disk for local storage *)
  debug "Creating a disk for local storage on the template";
  default_sr_must_be_suitable session_id;
  let realpool = List.hd (Client.Pool.get_all rpc session_id) in
  let defaultsr = Client.Pool.get_default_SR rpc session_id realpool in
  let newdisk = Client.VDI.create ~rpc ~session_id ~name_label:"SDK storage" ~name_description:"" ~sR:defaultsr
      ~virtual_size:sr_disk_size ~_type:`user ~sharable:false ~read_only:false ~xenstore_data:[] ~other_config:[oc_key,pool.key]
      ~sm_config:[] ~tags:[] in
  let (_: API.ref_VBD) = Client.VBD.create ~rpc ~session_id ~vM:template ~vDI:newdisk ~userdevice:sr_disk_device ~bootable:false
      ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~other_config:[oc_key,pool.key]
  in

  debug "Setting up xenstore keys";
  (* Set up the various xenstore keys *)
  Client.VM.set_PV_args ~rpc ~session_id ~self:template ~value:"noninteractive"; (* no password setting step *)
  Client.VM.add_to_xenstore_data ~rpc ~session_id ~self:template ~key:"vm-data/provision/interfaces/0/admin" ~value:"true";
  Array.iteri (fun i net ->
      Client.VM.add_to_xenstore_data ~rpc ~session_id ~self:template ~key:(Printf.sprintf "vm-data/provision/interfaces/%d/mode" i) ~value:"static";
      Client.VM.add_to_xenstore_data ~rpc ~session_id ~self:template ~key:(Printf.sprintf "vm-data/provision/interfaces/%d/netmask" i) ~value:"255.255.255.0") interfaces;

  debug "Setting memory to 128 Megs";
  Client.VM.set_memory_static_min rpc session_id template (Int64.mul 128L 1048576L);
  Client.VM.set_memory_dynamic_min rpc session_id template (Int64.mul 128L 1048576L);
  Client.VM.set_memory_dynamic_max rpc session_id template (Int64.mul 128L 1048576L);
  Client.VM.set_memory_static_max rpc session_id template (Int64.mul 128L 1048576L);

  Client.VM.remove_from_other_config rpc session_id template oc_key;
  Client.VM.add_to_other_config rpc session_id template oc_key pool.key;

  interfaces

let reset_template session_id template =
  (* Destroy template's VIFs *)
  debug "Resetting template to factory settings";
  let vifs = Client.VM.get_VIFs rpc session_id template in
  List.iter (fun vif -> try if List.mem_assoc oc_key (Client.VIF.get_other_config rpc session_id vif) then Client.VIF.destroy rpc session_id vif with _ -> ()) vifs;

  (* Destroy template's sr disk *)
  let vbds = Client.VM.get_VBDs rpc session_id template in
  List.iter (fun vbd ->
      if List.mem_assoc oc_key (Client.VBD.get_other_config rpc session_id vbd) then begin
        let vdi = Client.VBD.get_VDI rpc session_id vbd in
        assert (List.mem_assoc oc_key (Client.VDI.get_other_config rpc session_id vdi));
        Client.VDI.destroy rpc session_id vdi;
        try Client.VBD.destroy rpc session_id vbd with _ -> ();
      end) vbds;

  (* Remove xenstore keys *)
  Client.VM.set_xenstore_data rpc session_id template [];
  Client.VM.set_PV_args rpc session_id template "";
  try Client.VM.remove_from_other_config rpc session_id template oc_key with _ -> ()

let uninitialise session_id template key =
  (* Shut down and uninstall any VMs *)
  debug "Shutting down and uninstalling any VMs";
  let vms = Client.VM.get_all rpc session_id in
  List.iter (fun vm ->
      let is_a_template = Client.VM.get_is_a_template rpc session_id vm in
      let is_control_domain = Client.VM.get_is_control_domain rpc session_id vm in
      let is_managed = try (List.assoc oc_key (Client.VM.get_other_config rpc session_id vm)) = key with _ -> false in
      let running = Client.VM.get_power_state rpc session_id vm = `Running in
      if not is_a_template && not is_control_domain && is_managed then begin
        if running then Client.VM.hard_shutdown rpc session_id vm;
        let vbds = Client.VM.get_VBDs rpc session_id vm in
        let vdis = List.map (fun vbd -> Client.VBD.get_VDI rpc session_id vbd) vbds in
        List.iter (fun vdi -> try Client.VDI.destroy rpc session_id vdi with _ -> ()) vdis;
        List.iter (fun vbd -> try Client.VBD.destroy rpc session_id vbd with _ -> ()) vbds;
        List.iter (fun vif -> try Client.VIF.destroy rpc session_id vif with _ -> ()) (Client.VM.get_VIFs rpc session_id vm);
        Client.VM.destroy rpc session_id vm
      end) vms;

  (* Destroy networks *)
  debug "Destroying networks";
  let nets = Client.Network.get_all_records rpc session_id in
  let mynets = List.filter (fun (_,r) ->
      List.mem_assoc oc_key r.API.network_other_config &&
      List.assoc oc_key r.API.network_other_config = key) nets in
  List.iter (fun (net,_) -> Client.Network.destroy rpc session_id net) mynets;

  let nets = Client.Network.get_all_records rpc session_id in
  debug "Destroying any bridges";
  let ic = Unix.open_process_in "ifconfig -a | grep \"^xapi\" | awk '{print $1}'" in
  let netdevs =
    let rec doline () =
      try
        let x = input_line ic in
        x :: doline ()
      with _ -> []
    in doline ()
  in

  List.iter (fun netdev ->
      if not (List.exists (fun (_,net) -> net.API.network_bridge = netdev) nets)
      then begin
        ignore(Sys.command (Printf.sprintf "ifconfig %s down 2>/dev/null" netdev));
        ignore(Sys.command (Printf.sprintf "brctl delbr %s 2>/dev/null" netdev))
      end) netdevs

let destroy_sdk_pool session_id sdkname key =
  let template = List.hd (Client.VM.get_by_name_label rpc session_id sdkname) in  uninitialise session_id template key

let describe_pool template_name pool_name key =
  let pool = Scenario.get pool_name in
  let pool = {pool with key=key} in
  Printf.sprintf "Base template: %s" template_name :: (description_of_pool pool)

let iscsi_vm_iso_must_exist session_id =
  (* The iSCSI VM iso must exist *)
  if CreateVM.find_iscsi_iso session_id = None
  then failwith (Printf.sprintf "The iSCSI target VM iso could not be found (%s)" CreateVM.iscsi_vm_iso)

let create_sdk_pool session_id sdkname pool_name key ipbase =
  iscsi_vm_iso_must_exist session_id;
  default_sr_must_be_suitable session_id;
  let pool = List.find (fun p -> p.id = pool_name) pools in
  let pool = {pool with key=key; ipbase=ipbase} in

  let template =
    try List.hd (Client.VM.get_by_name_label rpc session_id sdkname)
    with _ -> debug ~out:stderr "template '%s' not found" sdkname; exit 1
  in
  let uuid = Client.VM.get_uuid rpc session_id template in
  debug "Creating test pool '%s' using SDK template uuid=%s" pool.id uuid;

  (* Clear up any leftover state on the template *)
  reset_template session_id template;

  let interfaces = initialise session_id template pool in

  Printf.printf "Creating iSCSI target VM serving %d LUNs\n%!" pool.iscsi_luns;
  let (_ : API.ref_VM option) = CreateVM.make_iscsi session_id pool (Client.VIF.get_network rpc session_id interfaces.(2)) in

  debug "Creating %d SDK VMs" pool.hosts;
  let hosts = Array.init pool.hosts (
      fun i ->
        let n = i + 1 in
        let vm = Client.VM.clone rpc session_id template (Printf.sprintf "perftestpool%d" n) in
        Client.VM.provision rpc session_id vm;
        Array.iteri (fun i _ ->
            ignore(Client.VM.add_to_xenstore_data rpc session_id vm (Printf.sprintf "vm-data/provision/interfaces/%d/ip" i)
                     (Printf.sprintf "192.168.%d.%d" (i+pool.ipbase) n))) interfaces;
        vm)
  in

  debug "Setting memory on master to be 256 Megs";
  Client.VM.set_memory_static_max rpc session_id hosts.(0) (Int64.mul 256L 1048576L);
  Client.VM.set_memory_static_min rpc session_id hosts.(0) (Int64.mul 256L 1048576L);
  Client.VM.set_memory_dynamic_max rpc session_id hosts.(0) (Int64.mul 256L 1048576L);
  Client.VM.set_memory_dynamic_min rpc session_id hosts.(0) (Int64.mul 256L 1048576L);

  Client.VM.add_to_other_config rpc session_id hosts.(0) master_of_pool pool.key;
  Client.VM.add_to_other_config rpc session_id hosts.(0) management_ip (Printf.sprintf "192.168.%d.1" pool.ipbase);

  let localhost_uuid = Inventory.lookup "INSTALLATION_UUID" in
  Array.iteri (fun i host -> debug "Starting VM %d" i; Client.VM.start_on rpc session_id host (Client.Host.get_by_uuid rpc session_id localhost_uuid) false false) hosts;

  ignore(Sys.command (Printf.sprintf "ifconfig %s 192.168.%d.200 up" (Client.Network.get_bridge rpc session_id (Client.VIF.get_network rpc session_id interfaces.(0))) pool.ipbase));

  reset_template session_id template;

  debug "Guests are now booting...";
  let pingable = Array.make (Array.length hosts) false in
  let firstboot = Array.make (Array.length hosts) false in
  let string_of_status () =
    String.implode
      (Array.to_list
         (Array.mapi (fun i ping ->
              let boot = firstboot.(i) in match ping, boot with
              | false, false -> '.'
              | true, false -> 'P'
              | true, true -> 'B'
              | _, _ -> '?') pingable)) in

  let has_guest_booted i vm =
    let ip = Printf.sprintf "192.168.%d.%d" pool.ipbase (i+1) in
    let is_pingable () =
      if pingable.(i) then true else begin
        if Sys.command (Printf.sprintf "ping -W 1 -c 1 %s 2>/dev/null >/dev/null" ip) = 0 then begin
          pingable.(i) <- true;
          debug "Individual host status: %s" (string_of_status ());
          true
        end else false
      end in
    let firstbooted () =
      if firstboot.(i) then true else begin
        let rpc = remoterpc ip in
        try
          let s = Client.Session.login_with_password rpc "root" "xensource" "1.1" "perftest" in
          finally
            (fun () ->
               let host = List.hd (Client.Host.get_all rpc s) in (* only one host because it hasn't joined the pool yet *)
               let other_config = Client.Host.get_other_config rpc s host in
               let key = "firstboot-complete" in
               (* Since these are 'fresh' hosts which have never booted, the key goes from missing -> present *)
               if List.mem_assoc key other_config then begin
                 firstboot.(i) <- true;
                 debug "Individual host status: %s" (string_of_status ());
                 true;
               end else false
            )
            (fun () -> Client.Session.logout rpc s)
        with _ -> false
      end in
    is_pingable () && (firstbooted ()) in

  let wait_until_guests_have_booted () =
    for i = 0 to Array.length pingable - 1 do
      pingable.(i) <- false;
    done;
    let finished = ref false in
    while not !finished do
      finished := List.fold_left (&&) true (Array.to_list (Array.mapi has_guest_booted hosts));
      Unix.sleep 20;
    done in

  wait_until_guests_have_booted ();
  debug "Guests have booted; issuing Pool.joins.";

  let host_uuids = Array.mapi (fun i vm ->
      let n = i + 1 in
      let rpc = remoterpc (Printf.sprintf "192.168.%d.%d" pool.ipbase n) in
      let s = Client.Session.login_with_password rpc "root" "xensource" "1.1" "perftest" in
      let h = List.hd (Client.Host.get_all rpc s) in
      let u = Client.Host.get_uuid rpc s h in
      debug "Setting name of host %d" n;
      Client.Host.set_name_label rpc s h (Printf.sprintf "perftest host %d" i);
      if i<>0 then begin
        debug "Joining to pool";
        Client.Pool.join rpc s (Printf.sprintf "192.168.%d.1" pool.ipbase) "root" "xensource"
      end;
      u
    ) hosts in

  let poolrpc = remoterpc (Printf.sprintf "192.168.%d.1" pool.ipbase) in
  let poolses = Client.Session.login_with_password poolrpc "root" "xensource" "1.1" "perftest" in

  let vpool=List.hd (Client.Pool.get_all poolrpc poolses) in
  Client.Pool.add_to_other_config poolrpc poolses vpool "scenario" pool_name;

  debug "Waiting for all hosts to become live and enabled";
  let hosts = Array.of_list (Client.Host.get_all poolrpc poolses) in
  let live = Array.make (Array.length hosts) false in
  let enabled = Array.make (Array.length hosts) false in
  let string_of_status () =
    String.implode
      (Array.to_list
         (Array.mapi (fun i live ->
              let enabled = enabled.(i) in match live, enabled with
              | false, false -> '.'
              | true, false -> 'L'
              | true, true -> 'E'
              | _, _ -> '?') live)) in

  let has_host_booted rpc session_id i host =
    try
      if live.(i) && enabled.(i) then true else begin
        let metrics = Client.Host.get_metrics rpc session_id host in
        let live' = Client.Host_metrics.get_live rpc session_id metrics in
        let enabled' = Client.Host.get_enabled rpc session_id host in
        if live.(i) <> live' || enabled.(i) <> enabled' then debug "Individual host status: %s" (string_of_status ());
        live.(i) <- live';
        enabled.(i) <- enabled';
        live' && enabled'
      end
    with _ -> false in
  let finished = ref false in
  while not !finished do
    Unix.sleep 20;
    finished := List.fold_left (&&) true (Array.to_list (Array.mapi (has_host_booted poolrpc poolses) hosts));
  done;
  debug "All hosts are ready.";

  let mypool = List.hd (Client.Pool.get_all poolrpc poolses) in
  let master = Client.Pool.get_master poolrpc poolses mypool in

  let iscsi_vm_ip = CreateVM.make_iscsi_ip pool in

  let xml = try
      Client.SR.probe ~rpc:poolrpc ~session_id:poolses ~host:master
        ~device_config:["target",iscsi_vm_ip]
        ~sm_config:[]
        ~_type:"lvmoiscsi"
    with Api_errors.Server_error("SR_BACKEND_FAILURE_96",[a;b;xml]) ->
      xml
  in
  let iqns = parse_sr_probe_for_iqn xml in
  if iqns = [] then failwith "iSCSI target VM failed again - maybe you should fix it this time?";
  let iqn = List.hd iqns in
  let xml = try
      Client.SR.probe ~rpc:poolrpc ~session_id:poolses ~host:master
        ~device_config:["target",iscsi_vm_ip; "targetIQN",iqn]
        ~sm_config:[]
        ~_type:"lvmoiscsi"
    with Api_errors.Server_error("SR_BACKEND_FAILURE_107",[a;b;xml]) ->
      xml
  in

  (* Create an SR for each LUN found *)
  Printf.printf "Creating LVMoISCSI SRs (one for each of %d LUNs)\n%!" pool.iscsi_luns;
  let scsiids = Array.of_list (parse_sr_probe_for_scsiids xml) in
  if Array.length scsiids <> pool.iscsi_luns then failwith (Printf.sprintf "We created %d VDIs on the iSCSI target VM but found %d LUNs" pool.iscsi_luns (Array.length scsiids));
  let lun_srs = Array.init pool.iscsi_luns
      (fun i ->
         Printf.printf " - Creating shared LVMoISCSI SR %d...\n%!" i;
         let name_label = Printf.sprintf "LVMoISCSI-%d" i in
         Client.SR.create poolrpc poolses master ["target",iscsi_vm_ip; "targetIQN",iqn; "SCSIid",scsiids.(i)]
           0L name_label "" "lvmoiscsi" "" true [])
  in

  let local_srs = Array.mapi (fun i host_uuid ->
      let h = Client.Host.get_by_uuid poolrpc poolses host_uuid in
      let name_label = Printf.sprintf "Local LVM on host %d" i in
      Client.SR.create poolrpc poolses h ["device","/dev/"^sr_disk_device] 0L name_label "" "lvm" "" false []) host_uuids
  in

  let pifs = Client.PIF.get_all poolrpc poolses in

  let bondednets = Array.init pool.bonds (fun i ->
      Client.Network.create poolrpc poolses (Printf.sprintf "Network associated with bond%d" i) "" 1500L [] "" true [])
  in

  let unused_nets = ref (List.setify (List.map (fun pif -> Client.PIF.get_network poolrpc poolses pif) pifs)) in

  (* Reconfigure the master's networking last as this will be the most destructive *)
  let master_uuid = Client.Host.get_uuid poolrpc poolses master in
  let slave_uuids = List.filter (fun x -> x <> master_uuid) (Array.to_list host_uuids) in
  let host_uuids = Array.of_list (slave_uuids @ [ master_uuid ]) in

  let (_ : API.ref_Bond array array) = Array.mapi (fun i host_uuid ->
      let host_ref = Client.Host.get_by_uuid poolrpc poolses host_uuid in
      let pifs = List.filter (fun pif -> Client.PIF.get_host poolrpc poolses pif = host_ref) pifs in
      Array.init pool.bonds (fun bnum ->
          let device = Printf.sprintf "eth%d" (bnum*2) in
          let device2 = Printf.sprintf "eth%d" (bnum*2 + 1) in
          let master = List.find (fun pif -> Client.PIF.get_device poolrpc poolses pif = device) pifs in
          let pifs = List.filter (fun pif -> let d = Client.PIF.get_device poolrpc poolses pif in d=device || d=device2) pifs in
          let nets = List.map (fun pif -> Client.PIF.get_network poolrpc poolses pif) pifs in
          unused_nets := List.filter (fun net -> not (List.mem net nets)) !unused_nets;
          let mac = Client.PIF.get_MAC poolrpc poolses master in
          let bond = Client.Bond.create poolrpc poolses bondednets.(bnum) pifs mac `balanceslb [] in
          let bondpif = Client.Bond.get_master poolrpc poolses bond in
          Client.PIF.reconfigure_ip poolrpc poolses bondpif `Static (Client.PIF.get_IP poolrpc poolses master) "255.255.255.0" "" "";
          if Client.PIF.get_management poolrpc poolses master then begin
            (try Client.Host.management_reconfigure poolrpc poolses bondpif;
             with _ -> ());
            debug "Reconfigured management interface to be on the bond.";
            (* In case we've lost our network connection *)
            wait_until_guests_have_booted ();
          end;
          bond
        )
    ) host_uuids in
  debug "Waiting for all guests to be pingable again.";
  wait_until_guests_have_booted ();
  debug "Successfully pinged all virtual hosts.";
  (* We'll use the Windows XP SP3 template to create the VMs required *)

  let nets_for_vms = !unused_nets @ (Array.to_list bondednets) in

  debug "Nets for VMs: %s" (String.concat "," (List.map (fun net -> Client.Network.get_name_label poolrpc poolses net) nets_for_vms));

  let networks = Array.of_list nets_for_vms in

  Printf.printf "Creating VMs (%s)\n%!" (if pool.use_shared_storage then "on shared storage" else "on local storage");
  let storages = if pool.use_shared_storage then lun_srs else local_srs in
  List.iter (fun vm -> CreateVM.make ~rpc:poolrpc ~session_id:poolses ~networks ~storages ~pool ~vm) pool.vms

let create_pool session_id sdkname pool_name key ipbase =
  iscsi_vm_iso_must_exist session_id;
  default_sr_must_be_suitable session_id;
  let pool = Scenario.get pool_name in
  let pool = { pool with key=key } in
  if pool.Scenario.hosts <> 1
  then begin
    debug ~out:stderr "At the moment, multiple host pool is supported only for SDK pool";
    exit 1
  end;
  let host = List.hd (Client.Host.get_all ~rpc ~session_id) in

  (* 1/ forget the local lvm storages *)
  List.iter (fun lvm_sr ->
      List.iter
        (fun pbd -> Client.PBD.unplug ~rpc ~session_id ~self:pbd)
        (Client.SR.get_PBDs ~rpc ~session_id ~self:lvm_sr);
      Client.SR.forget ~rpc ~session_id ~sr:lvm_sr)
    (Client.SR.get_by_name_label ~rpc ~session_id ~label:"Local storage");

  (* 2/ create an default ext storage *)
  let storages =
    match Client.SR.get_by_name_label ~rpc ~session_id ~label:"Local vhd" with
    | [] ->
      [| Client.SR.create ~rpc ~session_id ~_type:"ext" ~name_label:"Local vhd" ~name_description:"" ~device_config:["device","/dev/sda3"]
           ~host ~physical_size:Scenario.sr_disk_size ~shared:true ~sm_config:[] ~content_type:"" |]
    | l -> Array.of_list l
  in
  let pool_ref = List.hd (Client.Pool.get_all ~rpc ~session_id) in
  Client.Pool.set_default_SR ~rpc ~session_id ~self:pool_ref ~value:storages.(0);
  Client.Pool.set_crash_dump_SR ~rpc ~session_id ~self:pool_ref ~value:storages.(0);
  Client.Pool.set_suspend_image_SR ~rpc ~session_id ~self:pool_ref ~value:storages.(0);

  (* 3/ building the VMs *)
  let networks = Array.of_list (Client.Network.get_all ~rpc ~session_id) in
  List.iter (fun vm -> CreateVM.make ~rpc ~session_id ~networks ~storages ~pool ~vm) pool.vms
