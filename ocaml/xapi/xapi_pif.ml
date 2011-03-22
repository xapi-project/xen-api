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
module D=Debug.Debugger(struct let name="xapi" end)
open D

module L = Debug.Debugger(struct let name="license" end) 

open Stringext

let bridge_naming_convention (device: string) =
  if String.startswith "eth" device
  then ("xenbr" ^ (String.sub device 3 (String.length device - 3)))
  else ("br" ^ device)

let read_bridges_from_inventory () =
  try String.split ' ' (Xapi_inventory.lookup Xapi_inventory._current_interfaces) with _ -> []

let assert_not_in_bond ~__context ~self = 
  (* Prevent bond slaves interfaces *)
  let bond = Db.PIF.get_bond_slave_of ~__context ~self in
  if try ignore(Db.Bond.get_uuid ~__context ~self:bond); true with _ -> false 
  then raise (Api_errors.Server_error (Api_errors.pif_already_bonded, [ Ref.string_of self ]));
  (* Disallow for bond masters *)
  if Db.PIF.get_bond_master_of ~__context ~self <> []
  then raise (Api_errors.Server_error (Api_errors.pif_already_bonded, [ Ref.string_of self ]))

let assert_no_vlans ~__context ~self = 
  (* Disallow if this is a base interface of any existing VLAN *)
  let vlans = Db.PIF.get_VLAN_slave_of ~__context ~self in
  debug "PIF %s assert_no_vlans = [ %s ]" 
    (Db.PIF.get_uuid ~__context ~self) (String.concat "; " (List.map Ref.string_of vlans));
  if vlans <> [] then begin
    debug "PIF has associated VLANs: [ %s ]" 
      (String.concat "; " (List.map (fun self -> Db.VLAN.get_uuid ~__context ~self) vlans));
    raise (Api_errors.Server_error (Api_errors.pif_vlan_still_exists, [ Ref.string_of self ]))
  end;
  (* Disallow if this is a derived interface of a VLAN *)
  if Db.PIF.get_VLAN ~__context ~self <> (-1L) && not (Xapi_fist.allow_forget_of_vlan_pif ())
  then raise (Api_errors.Server_error (Api_errors.pif_vlan_still_exists, [ Ref.string_of self ]))

let assert_no_tunnels ~__context ~self = 
  (* Disallow if this is a transport interface of any existing tunnel *)
  let tunnels = Db.PIF.get_tunnel_transport_PIF_of ~__context ~self in
  debug "PIF %s assert_no_tunnels = [ %s ]" 
    (Db.PIF.get_uuid ~__context ~self) (String.concat "; " (List.map Ref.string_of tunnels));
  if tunnels <> [] then begin
    debug "PIF has associated tunnels: [ %s ]" 
      (String.concat "; " (List.map (fun self -> Db.Tunnel.get_uuid ~__context ~self) tunnels));
    raise (Api_errors.Server_error (Api_errors.pif_tunnel_still_exists, [ Ref.string_of self ]))
  end;
  (* Disallow if this is an access interface of a tunnel *)
  if Db.PIF.get_tunnel_access_PIF_of ~__context ~self <> []
  then raise (Api_errors.Server_error (Api_errors.pif_tunnel_still_exists, [ Ref.string_of self ]))

let assert_not_management_pif ~__context ~self = 
  if Db.PIF.get_currently_attached ~__context ~self 
    && Db.PIF.get_management ~__context ~self then
      raise (Api_errors.Server_error(Api_errors.pif_is_management_iface, [ Ref.string_of self ]))

let assert_not_slave_management_pif ~__context ~self = 
  if true
    && Pool_role.is_slave () 
    && Db.PIF.get_currently_attached ~__context ~self 
    && Db.PIF.get_management ~__context ~self
  then raise (Api_errors.Server_error(Api_errors.pif_is_management_iface, [ Ref.string_of self ]))

let assert_no_protection_enabled ~__context ~self = 
  (* If HA or redo-log is enabled and PIF is attached then refuse to reconfigure 
   * the interface at all *)
  if Db.PIF.get_currently_attached ~__context ~self then begin
    let pool = List.hd (Db.Pool.get_all ~__context) in
    if Db.Pool.get_ha_enabled ~__context ~self:pool then
      raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []))
    else if Db.Pool.get_redo_log_enabled ~__context ~self:pool then
      raise (Api_errors.Server_error(Api_errors.redo_log_is_enabled, []))
  end

let abort_if_network_attached_to_protected_vms ~__context ~self = 
  (* Abort a PIF.unplug if the Network has VIFs connected to protected VMs *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool && not(Db.Pool.get_ha_allow_overcommit ~__context ~self:pool) then begin
    let net = Db.PIF.get_network ~__context ~self in
    let vifs = Db.Network.get_VIFs ~__context ~self:net in
    let vms = List.map (fun vif -> Db.VIF.get_VM ~__context ~self:vif) vifs in
    List.iter
      (fun vm ->
	 if Helpers.is_xha_protected ~__context ~self:vm then begin
	   warn "PIF.unplug will make protected VM %s not agile since it has a VIF attached to network %s" (Ref.string_of vm) (Ref.string_of net);
	   raise (Api_errors.Server_error(Api_errors.ha_operation_would_break_failover_plan, []))
	 end
      ) vms
  end

let assert_no_other_local_pifs ~__context ~host ~network = 
  let all_pifs = Db.Host.get_PIFs ~__context ~self:host in
  let other_pifs = List.filter (fun self -> Db.PIF.get_network ~__context ~self = network) all_pifs in
  if other_pifs <> []
  then raise (Api_errors.Server_error(Api_errors.network_already_connected, [Ref.string_of host; Ref.string_of (List.hd other_pifs)]))

let find_or_create_network (bridge: string) (device: string) ~__context = 
  let all = Db.Network.get_all ~__context in
  let bridges = List.map (fun self -> Db.Network.get_bridge ~__context ~self) all in
  let table = List.combine bridges all in
  if List.mem_assoc bridge table 
  then List.assoc bridge table
  else 
    let net_ref = Ref.make () and net_uuid = Uuid.to_string (Uuid.make_uuid ()) in
    let () = Db.Network.create ~__context ~ref:net_ref ~uuid:net_uuid
      ~current_operations:[] ~allowed_operations:[]
      ~name_label:(Helpers.choose_network_name_for_pif device)
      ~name_description:"" ~mTU:1500L
      ~bridge ~other_config:[] ~blobs:[] ~tags:[] in
    net_ref

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

type tables = { mac_to_pif_table: (string * API.ref_PIF) list;
		mac_to_phy_table: (string * string) list;
		mac_to_biosname_table: (string * string) list}

let make_tables ~__context ~host = 
  (* Enumerate known MAC addresses *)
  let existing = List.filter (fun self -> Db.PIF.get_physical ~__context ~self) (Db.Host.get_PIFs ~__context ~self:host) in
  let existing_macs = List.map (fun self -> Db.PIF.get_MAC ~__context ~self) existing in
  let mac_to_pif_table = List.combine existing_macs existing in

  (* Enumerate the MAC addresses which really exist in hardware *)
  let physical = List.filter Netdev.is_physical (Netdev.list ()) in
  let physical_macs = List.map Netdev.get_address physical in
  let mac_to_phy_table = List.combine physical_macs physical in

  let bios_names = List.map Netdev.get_bios_name physical in
  let mac_to_biosname_table = List.combine physical_macs bios_names in

  { mac_to_pif_table = mac_to_pif_table;
    mac_to_phy_table = mac_to_phy_table;
    mac_to_biosname_table = mac_to_biosname_table}

let is_my_management_pif ~__context ~self = 
  let net = Db.PIF.get_network ~__context ~self in
  let management_if = Xapi_inventory.lookup Xapi_inventory._management_interface in
  Db.Network.get_bridge ~__context ~self:net = management_if

let make_pif_metrics ~__context =
  let metrics = Ref.make () and metrics_uuid = Uuid.to_string (Uuid.make_uuid ()) in
  let () = Db.PIF_metrics.create ~__context ~ref:metrics ~uuid:metrics_uuid ~carrier:false
    ~device_name:"" ~vendor_name:"" ~device_id:"" ~vendor_id:""
    ~speed:0L ~duplex:false ~pci_bus_path:""
    ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.) ~other_config:[] in
  metrics

let pool_introduce ~__context
    ~device ~network ~host ~mAC ~mTU ~vLAN ~physical ~ip_configuration_mode
    ~iP ~netmask ~gateway ~dNS ~bond_slave_of ~vLAN_master_of ~management ~other_config ~disallow_unplug =
  let pif_ref = Ref.make() in
  let metrics = make_pif_metrics ~__context in
  let () =
    Db.PIF.create ~__context ~ref:pif_ref ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
    ~device ~device_name:device ~network ~host ~mAC ~mTU ~vLAN ~metrics
    ~physical ~currently_attached:false 
    ~ip_configuration_mode ~iP ~netmask ~gateway ~dNS
    ~bond_slave_of:Ref.null ~vLAN_master_of ~management ~other_config ~disallow_unplug in
  pif_ref

let db_introduce = pool_introduce

let db_forget ~__context ~self = Db.PIF.destroy ~__context ~self

let mark_pif_as_dirty device vLAN =
  Threadext.Mutex.execute Rrd_shared.mutex
    (fun () ->
       Rrd_shared.dirty_pifs := Rrd_shared.StringSet.add (Helpers.get_dom0_network_device_name device vLAN) !Rrd_shared.dirty_pifs;
       Condition.broadcast Rrd_shared.condition
    )

(* Internal [introduce] is passed a pre-built table [t] *)
let introduce_internal ?network ?(physical=true) ~t ~__context ~host ~mAC ~mTU ~device ~vLAN ~vLAN_master_of () = 
  let is_vlan = vLAN >= 0L in

  (* Assert that an interface with the given MAC address exists (ALSO DONE IN [introduce]!) *)
  if not(List.mem_assoc mAC t.mac_to_phy_table) && not(is_vlan)
  then raise (Api_errors.Server_error(Api_errors.mac_does_not_exist, [ mAC ]));

  let bridge = bridge_naming_convention device in
  (* If we are not told which network to use, apply the default convention *)
  let net_ref = match network with None -> find_or_create_network bridge device ~__context | Some x -> x in
  let metrics = make_pif_metrics ~__context in  
  let pif = Ref.make () in

  debug "Creating a new record for NIC: %s: %s" device (Ref.string_of pif);

  let () = Db.PIF.create ~__context ~ref:pif ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
    ~device ~device_name:device ~network:net_ref ~host ~mAC ~mTU ~vLAN ~metrics
    ~physical ~currently_attached:false 
    ~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:"" ~dNS:""
    ~bond_slave_of:Ref.null ~vLAN_master_of ~management:false ~other_config:[] ~disallow_unplug:false in

  (* If I'm a pool slave and this pif represents my management interface then
     leave it alone: if the interface goes down (through a call to "up") then
     I loose my connection to the master's database and the call to "up"
     (which uses the API and requires the database) blocks until the slave
     restarts in emergency mode *)
  (* Rob: nothing seems to be done with the pool slave case mentioned in this comment...? *)
  if is_my_management_pif ~__context ~self:pif then begin 
    debug "NIC is the management interface";
    Db.PIF.set_management ~__context ~self:pif ~value:true;
    Db.PIF.set_currently_attached ~__context ~self:pif ~value:true;
  end;

  (* When a new PIF is introduced then we mark it as dirty wrt monitor thread; this ensures
     that the PIF metrics (including carrier and vendor etc.) will eventually get updated [and
     that subsequent changes to this PIFs' device's dom0 configuration will be reflected
     accordingly]
  *)
  mark_pif_as_dirty device vLAN;
 
  (* return ref of newly created pif record *) 
  pif 

(* Internal [forget] is passed a pre-built table [t] *)
let forget_internal ~t ~__context ~self = 
  Nm.bring_pif_down ~__context self;
  (* NB we are allowed to forget an interface which still exists *)

  let mac = Db.PIF.get_MAC ~__context ~self in
  if List.mem_assoc mac t.mac_to_phy_table
  then warn "Forgetting PIF record even though MAC %s still exists" mac;
  (try
     let metrics = Db.PIF.get_metrics ~__context ~self in
     Db.PIF_metrics.destroy ~__context ~self:metrics with _ -> ());
  Db.PIF.destroy ~__context ~self

let update_management_flags ~__context ~host = 
  let management_intf = Xapi_inventory.lookup Xapi_inventory._management_interface in
  let all_pifs = Db.PIF.get_all ~__context in
  let my_pifs = List.filter (fun self -> Db.PIF.get_host ~__context ~self = host) all_pifs in
  List.iter
    (fun pif ->
       let is_management_pif = 
	 try
	   let net = Db.PIF.get_network ~__context ~self:pif in
	   let bridge = Db.Network.get_bridge ~__context ~self:net in
	   bridge = management_intf
	 with _ -> false in
       Db.PIF.set_management ~__context ~self:pif ~value:is_management_pif)
    my_pifs

let introduce ~__context ~host ~mAC ~device =
  if not (Helpers.is_valid_MAC mAC) then
    raise (Api_errors.Server_error (Api_errors.mac_invalid, [mAC]));

  let mAC = String.lowercase mAC in (* just a convention *)
  let t = make_tables ~__context ~host in
  
  (* Assert that a local PIF with the given device name does not already exist *)
  let existing_pifs = List.map snd t.mac_to_pif_table in
  List.iter (fun pif -> if Db.PIF.get_device ~__context ~self:pif = device then 
    raise (Api_errors.Server_error(Api_errors.duplicate_pif_device_name, [device]))
  ) existing_pifs;
  
  (* Make sure the MAC exists *)
  let devices = Netdev.get_by_address mAC in
  match List.filter Netdev.is_physical devices with
  | [] -> raise (Api_errors.Server_error(Api_errors.mac_does_not_exist, [ mAC ]))
  | [ device' ] ->
      if List.mem_assoc mAC t.mac_to_pif_table
      then List.assoc mAC t.mac_to_pif_table
      else begin
	info "Introducing PIF MAC = %s; current device = %s; requested device = %s" mAC device' device;
	let mTU = Int64.of_string (Netdev.get_mtu device') in
	introduce_internal ~t ~__context ~host ~mAC ~device ~mTU ~vLAN:(-1L) ~vLAN_master_of:Ref.null ()
      end
  | _ -> failwith (Printf.sprintf "Multiple physical NICs with MAC %s found" mAC)

let forget ~__context ~self = 
  assert_not_in_bond ~__context ~self;
  assert_no_vlans ~__context ~self;
  assert_no_tunnels ~__context ~self;
  assert_not_slave_management_pif ~__context ~self; 
  assert_no_protection_enabled ~__context ~self;

  let host = Db.PIF.get_host ~__context ~self in
  let t = make_tables ~__context ~host in
  forget_internal ~t ~__context ~self

let scan ~__context ~host = 
  let t = make_tables ~__context ~host in

  let existing_macs = List.map fst t.mac_to_pif_table in
  let physical_macs = List.map fst t.mac_to_phy_table in

  (* Create PIF records for the new interfaces *)
  List.iter
    (fun mac -> 
       let device = List.assoc mac t.mac_to_phy_table in
       let mTU = Int64.of_string (Netdev.get_mtu device) in
       let (_: API.ref_PIF) = introduce_internal ~t ~__context ~host ~mAC:mac ~mTU ~vLAN:(-1L) ~vLAN_master_of:Ref.null ~device () in ()
    ) (set_difference physical_macs existing_macs);

  (* Make sure the right PIF(s) are marked as management PIFs *)
  update_management_flags ~__context ~host

let scan_bios ~__context ~host =	
	let t = make_tables ~__context ~host in
	
	let existing_macs = List.map fst t.mac_to_pif_table in
	let physical_macs = List.map fst t.mac_to_biosname_table in
	
	(* Create PIF records for the new interfaces *)
	let new_pifs =
		List.map (fun mac -> 
			let device = List.assoc mac t.mac_to_phy_table in
			let device' = List.assoc mac t.mac_to_biosname_table in
			let mTU = Int64.of_string (Netdev.get_mtu device) in
			introduce_internal ~t ~__context ~host ~mAC:mac ~mTU ~vLAN:(-1L) ~vLAN_master_of:Ref.null ~device:device' ()
		) (set_difference physical_macs existing_macs)
	in
	new_pifs

(* DEPRECATED! Rewritten to use VLAN.create. *)
let create_VLAN ~__context ~device ~network ~host ~vLAN =
	(* Find the "tagged PIF" (same device, no VLAN tag) *)
	let other_pifs = Db.Host.get_PIFs ~__context ~self:host in
	let base_pifs = List.filter (fun self -> 
		Db.PIF.get_device ~__context ~self = device
		&& (Db.PIF.get_VLAN ~__context ~self = (-1L))) other_pifs in
	if List.length base_pifs = 0
	then raise (Api_errors.Server_error (Api_errors.invalid_value, [ "device"; device ]));
	let tagged_PIF = List.hd base_pifs in
	let vlan = Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			Client.Client.VLAN.create rpc session_id tagged_PIF vLAN network
		) in
	Db.VLAN.get_untagged_PIF ~__context ~self:vlan

(* DEPRECATED! Rewritten to use VLAN.destroy. *)
let destroy ~__context ~self =
	if Db.PIF.get_VLAN ~__context ~self < 0L 
	then raise (Api_errors.Server_error (Api_errors.pif_is_physical, []));
	let vlan = Db.PIF.get_VLAN_master_of ~__context ~self in
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			Client.Client.VLAN.destroy rpc session_id vlan
		)

let reconfigure_ip ~__context ~self ~mode ~iP ~netmask ~gateway ~dNS =
  assert_no_protection_enabled ~__context ~self;

  let assert_is_valid ip_addr =
    try ignore (Unix.inet_addr_of_string ip_addr); true with _ -> false in
  if mode=`Static then
    begin
      (* require these parameters if mode is static *)
      if not (assert_is_valid iP) then raise (Api_errors.Server_error (Api_errors.invalid_ip_address_specified, [ "IP" ]));
      if not (assert_is_valid netmask) then raise (Api_errors.Server_error (Api_errors.invalid_ip_address_specified, [ "netmask" ]));
    end;
  (* for all IP parameters, if they're not empty then check they contain valid IP address *)
  List.iter
    (fun (param,value)->
       if value<>"" && (not (assert_is_valid value)) then
	 raise (Api_errors.Server_error (Api_errors.invalid_ip_address_specified, [ param ])))
    ["IP",iP; "netmask",netmask; "gateway",gateway]; (* Do NOT check DNS is a valid IP cos it can be a number of things, including a list of IPs separated by commas *)
  (* If this is a management PIF, make sure the IP config mode isn't None *)
  let management=Db.PIF.get_management ~__context ~self in

  if management && mode == `None
  then raise (Api_errors.Server_error(Api_errors.pif_is_management_iface, [ Ref.string_of self ]));

  Db.PIF.set_ip_configuration_mode ~__context ~self ~value:mode;
  Db.PIF.set_IP ~__context ~self ~value:iP;
  Db.PIF.set_netmask ~__context ~self ~value:netmask;
  Db.PIF.set_gateway ~__context ~self ~value:gateway;
  Db.PIF.set_DNS ~__context ~self ~value:dNS;
  if Db.PIF.get_currently_attached ~__context ~self then begin
    debug "PIF %s is currently_attached and the configuration has changed; calling out to reconfigure"
      (Db.PIF.get_uuid ~__context ~self);
    Db.PIF.set_currently_attached ~__context ~self ~value:false;
    Nm.bring_pif_up ~__context ~management_interface:management self;
  end;
  (* We kick the monitor thread to resync the dom0 device state with the PIF db record; this fixes a race
     where the you do a PIF.reconfigure_ip to set mode=dhcp, but you have already got an IP on the dom0
     device (e.g. because it's a management i/f that was brought up independently by init scripts) *)
  mark_pif_as_dirty (Db.PIF.get_device ~__context ~self) (Db.PIF.get_VLAN ~__context ~self)

let rec unplug ~__context ~self = 
  assert_no_protection_enabled ~__context ~self;
  assert_not_management_pif ~__context ~self;
  let host = Db.PIF.get_host ~__context ~self in
  if Db.Host.get_enabled ~__context ~self:host
  then abort_if_network_attached_to_protected_vms ~__context ~self;

  let network = Db.PIF.get_network ~__context ~self in
  let tunnel = Db.PIF.get_tunnel_transport_PIF_of ~__context ~self in
  if tunnel <> [] then begin
    debug "PIF is tunnel transport PIF... also bringing down access PIF";
    let tunnel = List.hd tunnel in
    let access_PIF = Db.Tunnel.get_access_PIF ~__context ~self:tunnel in
    unplug ~__context ~self:access_PIF
  end;
  Nm.bring_pif_down ~__context self

let rec plug ~__context ~self =
  let network = Db.PIF.get_network ~__context ~self in
  let host = Db.PIF.get_host ~__context ~self in
  let tunnel = Db.PIF.get_tunnel_access_PIF_of ~__context ~self in
  if tunnel <> [] then begin
    let tunnel = List.hd tunnel in
    let transport_PIF = Db.Tunnel.get_transport_PIF ~__context ~self:tunnel in
    if Db.PIF.get_ip_configuration_mode ~__context ~self:transport_PIF = `None then
      raise (Api_errors.Server_error (Api_errors.transport_pif_not_configured, [Ref.string_of transport_PIF]))
    else begin
      debug "PIF is tunnel access PIF... also bringing up transport PIF";
      plug ~__context ~self:transport_PIF
    end
  end;
  Xapi_network.attach ~__context ~network ~host
   
let calculate_pifs_required_at_start_of_day ~__context =
	let localhost = Helpers.get_localhost ~__context in
	List.filter (fun (_,pifr) -> 
		true &&
		pifr.API.pIF_host = localhost && (* this host only *)
		Nm.is_dom0_interface pifr &&
		not (Db.is_valid_ref __context pifr.API.pIF_bond_slave_of) (* not enslaved by a bond *)
  )
    (Db.PIF.get_all_records ~__context)

let start_of_day_best_effort_bring_up() = begin
  debug "Configured network backend: %s" (Netdev.string_of_kind Netdev.network.Netdev.kind);
  Server_helpers.exec_with_new_task "Bringing up physical PIFs"
    (fun __context ->
       List.iter
	 (fun (pif,pifr) ->
	    Helpers.log_exn_continue (Printf.sprintf "error trying to bring up pif: %s" pifr.API.pIF_uuid)
	      (fun pif -> debug "Best effort attempt to bring up PIF: %s" pifr.API.pIF_uuid;
		 plug ~__context ~self:pif) pif) 
	 (calculate_pifs_required_at_start_of_day ~__context)
    )
end
