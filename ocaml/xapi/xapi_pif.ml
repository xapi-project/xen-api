module D=Debug.Debugger(struct let name="xapi" end)
open D

module L = Debug.Debugger(struct let name="license" end) 

open Stringext

let bridge_naming_convention (device: string) =
  if String.startswith "eth" device
  then ("xenbr" ^ (String.sub device 3 (String.length device - 3)))
  else ("br" ^ device)

(** Returns the set of PIF references + records which we want to be plugged in by the end of the
    start of day code. Those for which 'disallow_unplug' is true and the management interface will
    actually be brought up ahead of time by the init scripts, so we don't have to plug them in.
    These are written to the xensource-inventory file when HA is enabled so that HA can bring up interfaces
    required by storage NICs etc. - CA-21416 
*)
let calculate_pifs_required_at_start_of_day ~__context =
  List.filter (fun (_,pifr) -> 
    true
    && (pifr.API.pIF_host = !Xapi_globs.localhost_ref) (* this host only *)
    && not (Db.is_valid_ref pifr.API.pIF_bond_slave_of) (* not enslaved by a bond *)
  )
    (Db.PIF.get_all_records ~__context)
  
let calculate_pifs_required_by_ha ~__context = 
  List.filter (fun (_,pifr) -> pifr.API.pIF_disallow_unplug || pifr.API.pIF_management) (calculate_pifs_required_at_start_of_day ~__context)

let read_ha_bridges_from_inventory () =
  try String.split ' ' (Xapi_inventory.lookup Xapi_inventory._ha_interfaces) with _ -> []

let assert_not_ha_pif ~__context ~self =
  let ha_bridges = read_ha_bridges_from_inventory () in
  let bridge = Db.Network.get_bridge ~__context ~self:(Db.PIF.get_network ~__context ~self) in
  if List.mem bridge ha_bridges then raise (Api_errors.Server_error (Api_errors.pif_does_not_allow_unplug, [Ref.string_of self]))

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

let assert_not_ha_heartbeat_pif ~__context ~self = 
  (* If HA is enabled then refuse to reconfigure the interface at all *)
  let pool = List.hd (Db.Pool.get_all ~__context) in
  if Db.Pool.get_ha_enabled ~__context ~self:pool && (Db.PIF.get_management ~__context ~self) 
  then raise (Api_errors.Server_error(Api_errors.ha_is_enabled, []))

let abort_if_network_attached_to_protected_vms ~__context ~self = 
  (* Abort a PIF.unplug if the Network has VIFs connected to protected VMs *)
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool && not(Db.Pool.get_ha_allow_overcommit ~__context ~self:pool) then begin
    let host = Db.PIF.get_host ~__context ~self in
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

(** Make sure no other PIFs connect this host to this network *)
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
      ~name_description:""
      ~bridge ~other_config:[] ~blobs:[] ~tags:[] in
    net_ref

(** Compute the set difference a - b *)
let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

(** Convenient lookup tables for scanning etc *)
type tables = { mac_to_pif_table: (string * API.ref_PIF) list;
		mac_to_phy_table: (string * string) list }

let make_tables ~__context ~host = 
  (* Enumerate known MAC addresses *)
  let existing = List.filter (fun self -> Db.PIF.get_physical ~__context ~self) (Db.Host.get_PIFs ~__context ~self:host) in
  let existing_macs = List.map (fun self -> Db.PIF.get_MAC ~__context ~self) existing in
  let mac_to_pif_table = List.combine existing_macs existing in

  (* Enumerate the MAC addresses which really exist in hardware *)
  let physical = List.filter Netdev.is_physical (Netdev.list ()) in
  let physical_macs = List.map Netdev.get_address physical in
  let mac_to_phy_table = List.combine physical_macs physical in

  { mac_to_pif_table = mac_to_pif_table;
    mac_to_phy_table = mac_to_phy_table }

(** Return true if this PIF is my management interface, according to xensource-inventory *)
let is_my_management_pif ~__context ~self = 
  let net = Db.PIF.get_network ~__context ~self in
  let management_if = Xapi_inventory.lookup Xapi_inventory._management_interface in
  Db.Network.get_bridge ~__context ~self:net = management_if

(** Make a new metrics objects and return reference to it *)
let make_pif_metrics ~__context =
  let metrics = Ref.make () and metrics_uuid = Uuid.to_string (Uuid.make_uuid ()) in
  let () = Db.PIF_metrics.create ~__context ~ref:metrics ~uuid:metrics_uuid ~carrier:false
    ~device_name:"" ~vendor_name:"" ~device_id:"" ~vendor_id:""
    ~speed:0L ~duplex:false ~pci_bus_path:""
    ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.) ~other_config:[] in
  metrics

(* Pool_introduce is an internal call used by pool-join to copy slave-to-be pif records to pool master *)
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

(* Perform a database delete on the master *)
let db_forget ~__context ~self = Db.PIF.destroy ~__context ~self

(* This signals the monitor thread to tell it that it should write to the database to sync it with the current
   dom0 networking config. *)
let mark_pif_as_dirty device vLAN =
  Threadext.Mutex.execute Rrd_shared.mutex
    (fun () ->
       Rrd_shared.dirty_pifs := Rrd_shared.StringSet.add (Helpers.get_dom0_network_device_name device vLAN) !Rrd_shared.dirty_pifs;
       Condition.broadcast Rrd_shared.condition
    )


(* Internal 'introduce' is passed a pre-built table 't' *)
let introduce_internal ?network ?(physical=true) ~t ~__context ~host ~mAC ~mTU ~device ~vLAN ~vLAN_master_of () = 
  let is_vlan = vLAN >= 0L in

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

  (* If I'm a slave and this pif represents my management interface then
     leave it alone: if the interface goes down (through a call to "up" then
     I loose my connection to the master's database and the call to "up"
     (which uses the API and requires the database) blocks until the slave
     restarts in emergency mode *)
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

(* Internal 'forget' is passed a pre-built table 't' *)
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

(* Look over all this host's PIFs and reset the management flag *)
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

(** Create a new PIF record for a new NIC *)
let introduce ~__context ~host ~mAC ~device =
  if not (Helpers.is_valid_MAC mAC) then
    raise (Api_errors.Server_error (Api_errors.mac_invalid, [mAC]));

  let mAC = String.lowercase mAC in (* just a convention *)
  let t = make_tables ~__context ~host in
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

(** Destroy the PIF record if the NIC really has gone *)
let forget ~__context ~self = 
  assert_not_in_bond ~__context ~self;
  assert_no_vlans ~__context ~self;
  assert_not_slave_management_pif ~__context ~self; 
  assert_not_ha_pif ~__context ~self;

  let host = Db.PIF.get_host ~__context ~self in
  let t = make_tables ~__context ~host in
  forget_internal ~t ~__context ~self

(** Scan for physical interfaces on this host and ensure PIF records are present
    and up-to-date *)
let scan ~__context ~host = 
  let t = make_tables ~__context ~host in

  let existing_macs = List.map fst t.mac_to_pif_table in
  let physical_macs = List.map fst t.mac_to_phy_table in

  (* Filter out the interfaces that are enslaved as part of bonds: CA-12690 *)
  let physical_macs =
    List.filter
      (fun mac ->
	 let device = List.assoc mac t.mac_to_phy_table in
	 let is_enslaved = try Unix.access ("/sys/class/net/"^device^"/master") [Unix.F_OK]; true with _ -> false in
	 not is_enslaved)
      physical_macs in

  (* Create PIF records for the new interfaces *)
  List.iter
    (fun mac -> 
       let device = List.assoc mac t.mac_to_phy_table in
       let mTU = Int64.of_string (Netdev.get_mtu device) in
       let (_: API.ref_PIF) = introduce_internal ~t ~__context ~host ~mAC:mac ~mTU ~vLAN:(-1L) ~vLAN_master_of:Ref.null ~device () in ()
    ) (set_difference physical_macs existing_macs);

  (* Make sure the right PIF(s) are marked as management PIFs *)
  update_management_flags ~__context ~host

(* Dummy MAC used by the VLAN *)
let vlan_mac = "fe:ff:ff:ff:ff:ff"

(* Used both externally and by PIF.create_VLAN *)
let vLAN_create ~__context ~tagged_PIF ~tag ~network = 
  let rstr = Restrictions.get () in
  if not(rstr.Restrictions.enable_vlans) then begin
    L.info "VLAN create restricted by license";
    raise (Api_errors.Server_error(Api_errors.license_restriction, []))
  end;
  let host = Db.PIF.get_host ~__context ~self:tagged_PIF in
  assert_no_other_local_pifs ~__context ~host ~network;
  
  (* Check that the tagged PIF is not a VLAN itself - CA-25160. This check can be skipped using the allow_vlan_on_vlan FIST point. *)
  let origtag = Db.PIF.get_VLAN ~__context ~self:tagged_PIF in
  if origtag >= 0L && not (Xapi_fist.allow_vlan_on_vlan()) then raise (Api_errors.Server_error (Api_errors.pif_is_vlan, [Ref.string_of tagged_PIF]));
 
  (* Check the VLAN tag is sensible;  4095 is reserved for implementation use (802.1Q) *)
  if tag<0L || tag>4094L 
  then raise (Api_errors.Server_error (Api_errors.vlan_tag_invalid, [Int64.to_string tag]));

  let other_pifs = Db.Host.get_PIFs ~__context ~self:host in
  let other_keys = List.map (fun self -> 
			       Db.PIF.get_device ~__context ~self,
			       Db.PIF.get_VLAN ~__context ~self) other_pifs in
  let device = Db.PIF.get_device ~__context ~self:tagged_PIF in
  if List.mem (device, tag) other_keys
  then raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [device]));

  (* Copy the MTU from the base PIF *)
  let mTU = Db.PIF.get_MTU ~__context ~self:tagged_PIF in

  let t = make_tables ~__context ~host in
  let vlan = Ref.make () and vlan_uuid = Uuid.to_string (Uuid.make_uuid ()) in

  (* NB we attach the untagged PIF to the supplied network *)
  let untagged_PIF = introduce_internal ~physical:false ~t ~__context ~host ~mAC:vlan_mac ~device ~vLAN:tag ~mTU ~vLAN_master_of:vlan ~network () in
  let () = Db.VLAN.create ~__context ~ref:vlan ~uuid:vlan_uuid ~tagged_PIF ~untagged_PIF ~tag ~other_config:[] in
  vlan

(** External facing call to create a new VLAN interface *)
(** DEPRECATED since Miami; use VLAN.create instead *)
let create_VLAN ~__context ~device ~network ~host ~vLAN =
  (* Find the "base PIF" (same device, no VLAN tag) *)
  let other_pifs = Db.Host.get_PIFs ~__context ~self:host in
  let base_pifs = List.filter (fun self -> 
				 Db.PIF.get_device ~__context ~self = device
			       && (Db.PIF.get_VLAN ~__context ~self = (-1L))) other_pifs in
  if List.length base_pifs = 0
  then raise (Api_errors.Server_error (Api_errors.invalid_value, [ "device"; device ]));
  let base_pif = List.hd base_pifs in

  let vlan = vLAN_create ~__context ~tagged_PIF:base_pif ~tag:vLAN ~network in
  Db.VLAN.get_untagged_PIF ~__context ~self:vlan

(** External facing call to destroy a VLAN interface *)
(** DEPRECATED since Miami; use VLAN.create instead *)
let destroy ~__context ~self =
  debug "PIF.destroy uuid = %s" (Db.PIF.get_uuid ~__context ~self);
  assert_not_in_bond ~__context ~self;
  assert_not_slave_management_pif ~__context ~self; 
  assert_not_ha_heartbeat_pif ~__context ~self;
  assert_not_ha_pif ~__context ~self;

  if Db.PIF.get_VLAN ~__context ~self < 0L 
  then raise (Api_errors.Server_error (Api_errors.pif_is_physical, []));
  (* Because of the precondition in create_VLAN, this will always be the only PIF
     connecting this host to the network. Therefore it is safe to detach the network. *)
  let network = Db.PIF.get_network ~__context ~self in
  let bridge = Db.Network.get_bridge ~__context ~self:network in

  Nm.bring_pif_down ~__context self;

  Xapi_network.detach bridge;

  (try
     let metrics = Db.PIF.get_metrics ~__context ~self in
     Db.PIF_metrics.destroy ~__context ~self:metrics with _ -> ());
  (try
     let vlan = Db.PIF.get_VLAN_master_of ~__context ~self in
     Db.VLAN.destroy ~__context ~self:vlan with _ -> ());
  Db.PIF.destroy ~__context ~self

(** External facing call to destroy a VLAN mux/demuxer *)
let vLAN_destroy ~__context ~self =
  debug "VLAN.destroy uuid = %s" (Db.VLAN.get_uuid ~__context ~self);
  let untagged_PIF = Db.VLAN.get_untagged_PIF ~__context ~self in
  (* Check if the untagged_PIF exists, if not we must be an orphaned record *)
  if try ignore(Db.PIF.get_uuid ~__context ~self:untagged_PIF); false with _ -> true then begin
    warn "VLAN's untagged PIF doesn't exist -- orphaned record?";
    Db.VLAN.destroy ~__context ~self
  end else begin    
    debug "untagged PIF uuid = %s" (Db.PIF.get_uuid ~__context ~self:untagged_PIF);
    (* Side-effect of this is to destroy any VLAN object *)
    destroy ~__context ~self:untagged_PIF
  end

let reconfigure_ip ~__context ~self ~mode ~iP ~netmask ~gateway ~dNS =
  assert_not_ha_heartbeat_pif ~__context ~self;

  let assert_is_valid ip_addr =
    try Unix.inet_addr_of_string ip_addr; true with _ -> false in
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

let unplug ~__context ~self = 
  assert_not_ha_heartbeat_pif ~__context ~self;
  assert_not_slave_management_pif ~__context ~self;
  assert_not_ha_pif ~__context ~self;
  let host = Db.PIF.get_host ~__context ~self in
  if Db.Host.get_enabled ~__context ~self:host
  then abort_if_network_attached_to_protected_vms ~__context ~self;
  Nm.bring_pif_down ~__context self

let plug ~__context ~self =
  let network = Db.PIF.get_network ~__context ~self in
  let host = Db.PIF.get_host ~__context ~self in
  Xapi_network.attach ~__context ~network ~host

let start_of_day_best_effort_bring_up() =
  Server_helpers.exec_with_new_task "Bringing up physical PIFs"
    (fun __context ->
       List.iter
	 (fun (pif,pifr) ->
	    Helpers.log_exn_continue (Printf.sprintf "error trying to bring up pif: %s" pifr.API.pIF_uuid)
	      (fun pif -> debug "Best effort attempt to bring up PIF: %s" pifr.API.pIF_uuid;
		 plug ~__context ~self:pif) pif) 
	 (calculate_pifs_required_at_start_of_day ~__context)
    )
