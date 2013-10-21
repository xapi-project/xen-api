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
module D = Debug.Make (struct let name="xapi" end)
open D

module L = Debug.Make (struct let name="license" end)

open Db_filter_types
open Fun
open Listext
open Pervasiveext
open Stringext
open Threadext

open Network

let refresh_internal ~__context ~self =
	let device = Db.PIF.get_device ~__context ~self in
	let network = Db.PIF.get_network ~__context ~self in
	let bridge = Db.Network.get_bridge ~__context ~self:network in
	let dbg = Context.string_of_task __context in

	(* Update the specified PIF field in the database, if
	 * and only if a corresponding value can be read from
	 * the underlying network device and if that value is
	 * different from the current field value.
	 *)
	let maybe_update_database
			field_name get_field set_field get_value print_value =
		Opt.iter
			(fun value ->
				if value <> (get_field ~__context ~self)
				then begin
					debug "PIF %s %s <- %s"
						(Ref.string_of self)
						(field_name)
						(print_value value);
					set_field ~__context ~self ~value
				end)
			(Opt.of_exception (fun () -> get_value ())) in

	if Db.PIF.get_physical ~__context ~self then
		maybe_update_database "MAC"
			(Db.PIF.get_MAC)
			(Db.PIF.set_MAC)
			(fun () -> Net.Interface.get_mac dbg ~name:device)
			(id);
	maybe_update_database "MTU"
		(Db.PIF.get_MTU)
		(Db.PIF.set_MTU)
		(Int64.of_int ++ (fun () -> Net.Interface.get_mtu dbg ~name:bridge))
		(Int64.to_string)

let refresh ~__context ~host ~self =
	assert (host = Helpers.get_localhost ~__context);
	refresh_internal ~__context ~self

let refresh_all ~__context ~host =
	assert (host = Helpers.get_localhost ~__context);
	(* Only refresh physical or attached PIFs *)
	let pifs = Db.PIF.get_refs_where ~__context ~expr:(And (
		Eq (Field "host", Literal (Ref.string_of host)),
		Or (Eq (Field "physical", Literal "true"),
			Eq (Field "currently_attached", Literal "true"))
	)) in
	List.iter (fun self -> refresh_internal ~__context ~self) pifs

let bridge_naming_convention (device: string) =
	if String.startswith "eth" device
	then ("xenbr" ^ (String.sub device 3 (String.length device - 3)))
	else ("br" ^ device)

let read_bridges_from_inventory () =
	try
		String.split
			(' ')
			(Xapi_inventory.lookup Xapi_inventory._current_interfaces)
	with _ ->
		[]

let assert_not_in_bond ~__context ~self =
	(* Prevent bond slaves interfaces *)
	let bond = Db.PIF.get_bond_slave_of ~__context ~self in
	if
		try ignore (Db.Bond.get_uuid ~__context ~self:bond); true
		with _ -> false
	then raise (Api_errors.Server_error
		(Api_errors.pif_already_bonded,
			[ Ref.string_of self ]));
	(* Disallow for bond masters *)
	if Db.PIF.get_bond_master_of ~__context ~self <> []
	then raise (Api_errors.Server_error
		(Api_errors.pif_already_bonded,
			[ Ref.string_of self ]))

let assert_no_vlans ~__context ~self =
	(* Disallow if this is a base interface of any existing VLAN *)
	let vlans = Db.PIF.get_VLAN_slave_of ~__context ~self in
	debug "PIF %s assert_no_vlans = [ %s ]"
		(Db.PIF.get_uuid ~__context ~self)
		(String.concat "; " (List.map Ref.string_of vlans));
	if vlans <> []
	then begin
		debug "PIF has associated VLANs: [ %s ]"
			(String.concat
				("; ")
				(List.map
					(fun self -> Db.VLAN.get_uuid ~__context ~self)
					(vlans)));
	raise (Api_errors.Server_error
		(Api_errors.pif_vlan_still_exists,
			[ Ref.string_of self ]))
	end;
	(* Disallow if this is a derived interface of a VLAN *)
	if
		Db.PIF.get_VLAN ~__context ~self <> (-1L)
		&& not (Xapi_fist.allow_forget_of_vlan_pif ())
	then raise (Api_errors.Server_error
		(Api_errors.pif_vlan_still_exists,
			[ Ref.string_of self ]))

let assert_no_tunnels ~__context ~self =
	(* Disallow if this is a transport interface of any existing tunnel *)
	let tunnels =
		Db.PIF.get_tunnel_transport_PIF_of ~__context ~self in
	debug "PIF %s assert_no_tunnels = [ %s ]"
		(Db.PIF.get_uuid ~__context ~self)
		(String.concat "; " (List.map Ref.string_of tunnels));
	if tunnels <> []
	then begin
		debug "PIF has associated tunnels: [ %s ]"
			(String.concat
				("; ")
				(List.map
					(fun self -> Db.Tunnel.get_uuid ~__context ~self)
					(tunnels)));
	raise (Api_errors.Server_error
		(Api_errors.pif_tunnel_still_exists,
			[ Ref.string_of self ]))
	end;
	(* Disallow if this is an access interface of a tunnel *)
	if Db.PIF.get_tunnel_access_PIF_of ~__context ~self <> []
	then raise (Api_errors.Server_error
		(Api_errors.pif_tunnel_still_exists,
			[ Ref.string_of self ]))

let assert_not_management_pif ~__context ~self =
	if Db.PIF.get_currently_attached ~__context ~self
		&& Db.PIF.get_management ~__context ~self
	then raise (Api_errors.Server_error
		(Api_errors.pif_is_management_iface,
			[ Ref.string_of self ]))

let assert_not_slave_management_pif ~__context ~self =
	if true
		&& Pool_role.is_slave ()
		&& Db.PIF.get_currently_attached ~__context ~self
		&& Db.PIF.get_management ~__context ~self
	then raise (Api_errors.Server_error
		(Api_errors.pif_is_management_iface,
			[ Ref.string_of self ]))

let assert_no_protection_enabled ~__context ~self =
	(* If HA or redo-log is enabled and PIF is attached
	 * then refuse to reconfigure the interface at all *)
	if Db.PIF.get_currently_attached ~__context ~self
	then begin
		let pool = List.hd (Db.Pool.get_all ~__context) in
		if Db.Pool.get_ha_enabled ~__context ~self:pool
		then raise (Api_errors.Server_error
			(Api_errors.ha_is_enabled, []))
		else if Db.Pool.get_redo_log_enabled ~__context ~self:pool
		then raise (Api_errors.Server_error
			(Api_errors.redo_log_is_enabled, []))
	end

let abort_if_network_attached_to_protected_vms ~__context ~self =
	(* Abort a PIF.unplug if the Network
	 * has VIFs connected to protected VMs *)
	let pool = Helpers.get_pool ~__context in
	if Db.Pool.get_ha_enabled ~__context ~self:pool
		&& not (Db.Pool.get_ha_allow_overcommit ~__context ~self:pool)
	then begin
		let net = Db.PIF.get_network ~__context ~self in
		let vifs = Db.Network.get_VIFs ~__context ~self:net in
		let vms = List.map
			(fun vif -> Db.VIF.get_VM ~__context ~self:vif)
			(vifs) in
		List.iter
			(fun vm ->
				if Helpers.is_xha_protected ~__context ~self:vm
				then begin
					warn
						"PIF.unplug will make protected VM %s not agile since it has a VIF attached to network %s"
						(Ref.string_of vm)
						(Ref.string_of net);
					raise (Api_errors.Server_error
						(Api_errors.ha_operation_would_break_failover_plan,
							[]))
				end)
			(vms)
	end

let assert_no_other_local_pifs ~__context ~host ~network =
	let other_pifs = Db.PIF.get_refs_where ~__context ~expr:(And (
		Eq (Field "network", Literal (Ref.string_of network)),
		Eq (Field "host", Literal (Ref.string_of host))
	)) in
	if other_pifs <> []
	then raise (Api_errors.Server_error
		(Api_errors.network_already_connected,
			[Ref.string_of host; Ref.string_of (List.hd other_pifs)]))

let find_or_create_network (bridge: string) (device: string) ~__context =
	let nets = Db.Network.get_refs_where ~__context ~expr:(Eq (Field "bridge", Literal bridge)) in
	match nets with
	| [net] -> net
	| _ ->
		let net_ref = Ref.make ()
		and net_uuid = Uuid.to_string (Uuid.make_uuid ()) in
		let () = Db.Network.create
			~__context ~ref:net_ref ~uuid:net_uuid
			~current_operations:[] ~allowed_operations:[]
			~name_label:(Helpers.choose_network_name_for_pif device)
			~name_description:"" ~mTU:1500L
			~bridge ~other_config:[] ~blobs:[]
			~tags:[] ~default_locking_mode:`unlocked
		in
		net_ref

type tables = {
	device_to_mac_table: (string * string) list;
	pif_to_device_table: (API.ref_PIF * string) list;
}

let make_tables ~__context ~host =
	let dbg = Context.string_of_task __context in
	let devices =
		List.filter
			(fun name -> Net.Interface.is_physical dbg ~name)
			(Net.Interface.get_all dbg ()) in
	let pifs = Db.PIF.get_records_where ~__context
		~expr:(And (Eq (Field "host", Literal (Ref.string_of host)),
			Eq (Field "physical", Literal "true"))) in
	{
		device_to_mac_table =
			List.combine
				(devices)
				(List.map (fun name -> Net.Interface.get_mac dbg ~name) devices);
		pif_to_device_table =
			List.map (fun (pref, prec) -> pref, prec.API.pIF_device) pifs;
	}

let is_my_management_pif ~__context ~self =
	let net = Db.PIF.get_network ~__context ~self in
	let management_if =
		Xapi_inventory.lookup Xapi_inventory._management_interface in
	Db.Network.get_bridge ~__context ~self:net = management_if

let make_pif_metrics ~__context =
	let metrics = Ref.make ()
	and metrics_uuid = Uuid.to_string (Uuid.make_uuid ()) in
	let () = Db.PIF_metrics.create
		~__context ~ref:metrics ~uuid:metrics_uuid ~carrier:false
		~device_name:"" ~vendor_name:"" ~device_id:"" ~vendor_id:""
		~speed:0L ~duplex:false ~pci_bus_path:""
		~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:(Date.of_float 0.)
		~other_config:[] in
	metrics

let pool_introduce
		~__context ~device ~network ~host
		~mAC ~mTU ~vLAN ~physical
		~ip_configuration_mode ~iP ~netmask ~gateway
		~dNS ~bond_slave_of ~vLAN_master_of ~management
		~other_config ~disallow_unplug ~ipv6_configuration_mode
		~iPv6 ~ipv6_gateway ~primary_address_type =
	let pif_ref = Ref.make () in
	let metrics = make_pif_metrics ~__context in
	let () =
		Db.PIF.create
			~__context ~ref:pif_ref ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
			~device ~device_name:device ~network ~host
			~mAC ~mTU ~vLAN ~metrics
			~physical ~currently_attached:false
			~ip_configuration_mode ~iP ~netmask ~gateway ~dNS
			~bond_slave_of:Ref.null ~vLAN_master_of ~management
			~other_config ~disallow_unplug ~ipv6_configuration_mode
			~iPv6 ~ipv6_gateway ~primary_address_type in
	pif_ref

let db_introduce = pool_introduce

let db_forget ~__context ~self = Db.PIF.destroy ~__context ~self

(* Internal [introduce] is passed a pre-built table [t] *)
let introduce_internal
		?network ?(physical=true) ~t ~__context ~host
		~mAC ~mTU ~device ~vLAN ~vLAN_master_of ?metrics () =

	let bridge = bridge_naming_convention device in

	(* If we are not told which network to use,
	 * apply the default convention *)
	let net_ref =
		match network with
			| None -> find_or_create_network bridge device ~__context
			| Some x -> x in
	let metrics = match metrics with
		| None -> make_pif_metrics ~__context
		| Some m -> m
	in
	let pif = Ref.make () in
	debug
		"Creating a new record for NIC: %s: %s"
		(device)
		(Ref.string_of pif);
	let () = Db.PIF.create
		~__context ~ref:pif ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
		~device ~device_name:device ~network:net_ref ~host ~mAC
		~mTU ~vLAN ~metrics ~physical ~currently_attached:false
		~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:""
		~dNS:"" ~bond_slave_of:Ref.null ~vLAN_master_of ~management:false
		~other_config:[] ~disallow_unplug:false ~ipv6_configuration_mode:`None
	        ~iPv6:[] ~ipv6_gateway:"" ~primary_address_type:`IPv4 in

	(* If I'm a pool slave and this pif represents my management
	 * interface then leave it alone: if the interface goes down
	 * (through a call to "up") then I lose my connection to the
	 * master's database and the call to "up" (which uses the API
	 * and requires the database) blocks until the slave restarts
	 * in emergency mode.
	 *)
	(* Rob: nothing seems to be done with the pool slave case
	 * mentioned in this comment...?
	 *)
	if is_my_management_pif ~__context ~self:pif
	then begin
		debug "NIC is the management interface";
		Db.PIF.set_management ~__context ~self:pif ~value:true;
		Db.PIF.set_currently_attached ~__context ~self:pif ~value:true;
	end;

	(* When a new PIF is introduced then we clear it from the cache w.r.t
	 * the monitor thread; this ensures that the PIF metrics (including
	 * carrier and vendor etc.) will eventually get updated [and that
	 * subsequent changes to this PIFs' device's dom0 configuration
	 * will be reflected accordingly]. *)
	Monitor_dbcalls.clear_cache_for_pif ~pif_name:device;

	(* return ref of newly created pif record *)
	pif

(* Internal [forget] is passed a pre-built table [t] *)
let forget_internal ~t ~__context ~self =
	Nm.bring_pif_down ~__context self;
	(* NB we are allowed to forget an interface which still exists *)
	let device = Db.PIF.get_device ~__context ~self in
	if List.mem_assoc device t.device_to_mac_table
	then warn "Forgetting PIF record even though device %s still exists" device;
	(try
		let metrics = Db.PIF.get_metrics ~__context ~self in
		Db.PIF_metrics.destroy ~__context ~self:metrics with _ -> ());
	Db.PIF.destroy ~__context ~self

let update_management_flags ~__context ~host =
	try
		let management_bridge = Xapi_inventory.lookup Xapi_inventory._management_interface in
		let management_networks = Db.Network.get_refs_where ~__context ~expr:(
			Eq (Field "bridge", Literal management_bridge)
		) in
		let current_management_pifs =
			match management_networks with
			| [] -> []
			| net :: _ ->
				Db.PIF.get_refs_where ~__context ~expr:(And (
					Eq (Field "host", Literal (Ref.string_of host)),
					Eq (Field "network", Literal (Ref.string_of net))
				))
		in
		let management_pifs_in_db = Db.PIF.get_refs_where ~__context ~expr:(And (
			Eq (Field "host", Literal (Ref.string_of host)),
			Eq (Field "management", Literal "true")
		)) in
		let set_management value self =
			debug "PIF %s management <- %b" (Ref.string_of self) value;
			Db.PIF.set_management ~__context ~self ~value
		in
		(* Set management flag of PIFs that are now management PIFs, and do not have this flag set *)
		List.iter (set_management true) (List.set_difference current_management_pifs management_pifs_in_db);
		(* Clear management flag of PIFs that are no longer management PIFs *)
		List.iter (set_management false) (List.set_difference management_pifs_in_db current_management_pifs)
	with Xapi_inventory.Missing_inventory_key _ ->
		error "Missing field MANAGEMENT_INTERFACE in inventory file"

let introduce ~__context ~host ~mAC ~device =

	let mAC = String.lowercase mAC in (* just a convention *)
	let t = make_tables ~__context ~host in
	let dbg = Context.string_of_task __context in

	(* Allow callers to omit the MAC address. Ideally, we should
	 * use an option type (instead of treating the empty string
	 * as a special value). However we must preserve the existing
	 * function signature as it appears in the published API.
	 *)
	let mAC =
		if mAC = ""
		then List.assoc_default device t.device_to_mac_table ""
		else mAC in

	if not (Helpers.is_valid_MAC mAC)
	then raise (Api_errors.Server_error
		(Api_errors.mac_invalid, [mAC]));

	(* Assert that a local PIF with the given device name does not already exist *)
	if List.mem device (List.map snd t.pif_to_device_table)
	then raise (Api_errors.Server_error
		(Api_errors.duplicate_pif_device_name, [device]));

	(* Assert that a network interface exists with *
	 * the specified device name and MAC address.  *)
	if not (List.mem (device, mAC) t.device_to_mac_table)
	then raise (Api_errors.Server_error (Api_errors
		.could_not_find_network_interface_with_specified_device_name_and_mac_address,
		[device; mAC]));

	info
		"Introducing PIF: device = %s; MAC = %s"
		device mAC;
	let mTU = Int64.of_int (Net.Interface.get_mtu dbg ~name:device) in
	introduce_internal
		~t ~__context ~host ~mAC ~device ~mTU
		~vLAN:(-1L) ~vLAN_master_of:Ref.null ()

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
	let dbg = Context.string_of_task __context in

	refresh_all ~__context ~host;
	let devices_not_yet_represented_by_pifs =
		List.set_difference
			(List.map fst t.device_to_mac_table)
			(List.map snd t.pif_to_device_table) in

	(* Create PIF records for the new interfaces *)
	List.iter
		(fun device ->
			let mAC = List.assoc device t.device_to_mac_table in
			let mTU = Int64.of_int (Net.Interface.get_mtu dbg ~name:device) in
			let (_: API.ref_PIF) =
				introduce_internal
					~t ~__context ~host ~mAC ~mTU ~vLAN:(-1L)
					~vLAN_master_of:Ref.null ~device () in
			())
		(devices_not_yet_represented_by_pifs);

	(* Make sure the right PIF(s) are marked as management PIFs *)
	update_management_flags ~__context ~host

(* DEPRECATED! Rewritten to use VLAN.create. *)
let create_VLAN ~__context ~device ~network ~host ~vLAN =
	(* Find the "tagged PIF" (same device, no VLAN tag) *)
	let other_pifs = Db.Host.get_PIFs ~__context ~self:host in
	let base_pifs =
		List.filter
			(fun self ->
				(Db.PIF.get_device ~__context ~self = device)
					&&
				(Db.PIF.get_VLAN ~__context ~self = (-1L)))
			(other_pifs) in
	if List.length base_pifs = 0
	then raise (Api_errors.Server_error
		(Api_errors.invalid_value, [ "device"; device ]));
	let tagged_PIF = List.hd base_pifs in
	let vlan = Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			Client.Client.VLAN.create
				rpc session_id tagged_PIF vLAN network) in
	Db.VLAN.get_untagged_PIF ~__context ~self:vlan

(* DEPRECATED! Rewritten to use VLAN.destroy. *)
let destroy ~__context ~self =
	if Db.PIF.get_VLAN ~__context ~self < 0L
	then raise (Api_errors.Server_error (Api_errors.pif_is_physical, []));
	let vlan = Db.PIF.get_VLAN_master_of ~__context ~self in
	Helpers.call_api_functions ~__context
		(fun rpc session_id ->
			Client.Client.VLAN.destroy rpc session_id vlan)

let is_valid_ip addr =
	try ignore (Unix.inet_addr_of_string addr); true with _ -> false

let reconfigure_ipv6 ~__context ~self ~mode ~iPv6 ~gateway ~dNS =
	assert_no_protection_enabled ~__context ~self;
		
	if gateway <> "" && (not (is_valid_ip gateway)) then
		raise (Api_errors.Server_error (Api_errors.invalid_ip_address_specified, ["gateway"]));

	(* If we have an IPv6 address, check that it is valid and a prefix length is specified *)
	if iPv6 <> "" then begin
		let index =
			try
				String.index iPv6 '/'
			with Not_found ->
				let msg = "Prefix length must be specified (format: <ipv6>/<prefix>" in
				raise (Api_errors.Server_error
					(Api_errors.invalid_ip_address_specified, [msg]))
		in
		let addr = String.sub iPv6 0 index in
		let prefix_len = String.sub iPv6 (index + 1) ((String.length iPv6) - index - 1) in
		if not (is_valid_ip addr) then
			raise (Api_errors.Server_error (Api_errors.invalid_ip_address_specified, ["IPv6"]));
		let pl_int =
			try
				int_of_string prefix_len
			with _ ->
				let msg = Printf.sprintf "Cannot parse prefix length '%s'" prefix_len in
				raise (Api_errors.Server_error
					(Api_errors.invalid_ip_address_specified, [msg]))
		in
		if (pl_int < 0 || pl_int > 128) then
			raise (Api_errors.Server_error
				(Api_errors.invalid_ip_address_specified, ["Prefix length must be between 0 and 128"]))
	end;

	(* Management iface must have an address for the primary address type *)
	let management = Db.PIF.get_management ~__context ~self in
	let primary_address_type = Db.PIF.get_primary_address_type ~__context ~self in
	if management && mode = `None && primary_address_type = `IPv6 then
		raise (Api_errors.Server_error
			(Api_errors.pif_is_management_iface, [ Ref.string_of self ]));

	let old_mode = Db.PIF.get_ipv6_configuration_mode ~__context ~self in

	(* Set the values in the DB *)
	Db.PIF.set_ipv6_configuration_mode ~__context ~self ~value:mode;
	Db.PIF.set_ipv6_gateway ~__context ~self ~value:gateway;
	Db.PIF.set_IPv6 ~__context ~self ~value:[iPv6];
	if dNS <> "" then Db.PIF.set_DNS ~__context ~self ~value:dNS;

	if Db.PIF.get_currently_attached ~__context ~self then begin
		debug
			"PIF %s is currently_attached and the configuration has changed; calling out to reconfigure"
			(Db.PIF.get_uuid ~__context ~self);
		Db.PIF.set_currently_attached ~__context ~self ~value:false;
		Nm.bring_pif_up ~__context ~management_interface:management self;
		if mode = `DHCP || mode = `Autoconf then
			(* Refresh IP address fields in case dhclient was already running, and
			 * we are not getting a host-signal-networking-change callback. *)
			Helpers.update_pif_address ~__context ~self
	end;
	Monitor_dbcalls.clear_cache_for_pif ~pif_name:(Db.PIF.get_device ~__context ~self);
	if ((old_mode == `None && mode <> `None) || (old_mode <> `None && mode == `None)) then
	begin
		debug "IPv6 mode has changed - updating management interface";
		Xapi_mgmt_iface.rebind ~__context;
	end

let reconfigure_ip ~__context ~self ~mode ~iP ~netmask ~gateway ~dNS =
	assert_no_protection_enabled ~__context ~self;

	if mode=`Static
	then begin
		(* require these parameters if mode is static *)
		if not (is_valid_ip iP)
		then raise (Api_errors.Server_error
			(Api_errors.invalid_ip_address_specified, [ "IP" ]));
		if not (is_valid_ip netmask)
		then raise (Api_errors.Server_error
			(Api_errors.invalid_ip_address_specified, [ "netmask" ]));
	end;
	(* for all IP parameters, if they're not empty
	 * then check they contain valid IP address *)
	List.iter
		(fun (param,value)->
			if value <> "" && (not (is_valid_ip value))
			then raise (Api_errors.Server_error
				(Api_errors.invalid_ip_address_specified, [ param ])))
		["IP",iP; "netmask",netmask; "gateway",gateway];
	(* Do NOT check DNS is a valid IP cos it can be a number
	 * of things, including a list of IPs separated by commas
	 *)
	(* If this is a management PIF, make sure the IP config mode isn't None *)
	let management=Db.PIF.get_management ~__context ~self in
	let primary_address_type=Db.PIF.get_primary_address_type ~__context ~self in

	if management && mode = `None && primary_address_type=`IPv4
	then raise (Api_errors.Server_error
		(Api_errors.pif_is_management_iface, [ Ref.string_of self ]));

	Db.PIF.set_ip_configuration_mode ~__context ~self ~value:mode;
	Db.PIF.set_IP ~__context ~self ~value:iP;
	Db.PIF.set_netmask ~__context ~self ~value:netmask;
	Db.PIF.set_gateway ~__context ~self ~value:gateway;
	Db.PIF.set_DNS ~__context ~self ~value:dNS;
	if Db.PIF.get_currently_attached ~__context ~self
	then begin
		debug
			"PIF %s is currently_attached and the configuration has changed; calling out to reconfigure"
			(Db.PIF.get_uuid ~__context ~self);
		Db.PIF.set_currently_attached ~__context ~self ~value:false;
		Nm.bring_pif_up ~__context ~management_interface:management self;
		if mode = `DHCP then
			(* Refresh IP address fields in case dhclient was already running, and
			 * we are not getting a host-signal-networking-change callback. *)
			Helpers.update_pif_address ~__context ~self
	end;
	(* We clear the monitor thread's cache for the PIF to resync the dom0 device
	 * state with the PIF db record; this fixes a race where the you do a
	 * PIF.reconfigure_ip to set mode=dhcp, but you have already got an IP on
	 * the dom0 device (e.g. because it's a management i/f that was brought up
	 * independently by init scripts) *)
	Monitor_dbcalls.clear_cache_for_pif ~pif_name:(Db.PIF.get_device ~__context ~self)

let set_primary_address_type ~__context ~self ~primary_address_type =
	assert_no_protection_enabled ~__context ~self;

	let management=Db.PIF.get_management ~__context ~self in
	if management then raise (Api_errors.Server_error(Api_errors.pif_is_management_iface, [ Ref.string_of self ]));

	Db.PIF.set_primary_address_type ~__context ~self ~value:primary_address_type;
	Monitor_dbcalls.clear_cache_for_pif ~pif_name:(Db.PIF.get_device ~__context ~self)

let rec unplug ~__context ~self =
	assert_no_protection_enabled ~__context ~self;
	assert_not_management_pif ~__context ~self;
	let host = Db.PIF.get_host ~__context ~self in
	if Db.Host.get_enabled ~__context ~self:host
	then abort_if_network_attached_to_protected_vms ~__context ~self;

	let network = Db.PIF.get_network ~__context ~self in
	Xapi_network_attach_helpers.assert_network_has_no_vifs_in_use_on_me ~__context ~host:(Helpers.get_localhost ~__context) ~network;
	Xapi_network_attach_helpers.assert_pif_disallow_unplug_not_set ~__context self;

	let tunnel = Db.PIF.get_tunnel_transport_PIF_of ~__context ~self in
	if tunnel <> []
	then begin
		debug "PIF is tunnel transport PIF... also bringing down access PIF";
		let tunnel = List.hd tunnel in
		let access_PIF = Db.Tunnel.get_access_PIF ~__context ~self:tunnel in
		unplug ~__context ~self:access_PIF
	end;
	Nm.bring_pif_down ~__context self

let rec plug ~__context ~self =
	let tunnel = Db.PIF.get_tunnel_access_PIF_of ~__context ~self in
	if tunnel <> []
	then begin
		let tunnel = List.hd tunnel in
		let transport_PIF =
			Db.Tunnel.get_transport_PIF ~__context ~self:tunnel in
		if Db.PIF.get_ip_configuration_mode
			~__context ~self:transport_PIF = `None
		then raise (Api_errors.Server_error
			(Api_errors.transport_pif_not_configured,
				[Ref.string_of transport_PIF]))
		else begin
			debug "PIF is tunnel access PIF... also bringing up transport PIF";
			plug ~__context ~self:transport_PIF
		end
	end;
	if Db.PIF.get_bond_slave_of ~__context ~self <> Ref.null then
		raise (Api_errors.Server_error (Api_errors.cannot_plug_bond_slave, [Ref.string_of self]));
	Nm.bring_pif_up ~__context ~management_interface:false self

let calculate_pifs_required_at_start_of_day ~__context =
	let localhost = Helpers.get_localhost ~__context in
	(* Select all PIFs on the host that are not bond slaves, and are physical, or bond master, or
	 * have IP configuration. The latter means that any VLAN or tunnel PIFs without IP address
	 * are excluded. *)
	Db.PIF.get_records_where ~__context
		~expr:(
			And (
				And (
					Eq (Field "host", Literal (Ref.string_of localhost)),
					Eq (Field "bond_slave_of", Literal (Ref.string_of Ref.null))
				),
				Or (Or (
					Not (Eq (Field "bond_master_of", Literal "()")),
					Eq (Field "physical", Literal "true")),
					Not (Eq (Field "ip_configuration_mode", Literal "None"))
				)
			)
		)

let start_of_day_best_effort_bring_up () =
	begin
		Server_helpers.exec_with_new_task
			"Bringing up physical PIFs"
			(fun __context ->
				let dbg = Context.string_of_task __context in
				debug
					"Configured network backend: %s"
					(Network_interface.string_of_kind (Net.Bridge.get_kind dbg ()));
				(* Clear the state of the network daemon, before refreshing it by plugging
				 * the most important PIFs (see above). *)
				Net.clear_state ();
				List.iter
					(fun (pif, pifr) ->
						Helpers.log_exn_continue
							(Printf.sprintf
								"error trying to bring up pif: %s"
								pifr.API.pIF_uuid)
							(fun pif ->
								debug
									"Best effort attempt to bring up PIF: %s"
									pifr.API.pIF_uuid;
								plug ~__context ~self:pif)
							(pif))
					(calculate_pifs_required_at_start_of_day ~__context))
	end
