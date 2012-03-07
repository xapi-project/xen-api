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

open Stringext
open Listext
open Threadext
open Db_filter_types

let use_networkd = ref true

module Net = (val (Network.get_client ()) : Network.CLIENT)
open Network_interface

(* Protect a bunch of local operations with a mutex *)
let local_m = Mutex.create ()

let with_local_lock f = Mutex.execute local_m f

let is_dom0_interface pif_r = pif_r.API.pIF_ip_configuration_mode <> `None

(* Make sure inventory file has all current interfaces on the local host, so
 * they will all be brought up again at start up. *)
let update_inventory ~__context =
	let localhost = Helpers.get_localhost ~__context in
	let pifs = Db.PIF.get_records_where ~__context
		~expr:(And (And (Eq (Field "host", Literal (Ref.string_of localhost)),
			Eq (Field "currently_attached", Literal "true")),
			Not (Eq (Field "ip_configuration_mode", Literal "None")))) in
	let bridges = List.map (fun (_, pif_r) -> Db.Network.get_bridge ~__context ~self:pif_r.API.pIF_network) pifs in
	Xapi_inventory.update Xapi_inventory._current_interfaces (String.concat " " bridges)

let interface_reconfigure_script = Filename.concat Fhs.libexecdir "interface-reconfigure"

(* Call the interface reconfigure script. For development ignore the exn if it doesn't exist *)
let reconfigure_pif ~__context (pif: API.ref_PIF) args =
	try
		Helpers.call_api_functions ~__context (fun _ session_id ->
			let args = "--session" :: (Ref.string_of session_id) :: "--pif" :: (Ref.string_of pif) :: args in
			ignore(Helpers.call_script interface_reconfigure_script args)
		)
	with
	| Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n) ->
		raise (Api_errors.Server_error(Api_errors.pif_configuration_error, [ Ref.string_of pif; stderr ]))

let netmask_to_prefixlen netmask =
	Scanf.sscanf netmask "%d.%d.%d.%d" (fun a b c d ->
		let rec length l x =
			if x > 0 then
				length (succ l) (x lsr 1)
			else
				l
		in
		List.fold_left length 0 [a; b; c; d]
	)

let determine_mtu ~__context pif_rc bridge =
	if List.mem_assoc "mtu" pif_rc.API.pIF_other_config then
		int_of_string (List.assoc "mtu" pif_rc.API.pIF_other_config)
	else
		Int64.to_int (Db.Network.get_MTU ~__context ~self:pif_rc.API.pIF_network)

let get_fail_mode ~__context pif_rc =
	let fail_mode_of_string = function
		| "secure" -> Secure
		| "standalone" | _ -> Standalone
	in
	let oc = Db.Network.get_other_config ~__context ~self:pif_rc.API.pIF_network in
	if List.mem_assoc "vswitch-controller-fail-mode" oc then
		fail_mode_of_string (List.assoc "vswitch-controller-fail-mode" oc)
	else
		let oc = Db.Pool.get_other_config ~__context ~self:(Helpers.get_pool ~__context) in
		if List.mem_assoc "vswitch-controller-fail-mode" oc then
			fail_mode_of_string (List.assoc "vswitch-controller-fail-mode" oc)
		else
			Standalone

let create_bond ~__context bond mtu =
	(* Get all information we need from the DB before doing anything that may drop our
	 * management connection *)
	let master = Db.Bond.get_master ~__context ~self:bond in
	let master_rc = Db.PIF.get_record ~__context ~self:master in
	let slaves = Db.Bond.get_slaves ~__context ~self:bond in
	let slave_devices_and_bridges = List.map (fun pif ->
		let device = Db.PIF.get_device ~__context ~self:pif in
		let bridge =
			let network = Db.PIF.get_network ~__context ~self:pif in
			Db.Network.get_bridge ~__context ~self:network
		in
		device, bridge
	) slaves in
	let bridge = Db.Network.get_bridge ~__context ~self:master_rc.API.pIF_network in
	let props = Db.Bond.get_properties ~__context ~self:bond in
	let mode = Db.Bond.get_mode ~__context ~self:bond in
	let fail_mode = get_fail_mode ~__context master_rc in

	(* clean up bond slaves *)
	List.iter (fun (device, bridge) ->
		Net.Interface.set_ipv4_addr bridge None4;
		Net.Bridge.destroy ~force:true bridge;
		Net.Interface.set_persistent bridge false;
		Net.Interface.set_mtu device mtu
	) slave_devices_and_bridges;

	(* create bond bridge *)
	let port = master_rc.API.pIF_device in
	let mac = master_rc.API.pIF_MAC in
	Net.Bridge.create ~mac ~fail_mode bridge;
	Net.Bridge.add_port ~mac bridge port
		(List.map (fun (device, _) -> device) slave_devices_and_bridges);

	(* set bond properties *)
	let hashing_algorithm =
		if List.mem_assoc "hashing_algorithm" props then
			List.assoc "hashing_algorithm" props
		else
			"src_mac"
	in
	let props = [
		"mode", Record_util.bond_mode_to_string mode;
		"miimon", "100";
		"downdelay", "200";
		"updelay", "31000";
		"use_carrier", "1";
		"hashing-algorithm", hashing_algorithm
	] in
	let overrides = List.filter_map (fun (k, v) ->
		if String.startswith "bond-" k then
			Some ((String.sub_to_end k 5), v)
		else
			None
	) master_rc.API.pIF_other_config in
	(* add defaults for properties that are not overridden *)
	let props = (List.filter (fun (k, _) -> not (List.mem_assoc k overrides)) props) @ overrides in
	Net.Bridge.set_bond_properties bridge port props

let destroy_bond ~__context ~force bond =
	let master = Db.Bond.get_master ~__context ~self:bond in
	let bridge =
		let network = Db.PIF.get_network ~__context ~self:master in
		Db.Network.get_bridge ~__context ~self:network
	in
	Net.Interface.set_ipv4_addr bridge None4;
	Net.Bridge.destroy ~force bridge

let create_vlan ~__context vlan =
	let master = Db.VLAN.get_untagged_PIF ~__context ~self:vlan in
	let slave = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
	let bridge =
		let network = Db.PIF.get_network ~__context ~self:master in
		Db.Network.get_bridge ~__context ~self:network
	in
	let parent =
		let network = Db.PIF.get_network ~__context ~self:slave in
		Db.Network.get_bridge ~__context ~self:network
	in
	let tag = Int64.to_int (Db.VLAN.get_tag ~__context ~self:vlan) in
	let vlan_bug_workaround =
		let oc = Db.PIF.get_other_config ~__context ~self:master in
		if List.mem_assoc "vlan-bug-workaround" oc then
			Some (List.assoc "vlan-bug-workaround" oc = "true")
		else
			None
	in
	Net.Bridge.create ~vlan:(parent, tag) ?vlan_bug_workaround bridge

let destroy_vlan ~__context vlan =
	let master = Db.VLAN.get_untagged_PIF ~__context ~self:vlan in
	let bridge =
		let network = Db.PIF.get_network ~__context ~self:master in
		Db.Network.get_bridge ~__context ~self:network
	in
	Net.Interface.set_ipv4_addr bridge None4;
	Net.Bridge.destroy bridge

let get_bond pif_rc =
	match pif_rc.API.pIF_bond_master_of with
	| [] -> None
	| bond :: _ ->
		Some bond

let get_vlan pif_rc =
	if pif_rc.API.pIF_VLAN_master_of = Ref.null then
		None
	else
		Some pif_rc.API.pIF_VLAN_master_of

let get_tunnel pif_rc =
	if pif_rc.API.pIF_tunnel_access_PIF_of = [] then
		None
	else
		Some (List.hd pif_rc.API.pIF_tunnel_access_PIF_of)

let get_pif_type pif_rc =
	match get_vlan pif_rc with
	| Some vlan -> `vlan_pif vlan
	| None ->
		match get_bond pif_rc with
		| Some bond -> `bond_pif bond
		| None ->
			match get_tunnel pif_rc with
			| Some tunnel -> `tunnel_pif tunnel
			| None -> `phy_pif

let rec create_bridges ~__context pif_rc bridge =
	let mtu = determine_mtu ~__context pif_rc bridge in
	begin match get_pif_type pif_rc with
	| `tunnel_pif _ ->
		let fail_mode = get_fail_mode ~__context pif_rc in
		Net.Bridge.create ~mac:pif_rc.API.pIF_MAC ~fail_mode bridge;
	| `vlan_pif vlan ->
		let slave = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
		let rc = Db.PIF.get_record ~__context ~self:slave in
		let bridge = Db.Network.get_bridge ~__context ~self:rc.API.pIF_network in
		if not (Net.Interface.is_up bridge) then
			create_bridges ~__context rc bridge;
		create_vlan ~__context vlan
	| `bond_pif bond ->
		create_bond ~__context bond mtu;
		Net.Interface.set_mtu pif_rc.API.pIF_device mtu;
	| `phy_pif  ->
		let fail_mode = get_fail_mode ~__context pif_rc in
		if pif_rc.API.pIF_bond_slave_of <> Ref.null then
			destroy_bond ~__context ~force:true pif_rc.API.pIF_bond_slave_of;
		Net.Bridge.create ~mac:pif_rc.API.pIF_MAC ~fail_mode bridge;
		Net.Bridge.add_port bridge pif_rc.API.pIF_device [pif_rc.API.pIF_device];
		Net.Interface.set_mtu pif_rc.API.pIF_device mtu;
	end;
	Net.Interface.set_mtu bridge mtu

let rec destroy_bridges ~__context ~force pif_rc bridge =
	begin match get_pif_type pif_rc with
	| `tunnel_pif _ ->
		Net.Bridge.destroy bridge
	| `vlan_pif vlan ->
		destroy_vlan ~__context vlan;
		let slave = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
		let rc = Db.PIF.get_record ~__context ~self:slave in
		if not rc.API.pIF_currently_attached then
			let bridge = Db.Network.get_bridge ~__context ~self:rc.API.pIF_network in
			destroy_bridges ~__context ~force rc bridge
	| `bond_pif bond ->
		destroy_bond ~__context ~force bond
	| `phy_pif  ->
		Net.Bridge.destroy bridge
	end

(* Determine the gateway and DNS PIFs:
 * If one of the PIFs with IP has other_config:defaultroute=true, then
 * pick this one as gateway PIF. If there are multiple, pick a random one of these.
 * If there are none, then pick the management interface. If there is no management
 * interface, pick a random PIF.
 * Similarly for the DNS PIF, but with other_config:peerdns. *)
let determine_dhcp_options ~__context pif management_interface =
	let localhost = Helpers.get_localhost ~__context in
	let ip_pifs = Db.PIF.get_records_where ~__context
		~expr:(And (Eq (Field "host", Literal (Ref.string_of localhost)),
			Not (Eq (Field "ip_configuration_mode", Literal "None")))) in
	let gateway_pif =
		let oc = List.filter (fun (_, r) ->
			List.mem_assoc "defaultroute" r.API.pIF_other_config &&
			List.assoc "defaultroute" r.API.pIF_other_config = "true"
		) ip_pifs in
		match oc with
		| (p, r) :: _ ->
			warn "multiple PIFs with other_config:defaultroute=true - choosing %s" r.API.pIF_device;
			p
		| [] ->
			if management_interface then
				pif
			else
				let mgmt = List.filter (fun (_, r) -> r.API.pIF_management) ip_pifs in
				match mgmt with
				| (p, _) :: _ -> p
				| [] ->
					let (p, r) = List.hd ip_pifs in
					warn "no gateway PIF found - choosing %s" r.API.pIF_device;
					p
	in
	let dns_pif =
		let oc = List.filter (fun (_, r) ->
			List.mem_assoc "peerdns" r.API.pIF_other_config &&
			List.assoc "peerdns" r.API.pIF_other_config = "true"
		) ip_pifs in
		match oc with
		| (p, r) :: _ ->
			warn "multiple PIFs with other_config:peerdns=true - choosing %s" r.API.pIF_device;
			p
		| [] ->
			if management_interface then
				pif
			else
				let mgmt = List.filter (fun (_, r) -> r.API.pIF_management) ip_pifs in
				match mgmt with
				| (p, _) :: _ -> p
				| [] ->
					let (p, r) = List.hd ip_pifs in
					warn "no DNS PIF found - choosing %s" r.API.pIF_device;
					p
	in
	(if pif = gateway_pif then [`set_gateway] else []) @
	(if pif = dns_pif then [`set_dns] else [])

let bring_pif_up ~__context ?(management_interface=false) (pif: API.ref_PIF) =
	with_local_lock (fun () ->
		let uuid = Db.PIF.get_uuid ~__context ~self:pif in
		let currently_attached = Db.PIF.get_currently_attached ~__context ~self:pif in
		(* In the case of the management interface since we need to call out just to 
		refresh the default gateway device setting *)
		if currently_attached = false || management_interface then begin
			debug "PIF %s has currently_attached set to %s%s; bringing up now" uuid
				(string_of_bool currently_attached) 
				(if management_interface then " and this is to be the new management interface" else "");

			(* If the PIF is a bond master, the bond slaves will now go down *)
			(* Interface-reconfigure in bridge mode requires us to set currently_attached to false here *)
			begin match Db.PIF.get_bond_master_of ~__context ~self:pif with
				| [] -> ()
				| bond :: _ ->
					let slaves = Db.Bond.get_slaves ~__context ~self:bond in
					List.iter (fun self -> Db.PIF.set_currently_attached ~__context ~self ~value:false) slaves
			end;

			if !use_networkd then
				begin try
					let rc = Db.PIF.get_record ~__context ~self:pif in
					let bridge = Db.Network.get_bridge ~__context ~self:rc.API.pIF_network in
					let dhcp_options =
						if rc.API.pIF_ip_configuration_mode = `DHCP then
							determine_dhcp_options ~__context pif management_interface
						else
							[]
					in
					create_bridges ~__context rc bridge;
					begin match rc.API.pIF_ip_configuration_mode with
					| `None ->
						Net.Interface.set_ipv4_addr bridge None4;
					| `DHCP ->
						Net.Interface.set_ipv4_addr bridge (DHCP4 dhcp_options)
					| `Static ->
						Net.Interface.set_ipv4_addr bridge
							(Static4 [
								Unix.inet_addr_of_string rc.API.pIF_IP,
								netmask_to_prefixlen rc.API.pIF_netmask]);
						if rc.API.pIF_gateway <> "" then
							Net.Interface.set_ipv4_gateway bridge (Unix.inet_addr_of_string rc.API.pIF_gateway);
						if rc.API.pIF_DNS <> "" then begin
							let dnss = List.map Unix.inet_addr_of_string (String.split ',' rc.API.pIF_DNS) in
							Net.Interface.set_dns bridge dnss
						end
					end;
					if is_dom0_interface rc then begin
						Net.Interface.set_persistent bridge true;
						Net.Bridge.set_persistent bridge true
					end else begin
						Net.Interface.set_persistent bridge false;
						Net.Bridge.set_persistent bridge false
					end
				with Network_interface.RpcFailure (err, params) ->
					let e = Printf.sprintf "%s [%s]" err (String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ v) params)) in
					error "Network configuration error: %s" e;
					raise (Api_errors.Server_error(Api_errors.pif_configuration_error, [Ref.string_of pif; e]))
				end
			else begin
				let args = "up" :: (if management_interface then [ "--management" ] else []) in
				reconfigure_pif ~__context pif args
			end;

			warn "About to kill idle client stunnels";
			(* The master_connection would otherwise try to take a broken stunnel from the cache *)
			Stunnel_cache.flush ();
			if management_interface then begin
				warn "About to kill active client stunnels";
				let stunnels =
					let all = Locking_helpers.Thread_state.get_all_acquired_resources () in
					debug "There are %d allocated resources" (List.length all);
					List.filter (function Locking_helpers.Process("stunnel", _) -> true | _ -> false) all in
				debug "Of which %d are stunnels" (List.length stunnels);
				List.iter Locking_helpers.kill_resource stunnels;
				warn "About to forcibly reset the master connection";
				Master_connection.force_connection_reset ();
			end;

			Db.PIF.set_currently_attached ~__context ~self:pif ~value:true;
			if Db.PIF.get_management ~__context ~self:pif then begin
				debug "PIF %s is an existing management interface: rebinding and restarting server thread" uuid;
				Xapi_mgmt_iface.rebind ()
			end;

			(* If the PIF is a bond slave, the bond master will now be down *)
			begin match Db.PIF.get_bond_slave_of ~__context ~self:pif with
				| bond when bond = Ref.null -> ()
				| bond ->
					let master = Db.Bond.get_master ~__context ~self:bond in
					Db.PIF.set_currently_attached ~__context ~self:master ~value:false
			end;

			(* sync MTU *)
			(try
				let device = Db.PIF.get_device ~__context ~self:pif in
				let mtu = Int64.of_string (Netdev.get_mtu device) in
				Db.PIF.set_MTU ~__context ~self:pif ~value:mtu
			with _ ->
				debug "could not update MTU field on PIF %s" uuid
			);

			Xapi_mgmt_iface.on_dom0_networking_change ~__context;

			if not !use_networkd then
				update_inventory ~__context
		end
	)

let bring_pif_down ~__context ?(force=false) (pif: API.ref_PIF) =
	with_local_lock (fun () ->
		let rc = Db.PIF.get_record ~__context ~self:pif in
		if rc.API.pIF_currently_attached = true then begin
			debug "PIF %s has currently_attached set to true; bringing down now" rc.API.pIF_uuid;
			if !use_networkd then
				try
					let bridge = Db.Network.get_bridge ~__context ~self:rc.API.pIF_network in
					destroy_bridges ~__context ~force rc bridge;
					Net.Interface.set_persistent bridge false
				with Network_interface.RpcFailure (err, params) ->
					let e = Printf.sprintf "%s [%s]" err (String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ v) params)) in
					error "Network configuration error: %s" e;
					raise (Api_errors.Server_error(Api_errors.pif_configuration_error, [Ref.string_of pif; e]))
			else begin
				reconfigure_pif ~__context pif [ "down" ];
				update_inventory ~__context
			end;
			Db.PIF.set_currently_attached ~__context ~self:pif ~value:false;
		end
	)

