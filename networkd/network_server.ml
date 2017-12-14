(*
 * Copyright (C) Citrix Systems Inc.
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

open Network_utils
open Network_interface

(* Backport of stdext rtrim using Astring functions *)
let rtrim s =
	let open Astring in
	let drop = Char.Ascii.is_white in
  let len = String.length s in
  if len = 0 then s else
	let max_idx = len - 1 in
	let rec right_pos i =
    if i < 0 then 0 else
    if drop (String.unsafe_get s i) then right_pos (i - 1) else (i + 1)
  in
  let right = right_pos max_idx in
  if right = len then s else String.take ~max:right s

module D = Debug.Make(struct let name = "network_server" end)
open D

type context = unit

let network_conf = ref "/etc/xcp/network.conf"
let config : config_t ref = ref empty_config
let backend_kind = ref Openvswitch
let enic_workaround_until_version = ref "2.3.0.30"

let legacy_management_interface_start () =
	try
		ignore (call_script "/opt/xensource/libexec/legacy-management-interface" ["start"]);
		debug "Upgrade: brought up interfaces using the old script. Xapi will sync up soon."
	with e ->
		debug "Error while configuring the management interface using the old script: %s\n%s"
			(Printexc.to_string e) (Printexc.get_backtrace ())

let write_config () =
	try
		Network_config.write_config !config
	with Network_config.Write_error -> ()

let read_config () =
	try
		config := Network_config.read_config ();
		debug "Read configuration from networkd.db file."
	with Network_config.Read_error ->
		(* No configuration file found. *)
		(* Perhaps it is an upgrade from the pre-networkd era. If network.dbcache exists, try to configure the
		 * management interface using the old scripts. *)
		if (try Unix.access (Filename.concat "/var/lib/xcp" "network.dbcache") [Unix.F_OK]; true with _ -> false) then
			legacy_management_interface_start ()
		else
			(* Try to get the initial network setup from the first-boot data written by the host installer. *)
			try
				config := Network_config.read_management_conf ();
				debug "Read configuration from management.conf file."
			with Network_config.Read_error ->
				debug "Could not interpret the configuration in management.conf"

let on_shutdown signal =
	let dbg = "shutdown" in
	Debug.with_thread_associated dbg (fun () ->
		debug "xcp-networkd caught signal %d; performing cleanup actions." signal;
		write_config ()
	) ()

let on_timer () =
	write_config ()

let reopen_logs _ () = true

let clear_state _ () =
	config := empty_config

let reset_state _ () =
	config := Network_config.read_management_conf ()

let set_gateway_interface _ dbg ~name =
	(* Update dhclient conf for interface on changing default gateway.
	 * If new default gateway is not same as gateway_interface from networkd.db then
	 * we need to remove gateway information from gateway_interface *)
	begin match !config.gateway_interface with
	| Some gateway_iface when name <> gateway_iface ->
		let opts =
			match !config.dns_interface with
			| Some dns_iface when gateway_iface = dns_iface -> [`set_dns]
			| _ -> []
		in
		Dhclient.write_conf_file gateway_iface opts
	| _ -> ()
	end;
	debug "Setting gateway interface to %s" name;
	config := {!config with gateway_interface = Some name}

let set_dns_interface _ dbg ~name =
	debug "Setting DNS interface to %s" name;
	config := {!config with dns_interface = Some name}

(* Returns `true` if vs1 is older than vs2 *)
let is_older_version vs1 vs2 () =
	try
		let list_of_version vs = List.map int_of_string (Astring.String.cuts ~empty:false ~sep:"." vs) in
		let rec loop vs1' vs2' =
			match vs1', vs2' with
			| [], _ | _, [] -> false
			| a :: _, b :: _ when a < b -> true
			| _ :: tl1, _ :: tl2 -> loop tl1 tl2
		in
		loop (list_of_version vs1) (list_of_version vs2)
	with _ ->
		warn "Failed to compare driver version.";
		false

(* The enic driver is for Cisco UCS devices. The current driver adds VLAN0 headers
 * to all incoming packets, which confuses certain guests OSes. The workaround
 * constitutes adding a VLAN0 Linux device to strip those headers again.
 *)
let need_enic_workaround () =
	!backend_kind = Bridge && List.mem "enic" (Sysfs.list_drivers ()) && (!enic_workaround_until_version <> "") && (
		match Sysfs.get_driver_version "enic" () with
		| Some vs -> (is_older_version vs !enic_workaround_until_version ())
		| None -> false )

module Interface = struct
	let get_config name =
		get_config !config.interface_config default_interface name

	let update_config name data =
		config := {!config with interface_config = update_config !config.interface_config name data}

	let get_all _ dbg () =
		Debug.with_thread_associated dbg (fun () ->
			Sysfs.list ()
		) ()

	let exists _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			List.mem name (Sysfs.list ())
		) ()

	let get_mac _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match Linux_bonding.get_bond_master_of name with
			| Some master -> Proc.get_bond_slave_mac master name
			| None -> Ip.get_mac name
		) ()

	let is_up _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			if List.mem name (Sysfs.list ()) then
				Ip.is_up name
			else
				false
		) ()

	let get_ipv4_addr _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Ip.get_ipv4 name
		) ()

	let set_ipv4_conf _ dbg ~name ~conf =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring IPv4 address for %s: %s" name (conf |> rpc_of_ipv4 |> Jsonrpc.to_string);
			update_config name {(get_config name) with ipv4_conf = conf};
			match conf with
			| None4 ->
				if List.mem name (Sysfs.list ()) then begin
					if Dhclient.is_running name then
						ignore (Dhclient.stop name);
					Ip.flush_ip_addr name
				end
			| DHCP4 ->
				let open Xapi_stdext_monadic in
				let gateway = Opt.default [] (Opt.map (fun n -> [`gateway n]) !config.gateway_interface) in
				let dns =
					if !config.dns_interface = None || !config.dns_interface = Some name then begin
						debug "%s is the DNS interface" name;
						[`set_dns]
					end else begin
						debug "%s is NOT the DNS interface" name;
						[]
					end
				in
				let options = gateway @ dns in
				Dhclient.ensure_running name options
			| Static4 addrs ->
				if Dhclient.is_running name then begin
					ignore (Dhclient.stop name);
					Ip.flush_ip_addr name
				end;
				(* the function is meant to be idempotent and we
				 * want to avoid CA-239919 *)
				let cur_addrs = Ip.get_ipv4 name in
				let rm_addrs = Xapi_stdext_std.Listext.List.set_difference cur_addrs addrs in
				let add_addrs = Xapi_stdext_std.Listext.List.set_difference addrs cur_addrs in
				List.iter (Ip.del_ip_addr name) rm_addrs;
				List.iter (Ip.set_ip_addr name) add_addrs
		) ()

	let get_ipv4_gateway _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			let output = Ip.route_show ~version:Ip.V4 name in
			try
				let line = List.find (fun s -> Astring.String.is_prefix ~affix:"default via" s) (Astring.String.cuts ~empty:false ~sep:"\n" output) in
				let addr = List.nth (Astring.String.cuts ~empty:false ~sep:" " line) 2 in
				Some (Unix.inet_addr_of_string addr)
			with Not_found -> None
		) ()

	let set_ipv4_gateway _ dbg ~name ~address =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring IPv4 gateway for %s: %s" name (Unix.string_of_inet_addr address);
			update_config name {(get_config name) with ipv4_gateway = Some address};
			if !config.gateway_interface = None || !config.gateway_interface = Some name then begin
				debug "%s is the default gateway interface" name;
				Ip.set_gateway name address
			end else
				debug "%s is NOT the default gateway interface" name
		) ()

	let get_ipv6_addr _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Ip.get_ipv6 name
		) ()

	let set_ipv6_conf _ dbg ~name ~conf =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring IPv6 address for %s: %s" name (conf |> rpc_of_ipv6 |> Jsonrpc.to_string);
			update_config name {(get_config name) with ipv6_conf = conf};
			match conf with
			| None6 ->
				if List.mem name (Sysfs.list ()) then begin
					if Dhclient.is_running ~ipv6:true name then
						ignore (Dhclient.stop ~ipv6:true name);
					Sysctl.set_ipv6_autoconf name false;
					Ip.flush_ip_addr ~ipv6:true name
				end
			| Linklocal6 ->
				if List.mem name (Sysfs.list ()) then begin
					if Dhclient.is_running ~ipv6:true name then
						ignore (Dhclient.stop ~ipv6:true name);
					Sysctl.set_ipv6_autoconf name false;
					Ip.flush_ip_addr ~ipv6:true name;
					Ip.set_ipv6_link_local_addr name
				end
			| DHCP6 ->
				if Dhclient.is_running ~ipv6:true name then
					ignore (Dhclient.stop ~ipv6:true name);
				Sysctl.set_ipv6_autoconf name false;
				Ip.flush_ip_addr ~ipv6:true name;
				Ip.set_ipv6_link_local_addr name;
				ignore (Dhclient.start ~ipv6:true name [])
			| Autoconf6 ->
				if Dhclient.is_running ~ipv6:true name then
					ignore (Dhclient.stop ~ipv6:true name);
				Ip.flush_ip_addr ~ipv6:true name;
				Ip.set_ipv6_link_local_addr name;
				Sysctl.set_ipv6_autoconf name true;
				(* Cannot link set down/up due to CA-89882 - IPv4 default route cleared *)
			| Static6 addrs ->
				if Dhclient.is_running ~ipv6:true name then
					ignore (Dhclient.stop ~ipv6:true name);
				Sysctl.set_ipv6_autoconf name false;
				(* add the link_local and clean the old one only when needed *)
				let cur_addrs = 
					let addrs = Ip.get_ipv6 name in
					let maybe_link_local = Ip.split_addr (Ip.get_ipv6_link_local_addr name) in
					match maybe_link_local with
					| Some addr -> Xapi_stdext_std.Listext.List.setify (addr :: addrs)
					| None -> addrs
				in
				let rm_addrs = Xapi_stdext_std.Listext.List.set_difference cur_addrs addrs in
				let add_addrs = Xapi_stdext_std.Listext.List.set_difference addrs cur_addrs in
				List.iter (Ip.del_ip_addr name) rm_addrs;
				List.iter (Ip.set_ip_addr name) add_addrs
		) ()

	let get_ipv6_gateway _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			let output = Ip.route_show ~version:Ip.V6 name in
			try
				let line = List.find (fun s -> Astring.String.is_prefix ~affix:"default via" s) (Astring.String.cuts ~empty:false ~sep:"\n" output) in
				let addr = List.nth (Astring.String.cuts ~empty:false ~sep:" " line) 2 in
				Some (Unix.inet_addr_of_string addr)
			with Not_found -> None
		) ()

	let set_ipv6_gateway _ dbg ~name ~address =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring IPv6 gateway for %s: %s" name (Unix.string_of_inet_addr address);
			update_config name {(get_config name) with ipv6_gateway = Some address};
			if !config.gateway_interface = None || !config.gateway_interface = Some name then begin
				debug "%s is the default gateway interface" name;
				Ip.set_gateway name address
			end else
				debug "%s is NOT the default gateway interface" name
		) ()

	let set_ipv4_routes _ dbg ~name ~routes =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring IPv4 static routes for %s: %s" name (String.concat ", " (List.map (fun (i, p, g) ->
				Printf.sprintf "%s/%d/%s" (Unix.string_of_inet_addr i) p (Unix.string_of_inet_addr g)) routes));
			update_config name {(get_config name) with ipv4_routes = routes};
			List.iter (fun (i, p, g) -> Ip.set_route ~network:(i, p) name g) routes
		) ()

	let get_dns _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			let nameservers, domains = Xapi_stdext_unix.Unixext.file_lines_fold (fun (nameservers, domains) line ->
				if Astring.String.is_prefix ~affix:"nameserver" line then
					let server = List.nth (Astring.String.fields ~empty:false line) 1 in
					(Unix.inet_addr_of_string server) :: nameservers, domains
				else if Astring.String.is_prefix ~affix:"search" line then
					let domains = List.tl (Astring.String.fields ~empty:false line) in
					nameservers, domains
				else
					nameservers, domains
			) ([], []) resolv_conf in
			List.rev nameservers, domains
		) ()

	let set_dns _ dbg ~name ~nameservers ~domains =
		Debug.with_thread_associated dbg (fun () ->
			update_config name {(get_config name) with dns = nameservers, domains};
			debug "Configuring DNS for %s: nameservers: [%s]; domains: [%s]" name
				(String.concat ", " (List.map Unix.string_of_inet_addr nameservers))
				(String.concat ", " domains);
			if (!config.dns_interface = None || !config.dns_interface = Some name) then begin
				debug "%s is the DNS interface" name;
				let domains' = if domains <> [] then ["search " ^ (String.concat " " domains)] else [] in
				let nameservers' = List.map (fun ip -> "nameserver " ^ (Unix.string_of_inet_addr ip)) nameservers in
				let lines = domains' @ nameservers' in
				Xapi_stdext_unix.Unixext.write_string_to_file resolv_conf ((String.concat "\n" lines) ^ "\n")
			end else
				debug "%s is NOT the DNS interface" name
		) ()

	let get_mtu _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Ip.get_mtu name
		) ()

	let set_mtu _ dbg ~name ~mtu =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring MTU for %s: %d" name mtu;
			update_config name {(get_config name) with mtu};
			match !backend_kind with
			| Openvswitch -> ignore (Ovs.set_mtu name mtu)
			| Bridge -> Ip.link_set_mtu name mtu
		) ()

	let set_ethtool_settings _ dbg ~name ~params =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring ethtool settings for %s: %s" name
				(String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) params));
			let add_defaults = List.filter (fun (k, v) -> not (List.mem_assoc k params)) default_interface.ethtool_settings in
			let params = params @ add_defaults in
			update_config name {(get_config name) with ethtool_settings = params};
			Ethtool.set_options name params
		) ()

	let set_ethtool_offload _ dbg ~name ~params =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring ethtool offload settings for %s: %s" name
				(String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) params));
			let add_defaults = List.filter (fun (k, v) -> not (List.mem_assoc k params)) default_interface.ethtool_offload in
			let params = params @ add_defaults in
			update_config name {(get_config name) with ethtool_offload = params};
			Ethtool.set_offload name params
		) ()

	let get_capabilities _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Fcoe.get_capabilities name
		) ()

	let is_connected _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Sysfs.get_carrier name
		) ()

	let is_physical _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Sysfs.is_physical name
		) ()

	let has_vlan _ dbg ~name ~vlan =
		(* Identify the vlan is used by kernel which is unknown to XAPI *)
		Debug.with_thread_associated dbg (fun () ->
			List.exists (fun (_, v, p) -> v = vlan && p = name) (Proc.get_vlans ())
	) ()

	let bring_up _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			debug "Bringing up interface %s" name;
			Ip.link_set_up name
		) ()

	let bring_down _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			debug "Bringing down interface %s" name;
			Ip.link_set_down name
		) ()

	let is_persistent _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			(get_config name).persistent_i
		) ()

	let set_persistent _ dbg ~name ~value =
		Debug.with_thread_associated dbg (fun () ->
			debug "Making interface %s %spersistent" name (if value then "" else "non-");
			update_config name {(get_config name) with persistent_i = value}
		) ()

	let make_config _ dbg ?(conservative=false) ~config () =
		Debug.with_thread_associated dbg (fun () ->
			(* Only attempt to configure interfaces that exist in the system *)
			let all = get_all () dbg () in
			let config = List.filter (fun (name, _) -> List.mem name all) config in
			(* Handle conservativeness *)
			let config =
				if conservative then begin
					(* Do not touch non-persistent interfaces *)
					debug "Only configuring persistent interfaces";
					List.filter (fun (name, interface) -> interface.persistent_i) config
				end else
					config
			in
			let config =
				if need_enic_workaround () then
					List.fold_left (fun accu (name, interface) ->
						if (Sysfs.is_physical name && Linux_bonding.get_bond_master_of name = None) || Linux_bonding.is_bond_device name then
							(name, interface) :: (Ip.vlan_name name 0, interface) :: accu
						else
							(name, interface) :: accu
					) [] config
				else
					config
			in
			debug "** Configuring the following interfaces: %s%s" (String.concat ", " (List.map (fun (name, _) -> name) config))
				(if conservative then " (best effort)" else "");
			let exec f = if conservative then (try f () with _ -> ()) else f () in
			List.iter (function (name, ({ipv4_conf; ipv4_gateway; ipv6_conf; ipv6_gateway; ipv4_routes; dns=nameservers,domains; mtu;
				ethtool_settings; ethtool_offload; _} as c)) ->
				update_config name c;
				exec (fun () ->
					(* We only apply the DNS settings when in static IPv4 mode to avoid conflicts with DHCP mode.
					 * The `dns` field should really be an option type so that we don't have to derive the intention
					 * of the caller by looking at other fields. *)
					match ipv4_conf with Static4 _ -> set_dns () dbg ~name ~nameservers ~domains | _ -> ());
				exec (fun () -> set_ipv4_conf () dbg ~name ~conf:ipv4_conf);
				exec (fun () -> match ipv4_gateway with None -> () | Some gateway ->
					set_ipv4_gateway () dbg ~name ~address:gateway);
				(try set_ipv6_conf () dbg ~name ~conf:ipv6_conf with _ -> ());
				(try match ipv6_gateway with None -> () | Some gateway ->
					set_ipv6_gateway () dbg ~name ~address:gateway with _ -> ());
				exec (fun () -> set_ipv4_routes () dbg ~name ~routes:ipv4_routes);
				exec (fun () -> set_mtu () dbg ~name ~mtu);
				exec (fun () -> bring_up () dbg ~name);
				exec (fun () -> set_ethtool_settings () dbg ~name ~params:ethtool_settings);
				exec (fun () -> set_ethtool_offload () dbg ~name ~params:ethtool_offload)
			) config
		) ()
end

module Bridge = struct
	let add_default = ref []

	let get_config name =
		get_config !config.bridge_config default_bridge name

	let remove_config name =
		config := {!config with bridge_config = remove_config !config.bridge_config name}

	let update_config name data =
		config := {!config with bridge_config = update_config !config.bridge_config name data}

	let determine_backend () =
		try
			let backend = Astring.String.trim (Xapi_stdext_unix.Unixext.string_of_file !network_conf) in
			match backend with
			| "openvswitch" | "vswitch" -> backend_kind := Openvswitch
			| "bridge" -> backend_kind := Bridge
			| backend ->
				warn "Network backend unknown (%s). Falling back to Open vSwitch." backend;
				backend_kind := Openvswitch
		with _ ->
			warn "Network-conf file not found. Falling back to Open vSwitch.";
			backend_kind := Openvswitch

	let get_bond_links_up _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch -> Ovs.get_bond_links_up name
			| Bridge -> Proc.get_bond_links_up name
		) ()

	let get_all _ dbg () =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch -> Ovs.list_bridges ()
			| Bridge -> Sysfs.get_all_bridges ()
		) ()

	let destroy_existing_vlan_bridge name (parent, vlan) =
		begin match !backend_kind with
		| Openvswitch ->
			let bridges =
				let raw = Ovs.vsctl ["--bare"; "-f"; "table"; "--"; "--columns=name"; "find"; "port"; "fake_bridge=true"; "tag=" ^ (string_of_int vlan)] in
				if raw <> "" then Astring.String.cuts ~empty:false ~sep:"\n" (rtrim raw) else []
			in
			let existing_bridges =
				List.filter ( fun bridge ->
					match Ovs.bridge_to_vlan bridge with
					| Some (p, v) -> p = parent && v = vlan
					| None -> false
				) bridges in
			List.iter (fun bridge ->
				if bridge <> name then begin
					debug "Destroying existing bridge %s" bridge;
					remove_config bridge;
					ignore (Ovs.destroy_bridge bridge)
				end
			) existing_bridges
		| Bridge ->
			let ifaces = Sysfs.bridge_to_interfaces parent in
			let existing_bridges =
				match List.filter (fun (_, tag, iface) -> tag = vlan && List.mem iface ifaces) (Proc.get_vlans ()) with
				| [] -> []
				| (vlan_iface, _, _) :: _ ->
					List.filter (fun bridge ->
						List.mem vlan_iface (Sysfs.bridge_to_interfaces bridge)
					) (Sysfs.get_all_bridges ())
			in
			List.iter (fun bridge ->
				if bridge <> name then begin
					debug "Destroying existing bridge %s" bridge;
					Interface.bring_down () "Destroying existing bridge" ~name:bridge;
					remove_config bridge;
					List.iter (fun dev ->
						Brctl.destroy_port bridge dev;
					) (Sysfs.bridge_to_interfaces bridge);
					ignore (Brctl.destroy_bridge bridge)
				end
			) existing_bridges
		end

	let create _ dbg ?vlan ?mac ?igmp_snooping ?(other_config=[]) ~name () =
		Debug.with_thread_associated dbg (fun () ->
			debug "Creating bridge %s%s" name (match vlan with
				| None -> ""
				| Some (parent, vlan) -> Printf.sprintf " (VLAN %d on bridge %s)" vlan parent
			);
			Xapi_stdext_monadic.Opt.iter (destroy_existing_vlan_bridge name) vlan;
			update_config name {(get_config name) with vlan; bridge_mac=mac; igmp_snooping; other_config};
			begin match !backend_kind with
			| Openvswitch ->
				let fail_mode =
					if not (List.mem_assoc "vswitch-controller-fail-mode" other_config) then
						"standalone"
					else
						let mode = List.assoc "vswitch-controller-fail-mode" other_config in
						if mode = "secure" || mode = "standalone" then begin
							(try if mode = "secure" && Ovs.get_fail_mode name <> "secure" then
								add_default := name :: !add_default
							with _ -> ());
							mode
						end else begin
							debug "%s isn't a valid setting for other_config:vswitch-controller-fail-mode; \
								defaulting to 'standalone'" mode;
							"standalone"
						end
				in
				let vlan_bug_workaround =
					if List.mem_assoc "vlan-bug-workaround" other_config then
						Some (List.assoc "vlan-bug-workaround" other_config = "true")
					else
						None
				in
				let external_id =
					if List.mem_assoc "network-uuids" other_config then
						Some ("xs-network-uuids", List.assoc "network-uuids" other_config)
					else
						None
				in
				let disable_in_band =
					if not (List.mem_assoc "vswitch-disable-in-band" other_config) then
						Some None
					else
						let dib = List.assoc "vswitch-disable-in-band" other_config in
						if dib = "true" || dib = "false" then
							Some (Some dib)
						else
							(debug "%s isn't a valid setting for other_config:vswitch-disable-in-band" dib;
							None)
				in
				let old_igmp_snooping = Ovs.get_mcast_snooping_enable name in
				ignore (Ovs.create_bridge ?mac ~fail_mode ?external_id ?disable_in_band ?igmp_snooping
					vlan vlan_bug_workaround name);
				if igmp_snooping = Some true && not old_igmp_snooping then
					Ovs.inject_igmp_query name
                
			| Bridge ->
				ignore (Brctl.create_bridge name);
				Brctl.set_forwarding_delay name 0;
				Sysfs.set_multicast_snooping name false;
				Xapi_stdext_monadic.Opt.iter (Ip.set_mac name) mac;
				match vlan with
				| None -> ()
				| Some (parent, vlan) ->
					let bridge_interfaces = Sysfs.bridge_to_interfaces name in
					let parent_bridge_interface = List.hd (List.filter (fun n ->
						Astring.String.is_prefix ~affix:"eth" n || Astring.String.is_prefix ~affix:"bond" n
					) (Sysfs.bridge_to_interfaces parent)) in
					let parent_interface =
						if need_enic_workaround () then begin
							let n = String.length parent_bridge_interface in
							let m = String.sub parent_bridge_interface 0 (n - 2) in
							if vlan = 0 then
								error "The enic workaround is in effect. Bridge %s is used for VLAN 0 on %s." parent m;
							m
						end else
							parent_bridge_interface
					in
					let vlan_name = Ip.vlan_name parent_interface vlan in
					(* Check if the VLAN is already in use by something else *)
					List.iter (fun (device, vlan', parent') ->
						(* A device for the same VLAN (parent + tag), but with a different
						 * device name or not on the requested bridge is bad. *)
						if parent' = parent && vlan' = vlan &&
							(device <> vlan_name || not (List.mem device bridge_interfaces)) then
							raise (Vlan_in_use (parent, vlan))
					) (Proc.get_vlans ());
					(* Robustness enhancement: ensure there are no other VLANs in the bridge *)
					let current_interfaces = List.filter (fun n ->
						Astring.String.is_prefix ~affix:"eth" n || Astring.String.is_prefix ~affix:"bond" n
					) bridge_interfaces in
					debug "Removing these non-VIF interfaces found on the bridge: %s"
						(String.concat ", " current_interfaces);
					List.iter (fun interface ->
						Brctl.destroy_port name interface;
						Interface.bring_down () dbg ~name:interface
					) current_interfaces;
					(* Now create the new VLAN device and add it to the bridge *)
					Ip.create_vlan parent_interface vlan;
					Interface.bring_up () dbg ~name:vlan_name;
					Brctl.create_port name vlan_name
			end;
			Interface.bring_up () dbg ~name
		) ()

	let destroy _ dbg ?(force=false) ~name () =
		Debug.with_thread_associated dbg (fun () ->
			Interface.bring_down () dbg ~name;
			match !backend_kind with
			| Openvswitch ->
				let vlans_on_this_parent = Ovs.get_vlans name in
				if vlans_on_this_parent = [] || force then begin
					debug "Destroying bridge %s" name;
					remove_config name;
					let interfaces = (Ovs.bridge_to_interfaces name) @ vlans_on_this_parent in
					List.iter (fun dev ->
						Interface.set_ipv4_conf () dbg ~name:dev ~conf:None4;
						Interface.bring_down () dbg ~name:dev
					) interfaces;
					Interface.set_ipv4_conf () dbg ~name ~conf:None4;
					ignore (Ovs.destroy_bridge name)
				end else
					debug "Not destroying bridge %s, because it has VLANs on top" name
			| Bridge ->
				let ifs = Sysfs.bridge_to_interfaces name in
				let vlans_on_this_parent =
					let interfaces = List.filter (fun n ->
					Astring.String.is_prefix ~affix:"eth" n || Astring.String.is_prefix ~affix:"bond" n
					) ifs in
					match interfaces with
					| [] -> []
					| interface :: _ ->
						List.filter (Astring.String.is_prefix ~affix:(interface ^ ".")) (Sysfs.list ())
				in
				if vlans_on_this_parent = [] || force then begin
					debug "Destroying bridge %s" name;
					remove_config name;
					List.iter (fun dev ->
						Interface.set_ipv4_conf () dbg ~name:dev ~conf:None4;
						Brctl.destroy_port name dev;
						Interface.bring_down () dbg ~name:dev;
						if Linux_bonding.is_bond_device dev then
							Linux_bonding.remove_bond_master dev;
						if (Astring.String.is_prefix ~affix:"eth" dev || Astring.String.is_prefix ~affix:"bond" dev) && String.contains dev '.' then begin
							ignore (Ip.destroy_vlan dev);
							let n = String.length dev in
							if String.sub dev (n - 2) 2 = ".0" && need_enic_workaround () then
								let vlan_base = String.sub dev 0 (n - 2) in
								if Linux_bonding.is_bond_device vlan_base then
									Linux_bonding.remove_bond_master (String.sub dev 0 (n - 2))
						end;
					) ifs;
					Interface.set_ipv4_conf () dbg ~name ~conf:None4;
					ignore (Brctl.destroy_bridge name)
				end else
					debug "Not destroying bridge %s, because it has VLANs on top" name
		) ()

	let get_kind _ dbg () =
		Debug.with_thread_associated dbg (fun () ->
			!backend_kind
		) ()

	let get_ports _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch -> Ovs.bridge_to_ports name
			| Bridge -> raise Not_implemented
		) ()

	let get_all_ports _ dbg ?(from_cache=false) () =
		Debug.with_thread_associated dbg (fun () ->
			if from_cache then
				let ports = List.concat (List.map (fun (_, {ports}) -> ports) !config.bridge_config) in
				List.map (fun (port, {interfaces}) -> port, interfaces) ports
			else
				match !backend_kind with
				| Openvswitch -> List.concat (List.map Ovs.bridge_to_ports (Ovs.list_bridges ()))
				| Bridge -> raise Not_implemented
		) ()

	let get_bonds _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch -> Ovs.bridge_to_ports name
			| Bridge -> raise Not_implemented
		) ()

	let get_all_bonds _ dbg ?(from_cache=false) () =
		Debug.with_thread_associated dbg (fun () ->
			if from_cache then
				let ports = List.concat (List.map (fun (_, {ports}) -> ports) !config.bridge_config) in
				let names = List.map (fun (port, {interfaces}) -> port, interfaces) ports in
				List.filter (fun (_, ifs) -> List.length ifs > 1) names
			else
				match !backend_kind with
				| Openvswitch -> List.concat (List.map Ovs.bridge_to_ports (Ovs.list_bridges ()))
				| Bridge -> raise Not_implemented
		) ()

	type bond_link_info = {
		slave: iface;
		up: bool;
		active: bool;
	}

	let get_bond_link_info _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch ->
				let slaves, active_slave = Ovs.get_bond_link_status name in
				let mode = Ovs.get_bond_mode name in
				List.map (fun (slave, up) ->
					let active =
						let ab = mode = Some "active-backup" in
						ab && (active_slave = Some slave) ||
						(not ab) && up
					in
					{slave; up; active}
				) slaves
			| Bridge ->
				let active_slave = Linux_bonding.get_bond_active_slave name in
				let slaves = Proc.get_bond_slave_info name "MII Status" in
				let bond_props = Linux_bonding.get_bond_properties name in
				List.map (fun (slave, status) ->
					let up = status = "up" in
					let active =
						let ab =
							List.mem_assoc "mode" bond_props &&
							Astring.String.is_prefix ~affix:"active-backup" (List.assoc "mode" bond_props)
						in
						ab && (active_slave = Some slave) ||
						(not ab) && up
					in
					{slave; up; active}
				) slaves
		) ()

	let get_vlan _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch -> Ovs.bridge_to_vlan name
			| Bridge -> raise Not_implemented
		) ()

	let add_default_flows _ dbg bridge mac interfaces =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch -> Ovs.add_default_flows bridge mac interfaces
			| Bridge -> ()
		) ()

	let add_basic_port dbg bridge name {interfaces; bond_mac; bond_properties} =
		match !backend_kind with
		| Openvswitch ->
			if List.length interfaces = 1 then begin
				List.iter (fun name -> Interface.bring_up () dbg ~name) interfaces;
				ignore (Ovs.create_port (List.hd interfaces) bridge)
			end else begin
				if bond_mac = None then
					warn "No MAC address specified for the bond";
				ignore (Ovs.create_bond ?mac:bond_mac name interfaces bridge bond_properties);
				List.iter (fun name -> Interface.bring_up () dbg ~name) interfaces
			end;
			if List.mem bridge !add_default then begin
				let mac = match bond_mac with
					| None -> (try Some (Ip.get_mac name) with _ -> None)
					| Some mac -> Some mac
				in
				match mac with
				| Some mac ->
					add_default_flows () dbg bridge mac interfaces;
					add_default := List.filter ((<>) bridge) !add_default
				| None ->
					warn "Could not add default flows for port %s on bridge %s because no MAC address was specified"
						name bridge
			end
		| Bridge ->
			if List.length interfaces = 1 then
				List.iter (fun name -> Interface.bring_up () dbg ~name) interfaces
			else begin
				Linux_bonding.add_bond_master name;
				let bond_properties =
					if List.mem_assoc "mode" bond_properties && List.assoc "mode" bond_properties = "lacp" then
					Xapi_stdext_std.Listext.List.replace_assoc "mode" "802.3ad" bond_properties
					else bond_properties
				in
				Linux_bonding.set_bond_properties name bond_properties;
				Linux_bonding.set_bond_slaves name interfaces;
				begin match bond_mac with
					| Some mac -> Ip.set_mac name mac
					| None -> warn "No MAC address specified for the bond"
				end;
				Interface.bring_up () dbg ~name
			end;
			if need_enic_workaround () then begin
				debug "Applying enic workaround: adding VLAN0 device to bridge";
				Ip.create_vlan name 0;
				let vlan0 = Ip.vlan_name name 0 in
				Interface.bring_up () dbg ~name:vlan0;
				ignore (Brctl.create_port bridge vlan0)
			end else
				ignore (Brctl.create_port bridge name)

	let add_pvs_proxy_port dbg bridge name port =
		match !backend_kind with
		| Openvswitch ->
			ignore (Ovs.create_port ~internal:true name bridge);
			let real_bridge = Ovs.get_real_bridge bridge in
			Ovs.mod_port real_bridge name "no-flood";
			Interface.bring_up () dbg ~name
		| Bridge ->
			raise Not_implemented

	let add_port _ dbg ?bond_mac ~bridge ~name ~interfaces ?(bond_properties=[]) ?(kind=Basic) () =
		Debug.with_thread_associated dbg (fun () ->
			let config = get_config bridge in
			let ports =
				if List.mem_assoc name config.ports then
					List.remove_assoc name config.ports
				else
					config.ports
			in
			let port = {interfaces; bond_mac; bond_properties; kind} in
			let ports = (name, port) :: ports in
			update_config bridge {config with ports};
			debug "Adding %s port %s to bridge %s with interface(s) %s%s"
				(string_of_port_kind kind)
				name bridge
				(String.concat ", " interfaces)
				(match bond_mac with Some mac -> " and MAC " ^ mac | None -> "");
			match kind with
			| Basic -> add_basic_port dbg bridge name port
			| PVS_proxy -> add_pvs_proxy_port dbg bridge name port
		) ()

	let remove_port _ dbg ~bridge ~name =
		Debug.with_thread_associated dbg (fun () ->
			debug "Removing port %s from bridge %s" name bridge;
			let config = get_config bridge in
			if List.mem_assoc name config.ports then begin
				let ports = List.remove_assoc name config.ports in
				update_config bridge {config with ports}
			end;
			match !backend_kind with
			| Openvswitch ->
				ignore (Ovs.destroy_port name)
			| Bridge ->
				ignore (Brctl.destroy_port bridge name)
		) ()

	let get_interfaces _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch ->
				Ovs.bridge_to_interfaces name
			| Bridge ->
				Sysfs.bridge_to_interfaces name
		) ()

	let get_physical_interfaces _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch ->
				Ovs.get_real_bridge name
				|> Ovs.bridge_to_interfaces
				|> List.filter (Sysfs.is_physical)

			| Bridge ->
				let ifaces = Sysfs.bridge_to_interfaces name in
				let vlan_ifaces = List.filter (fun (bridge, _, _) -> List.mem bridge ifaces) (Proc.get_vlans ()) in
				let bond_ifaces = List.filter (fun iface -> Linux_bonding.is_bond_device iface) ifaces in
				let physical_ifaces = List.filter (fun iface -> Sysfs.is_physical iface) ifaces in
				if vlan_ifaces <> [] then
					let _, _, parent = List.hd vlan_ifaces in
					if Linux_bonding.is_bond_device parent then
						Linux_bonding.get_bond_slaves parent
					else
						[parent]
				else if bond_ifaces <> [] then
					Linux_bonding.get_bond_slaves (List.hd bond_ifaces)
				else
					physical_ifaces
		) ()

	let get_fail_mode _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !backend_kind with
			| Openvswitch ->
				begin match Ovs.get_fail_mode name with
				| "standalone" -> Some Standalone
				| "secure" -> Some Secure
				| _ -> None
				end
			| Bridge -> raise Not_implemented
		) ()

	let is_persistent _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			(get_config name).persistent_b
		) ()

	let set_persistent _ dbg ~name ~value =
		Debug.with_thread_associated dbg (fun () ->
			debug "Making bridge %s %spersistent" name (if value then "" else "non-");
			update_config name {(get_config name) with persistent_b = value}
		) ()

	let make_config _ dbg ?(conservative=false) ~config () =
		Debug.with_thread_associated dbg (fun () ->
			let vlans_go_last (_, {vlan=vlan_of_a}) (_, {vlan=vlan_of_b}) =
				if vlan_of_a = None && vlan_of_b = None then 0
				else if vlan_of_a <> None && vlan_of_b = None then 1
				else if vlan_of_a = None && vlan_of_b <> None then -1
				else 0
			in
			let config =
				if conservative then begin
					let persistent_config = List.filter (fun (name, bridge) -> bridge.persistent_b) config in
					debug "Ensuring the following persistent bridges are up: %s"
						(String.concat ", " (List.map (fun (name, _) -> name) persistent_config));
					let vlan_parents = Xapi_stdext_std.Listext.List.filter_map (function
						| (_, {vlan=Some (parent, _)}) ->
							if not (List.mem_assoc parent persistent_config) then
								Some (parent, List.assoc parent config)
							else
								None
						| _ -> None) persistent_config in
					debug "Additionally ensuring the following VLAN parent bridges are up: %s"
						(String.concat ", " (List.map (fun (name, _) -> name) vlan_parents));
					let config = vlan_parents @ persistent_config in
					(* Do not try to recreate bridges that already exist *)
					let current = get_all () dbg () in
					List.filter (function (name, _) -> not (List.mem name current)) config
				end else
					config
			in
			let config = List.sort vlans_go_last config in
			let exec f = if conservative then (try f () with _ -> ()) else f () in
			debug "** Configuring the following bridges: %s%s"
				(String.concat ", " (List.map (fun (name, _) -> name) config))
				(if conservative then " (best effort)" else "");
			List.iter (function (bridge_name, ({ports; vlan; bridge_mac; igmp_snooping; other_config; _} as c)) ->
				update_config bridge_name c;
				exec (fun () ->
					create () dbg ?vlan ?mac:bridge_mac ?igmp_snooping ~other_config ~name:bridge_name ();
					List.iter (fun (port_name, {interfaces; bond_properties; bond_mac; kind}) ->
						add_port () dbg ?bond_mac ~bridge:bridge_name ~name:port_name ~interfaces ~bond_properties ~kind ()
					) ports
				)
			) config
		) ()
end

module PVS_proxy = struct
	open PVS_proxy

	let path = ref "/opt/citrix/pvsproxy/socket/pvsproxy"
	
	let do_call call =
		try
			Jsonrpc_client.with_rpc ~path:!path ~call ()
		with e ->
			error "Error when calling PVS proxy: %s" (Printexc.to_string e);
			raise PVS_proxy_connection_error

	let configure_site _ dbg config =
		debug "Configuring PVS proxy for site %s" config.site_uuid;
		let call = {Rpc.name = "configure_site"; params = [rpc_of_t config]} in
		let _ = do_call call in
		()

	let remove_site _ dbg uuid =
		debug "Removing PVS proxy for site %s" uuid;
		let call = Rpc.{name = "remove_site"; params = [Dict ["site_uuid", rpc_of_string uuid]]} in
		let _ = do_call call in
		()
end

let on_startup () =
	let dbg = "startup" in
	Debug.with_thread_associated dbg (fun () ->
		Bridge.determine_backend ();
		let remove_centos_config () =
			(* Remove DNSDEV and GATEWAYDEV from Centos networking file, because the interfere
			 * with this daemon. *)
			try
				let file = rtrim (Xapi_stdext_unix.Unixext.string_of_file "/etc/sysconfig/network") in
				let args = Astring.String.cuts ~empty:false ~sep:"\n" file in
				let args = List.map (fun s -> match (Astring.String.cuts ~empty:false ~sep:"=" s) with k :: [v] -> k, v | _ -> "", "") args in
				let args = List.filter (fun (k, v) -> k <> "DNSDEV" && k <> "GATEWAYDEV") args in
				let s = String.concat "\n" (List.map (fun (k, v) -> k ^ "=" ^ v) args) ^ "\n" in
				Xapi_stdext_unix.Unixext.write_string_to_file "/etc/sysconfig/network" s
			with _ -> ()
		in
		try
			(* the following is best-effort *)
			read_config ();
			remove_centos_config ();
			if !backend_kind = Openvswitch then
				Ovs.set_max_idle 5000;
			Bridge.make_config () dbg ~conservative:true ~config:!config.bridge_config ();
			Interface.make_config () dbg ~conservative:true ~config:!config.interface_config ();
			(* If there is still a network.dbcache file, move it out of the way. *)
			if (try Unix.access (Filename.concat "/var/lib/xcp" "network.dbcache") [Unix.F_OK]; true with _ -> false) then
				Unix.rename (Filename.concat "/var/lib/xcp" "network.dbcache") (Filename.concat "/var/lib/xcp" "network.dbcache.bak");
		with e ->
			debug "Error while configuring networks on startup: %s\n%s"
				(Printexc.to_string e) (Printexc.get_backtrace ())
	) ()

