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

open Fun
open Stringext
open Listext

let config_file_path = Filename.concat Fhs.vardir "networkd.db"

let netmask_to_prefixlen netmask =
	let s = Unix.string_of_inet_addr netmask in
	Scanf.sscanf s "%d.%d.%d.%d" (fun a b c d ->
		let rec length l x =
			if x > 0 then
				length (succ l) (x lsr 1)
			else
				l
		in
		List.fold_left length 0 [a; b; c; d]
	)

type context = unit

let empty_config = {interface_config = []; bridge_config = []; gateway_interface = None; dns_interface = None}
let config : config_t ref = ref empty_config

let read_management_conf () =
	let management_conf = Unixext.string_of_file (Xapi_globs.first_boot_dir ^ "data/management.conf") in
	let args = String.split '\n' (String.rtrim management_conf) in
	let args = List.map (fun s -> match (String.split '=' s) with k :: [v] -> k, String.strip ((=) '\'') v | _ -> "", "") args in
	debug "Firstboot file management.conf has: %s" (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) args));
	let device = List.assoc "LABEL" args in
	Util_inventory.reread_inventory ();
	let bridge_name = Util_inventory.lookup Util_inventory._management_interface in
	debug "Management bridge in inventory file: %s" bridge_name;
	let ipv4_conf, ipv4_gateway, dns =
		match List.assoc "MODE" args with
		| "static" ->
			let ip = List.assoc "IP" args |> Unix.inet_addr_of_string in
			let prefixlen = List.assoc "NETMASK" args |> Unix.inet_addr_of_string |> netmask_to_prefixlen in
			let gateway =
				if List.mem_assoc "GATEWAY" args then
					Some (List.assoc "GATEWAY" args |> Unix.inet_addr_of_string)
				else None
			in
			let nameservers =
				if List.mem_assoc "DNS" args && List.assoc "DNS" args <> "" then
					List.map Unix.inet_addr_of_string (String.split ',' (List.assoc "DNS" args))
				else []
			in
			let domains =
				if List.mem_assoc "DOMAIN" args && List.assoc "DOMAIN" args <> "" then
					String.split ' ' (List.assoc "DOMAIN" args)
				else []
			in
			let dns = nameservers, domains in
			Static4 [ip, prefixlen], gateway, dns
		| "dhcp" | _ ->
			DHCP4 [`set_gateway; `set_dns], None, ([], [])
	in
	let phy_interface = {default_interface with persistent_i = true} in
	let bridge_interface = {default_interface with ipv4_conf; ipv4_gateway; persistent_i = true} in
	let bridge = {default_bridge with
		ports = [device, {default_port with interfaces = [device]}];
		persistent_b = true
	} in
	{interface_config = [device, phy_interface; bridge_name, bridge_interface];
		bridge_config = [bridge_name, bridge];
		gateway_interface = Some bridge_name; dns_interface = Some bridge_name}

let legacy_management_interface_start () =
	try
		ignore (call_script "/etc/init.d/management-interface" ["start"]);
		debug "Upgrade: brought up interfaces using the old script. Xapi will sync up soon."
	with e ->
		debug "Error while configuring the management interface using the old script: %s\n%s"
			(Printexc.to_string e) (Printexc.get_backtrace ())

let write_config () =
	let config_json = !config |> rpc_of_config_t |> Jsonrpc.to_string in
	Unixext.write_string_to_file config_file_path config_json

let read_config () =
	try
		let config_json = Unixext.string_of_file config_file_path in
		config := config_json |> Jsonrpc.of_string |> config_t_of_rpc;
		debug "Read configuration from networkd.db file."
	with _ ->
		(* No configuration file found. *)
		(* Perhaps it is an upgrade from the pre-networkd era. If network.dbcache exists, try to configure the
		 * management interface using the old scripts. *)
		if (try Unix.access (Filename.concat Fhs.vardir "network.dbcache") [Unix.F_OK]; true with _ -> false) then
			legacy_management_interface_start ()
		else
			(* Try to get the initial network setup from the first-boot data written by the host installer. *)
			try
				config := read_management_conf ();
				debug "Read configuration from management.conf file."
			with _ ->
				debug "Could not interpret the configuration in management.conf"

let on_shutdown signal =
	debug "xcp-networkd caught signal %d; performing cleanup actions." signal;
	write_config ();
	exit 0

let on_timer () =
	write_config ()

let reopen_logs _ () = true

let clear_state _ () =
	config := empty_config

let reset_state _ () =
	config := read_management_conf ()

module Interface = struct
	let get_config name =
		get_config !config.interface_config default_interface name

	let update_config name data =
		config := {!config with interface_config = update_config !config.interface_config name data}

	let get_all _ () =
		Sysfs.list ()

	let exists _ ~name =
		List.mem name (Sysfs.list ())

	let get_mac _ ~name =
		Ip.get_mac name

	let is_up _ ~name =
		if List.mem name (Sysfs.list ()) then
			Ip.is_up name
		else
			false

	let get_ipv4_addr _ ~name =
		Ip.get_ipv4 name

	let configure_ipv4 name conf =
		debug "Configuring IPv4 address for %s: %s" name (conf |> rpc_of_ipv4 |> Jsonrpc.to_string);
		match conf with
		| None4 ->
			if List.mem name (get_all () ()) then begin
				if Dhclient.is_running name then
					ignore (Dhclient.stop name);
				Ip.flush_ip_addr name
			end
		| DHCP4 options ->
			let options =
				if !config.gateway_interface = None || !config.gateway_interface = Some name then
					options
				else begin
					debug "%s is not the default gateway interface" name;
					List.filter ((<>) `set_gateway) options
				end
			in
			let options =
				if !config.dns_interface = None || !config.dns_interface = Some name then
					options
				else begin
					debug "%s is not the DNS interface" name;
					List.filter ((<>) `set_dns) options
				end
			in
			if Dhclient.is_running name then
				ignore (Dhclient.stop name);
			ignore (Dhclient.start name options)
		| Static4 addrs ->
			if Dhclient.is_running name then
				ignore (Dhclient.stop name);
			Ip.flush_ip_addr name;
			List.iter (Ip.set_ip_addr name) addrs

	let set_ipv4_conf _ ~name ~conf =
		update_config name {(get_config name) with ipv4_conf = conf};
		(match conf with
		| DHCP4 options ->
			if List.mem `set_gateway options then
				config := {!config with gateway_interface = Some name};
			if List.mem `set_dns options then
				config := {!config with dns_interface = Some name};
		| _ -> ());
		configure_ipv4 name conf

	let get_ipv4_gateway _ ~name =
		let output = Ip.route_show ~version:Ip.V4 name in
		try
			let line = List.find (fun s -> String.startswith "default via" s) (String.split '\n' output) in
			let addr = List.nth (String.split ' ' line) 2 in
			Some (Unix.inet_addr_of_string addr)
		with Not_found -> None

	let configure_ipv4_gateway name address =
		debug "Configuring IPv4 gateway for %s: %s" name (Unix.string_of_inet_addr address);
		if !config.gateway_interface = None || !config.gateway_interface = Some name then
			Ip.set_gateway name address
		else
			debug "%s is not the default gateway interface" name

	let set_ipv4_gateway _ ~name ~address =
		update_config name {(get_config name) with ipv4_gateway = Some address};
		config := {!config with gateway_interface = Some name};
		configure_ipv4_gateway name address

	let get_ipv6_addr _ ~name =
		Ip.get_ipv6 name

	let set_ipv6_conf _ ~name ~conf =
		debug "Configuring IPv6 address for %s: %s" name (conf |> rpc_of_ipv6 |> Jsonrpc.to_string);
		update_config name {(get_config name) with ipv6_conf = conf};
		match conf with
		| None6 ->
			if List.mem name (get_all () ()) then begin
				if Dhclient.is_running ~ipv6:true name then
					ignore (Dhclient.stop ~ipv6:true name);
				ignore (Sysctl.set_ipv6_autoconf name false);
				Ip.flush_ip_addr ~ipv6:true name
			end
		| DHCP6 options ->
			if Dhclient.is_running ~ipv6:true name then
				ignore (Dhclient.stop ~ipv6:true name);
			ignore (Sysctl.set_ipv6_autoconf name false);
			ignore (Dhclient.start ~ipv6:true name options)
		| Autoconf6 ->
			ignore (Sysctl.set_ipv6_autoconf name true);
			Ip.link_set_down name;
			Ip.link_set_up name
		| Static6 addrs ->
			if Dhclient.is_running ~ipv6:true name then
				ignore (Dhclient.stop ~ipv6:true name);
			ignore (Sysctl.set_ipv6_autoconf name false);
			List.iter (Ip.set_ip_addr name) addrs

	let get_ipv6_gateway _ ~name =
		Some (Unix.inet_addr_of_string "fd::1")

	let set_ipv6_gateway _ ~name ~address =
		debug "Configuring IPv6 gateway for %s: %s" name (Unix.string_of_inet_addr address);
		update_config name {(get_config name) with ipv6_gateway = Some address};
		Ip.set_gateway name address

	let set_ipv4_routes _ ~name ~routes =
		debug "Configuring IPv4 static routes for %s: %s" name (String.concat ", " (List.map (fun (i, p, g) ->
			Printf.sprintf "%s/%d/%s" (Unix.string_of_inet_addr i) p (Unix.string_of_inet_addr g)) routes));
		update_config name {(get_config name) with ipv4_routes = routes};
		List.iter (fun (i, p, g) -> Ip.set_route ~network:(i, p) name g) routes

	let get_dns _ ~name =
		let nameservers, domains = Unixext.file_lines_fold (fun (nameservers, domains) line ->
			if String.startswith "nameserver" line then
				let server = List.nth (String.split_f String.isspace line) 1 in
				(Unix.inet_addr_of_string server) :: nameservers, domains
			else if String.startswith "search" line then
				let domains = List.tl (String.split_f String.isspace line) in
				nameservers, domains
			else
				nameservers, domains
		) ([], []) resolv_conf in
		List.rev nameservers, domains

	let configure_dns name nameservers domains =
		if (nameservers <> [] || domains <> []) then begin
			debug "Configuring DNS for %s: nameservers: %s; domains: %s" name
				(String.concat ", " (List.map Unix.string_of_inet_addr nameservers)) (String.concat ", " domains);
			if (!config.dns_interface = None || !config.dns_interface = Some name) then
				let domains' = if domains <> [] then ["search " ^ (String.concat " " domains)] else [] in
				let nameservers' = List.map (fun ip -> "nameserver " ^ (Unix.string_of_inet_addr ip)) nameservers in
				let lines = domains' @ nameservers' in
				Unixext.write_string_to_file resolv_conf ((String.concat "\n" lines) ^ "\n")
			else
				debug "%s is not the DNS interface" name
		end

	let set_dns _ ~name ~nameservers ~domains =
		update_config name {(get_config name) with dns = nameservers, domains};
		config := {!config with dns_interface = Some name};
		configure_dns name nameservers domains

	let get_mtu _ ~name =
		Ip.get_mtu name

	let set_mtu _ ~name ~mtu =
		debug "Configuring MTU for %s: %d" name mtu;
		update_config name {(get_config name) with mtu};
		ignore (Ip.link_set_mtu name mtu)

	let set_ethtool_settings _ ~name ~params =
		debug "Configuring ethtool settings for %s: %s" name
			(String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) params));
		let add_defaults = List.filter (fun (k, v) -> not (List.mem_assoc k params)) default_interface.ethtool_settings in
		let params = params @ add_defaults in
		update_config name {(get_config name) with ethtool_settings = params};
		Ethtool.set_options name params

	let set_ethtool_offload _ ~name ~params =
		debug "Configuring ethtool offload settings for %s: %s" name
			(String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) params));
		let add_defaults = List.filter (fun (k, v) -> not (List.mem_assoc k params)) default_interface.ethtool_offload in
		let params = params @ add_defaults in
		update_config name {(get_config name) with ethtool_offload = params};
		Ethtool.set_offload name params

	let is_connected _ ~name =
		Sysfs.get_carrier name

	let is_physical _ ~name =
		Sysfs.is_physical name

	let bring_up _ ~name =
		debug "Bringing up interface %s" name;
		Ip.link_set_up name

	let bring_down _ ~name =
		debug "Bringing down interface %s" name;
		Ip.link_set_down name

	let is_persistent _ ~name =
		(get_config name).persistent_i

	let set_persistent _ ~name ~value =
		debug "Making interface %s %spersistent" name (if value then "" else "non-");
		update_config name {(get_config name) with persistent_i = value}

	let make_config _ ?(conservative=false) ~config () =
		(* Only attempt to configure interfaces that exist in the system *)
		let all = get_all () () in
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
		debug "** Configuring the following interfaces: %s" (String.concat ", " (List.map (fun (name, _) -> name) config));
		let exec f = if conservative then (try f () with _ -> ()) else f () in
		List.iter (function (name, ({ipv4_conf; ipv4_gateway; ipv6_conf; ipv6_gateway; ipv4_routes; dns=nameservers,domains; mtu;
			ethtool_settings; ethtool_offload; _} as c)) ->
			update_config name c;
			exec (fun () -> if conservative then configure_ipv4 name ipv4_conf else set_ipv4_conf () name ipv4_conf);
			exec (fun () -> match ipv4_gateway with None -> () | Some gateway -> configure_ipv4_gateway name gateway);
			(try set_ipv6_conf () ~name ~conf:ipv6_conf with _ -> ());
			(try match ipv6_gateway with None -> () | Some gateway -> set_ipv6_gateway () ~name ~address:gateway with _ -> ());
			exec (fun () -> set_ipv4_routes () ~name ~routes:ipv4_routes);
			exec (fun () -> configure_dns name nameservers domains);
			exec (fun () -> set_mtu () ~name ~mtu);
			exec (fun () -> bring_up () ~name);
			exec (fun () -> set_ethtool_settings () ~name ~params:ethtool_settings);
			exec (fun () -> set_ethtool_offload () ~name ~params:ethtool_offload)
		) config;
end

module Bridge = struct
	let kind = ref Openvswitch
	let add_default = ref []

	let get_config name =
		get_config !config.bridge_config default_bridge name

	let remove_config name =
		config := {!config with bridge_config = remove_config !config.bridge_config name}

	let update_config name data =
		config := {!config with bridge_config = update_config !config.bridge_config name data}

	let determine_backend () =
		let backend = String.strip String.isspace
			(Unixext.string_of_file (Fhs.etcdir ^ "/network.conf")) in
		match backend with
		| "openvswitch" | "vswitch" -> kind := Openvswitch
		| "bridge" -> kind := Bridge
		| backend ->
			let error = Printf.sprintf "ERROR: network backend unknown (%s)" backend in
			debug "%s" error;
			failwith error

	let get_bond_links_up _ ~name = 
		match !kind with
		| Openvswitch -> Ovs.get_bond_links_up name
		| Bridge -> Proc.get_bond_links_up name

	let get_all _ () =
		match !kind with
		| Openvswitch -> Ovs.list_bridges ()
		| Bridge -> []

	let create _ ?vlan ?mac ?(other_config=[]) ~name () =
		debug "Creating bridge %s%s" name (match vlan with
			| None -> ""
			| Some (parent, vlan) -> Printf.sprintf " (VLAN %d on bridge %s)" vlan parent
		);
		update_config name {get_config name with vlan; bridge_mac=mac; other_config};
		begin match !kind with
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
			ignore (Ovs.create_bridge ?mac ~fail_mode ?external_id ?disable_in_band
				vlan vlan_bug_workaround name)
		| Bridge ->
			ignore (Brctl.create_bridge name);
			Opt.iter (Ip.set_mac name) mac;
			match vlan with
			| None -> ()
			| Some (parent, vlan) ->
				let interface = List.hd (List.filter (fun n ->
					String.startswith "eth" n || String.startswith "bond" n
				) (Sysfs.bridge_to_interfaces parent)) in
				Ip.create_vlan interface vlan;
				let vlan_name = Ip.vlan_name interface vlan in
				Interface.bring_up () ~name:vlan_name;
				Brctl.create_port name vlan_name
		end;
		Interface.bring_up () ~name

	let destroy _ ?(force=false) ~name () =
		Interface.bring_down () ~name;
		match !kind with
		| Openvswitch ->
			if Ovs.get_vlans name = [] || force then begin
				debug "Destroying bridge %s" name;
				remove_config name;
				List.iter (fun dev ->
					Interface.set_ipv4_conf () ~name:dev ~conf:None4;
					Interface.bring_down () ~name:dev
				) (Ovs.bridge_to_interfaces name);
				Interface.set_ipv4_conf () ~name ~conf:None4;
				ignore (Ovs.destroy_bridge name)
			end else
				debug "Not destroying bridge %s, because it has VLANs on top" name
		| Bridge ->
			let ifs = Sysfs.bridge_to_interfaces name in
			let vlans_on_this_parent =
				let interfaces = List.filter (fun n ->
					String.startswith "eth" n || String.startswith "bond" n
				) ifs in
				match interfaces with
				| [] -> []
				| interface :: _ ->
					List.filter (String.startswith (interface ^ ".")) (Sysfs.list ())
			in
			if vlans_on_this_parent = [] || force then begin
				debug "Destroying bridge %s" name;
				remove_config name;
				List.iter (fun dev ->
					Interface.set_ipv4_conf () ~name:dev ~conf:None4;
					Interface.bring_down () ~name:dev;
					if Linux_bonding.is_bond_device dev then
						Linux_bonding.remove_bond_master dev;
					if String.startswith "eth" dev && String.contains dev '.' then
						ignore (Ip.destroy_vlan dev)
				) ifs;
				Interface.set_ipv4_conf () ~name ~conf:None4;
				ignore (Brctl.destroy_bridge name)
			end else
				debug "Not destroying bridge %s, because it has VLANs on top" name

	let get_kind _ () =
		!kind

	let get_ports _ ~name =
		match !kind with
		| Openvswitch -> Ovs.bridge_to_ports name
		| Bridge -> []

	let get_all_ports _ ?(from_cache=false) () =
		if from_cache then
			let ports = List.concat (List.map (fun (_, {ports}) -> ports) !config.bridge_config) in
			List.map (fun (port, {interfaces}) -> port, interfaces) ports
		else
			match !kind with
			| Openvswitch -> List.concat (List.map Ovs.bridge_to_ports (Ovs.list_bridges ()))
			| Bridge -> []

	let get_bonds _ ~name =
		match !kind with
		| Openvswitch -> Ovs.bridge_to_ports name
		| Bridge -> []

	let get_all_bonds _ ?(from_cache=false) () =
		if from_cache then
			let ports = List.concat (List.map (fun (_, {ports}) -> ports) !config.bridge_config) in
			let names = List.map (fun (port, {interfaces}) -> port, interfaces) ports in
			List.filter (fun (_, ifs) -> List.length ifs > 1) names
		else
			match !kind with
			| Openvswitch -> List.concat (List.map Ovs.bridge_to_ports (Ovs.list_bridges ()))
			| Bridge -> []

	let get_vlan _ ~name =
		match !kind with
		| Openvswitch -> Some (Ovs.bridge_to_vlan name)
		| Bridge -> None

	let add_default_flows _ bridge mac interfaces =
		match !kind with
		| Openvswitch -> Ovs.add_default_flows bridge mac interfaces
		| Bridge -> ()

	let add_port _ ?(mac="") ~bridge ~name ~interfaces () =
		let config = get_config bridge in
		let ports =
			if List.mem_assoc name config.ports then
				List.remove_assoc name config.ports
			else
				config.ports
		in
		let ports = (name, {default_port with interfaces; mac}) :: ports in
		update_config bridge {config with ports};
		debug "Adding port %s to bridge %s with interfaces %s%s" name bridge
			(String.concat ", " interfaces)
			(if mac <> "" then " and MAC " ^ mac else "");
		match !kind with
		| Openvswitch ->
			if List.length interfaces = 1 then begin
				List.iter (fun name -> Interface.bring_up () ~name) interfaces;
				ignore (Ovs.create_port (List.hd interfaces) bridge)
			end else begin
				ignore (Ovs.create_bond name interfaces bridge mac);
				List.iter (fun name -> Interface.bring_up () ~name) interfaces
			end;
			if List.mem bridge !add_default then begin
				add_default_flows () bridge mac interfaces;
				add_default := List.filter ((<>) bridge) !add_default
			end
		| Bridge ->
			if List.length interfaces = 1 then begin
				List.iter (fun name -> Interface.bring_up () ~name) interfaces;
				ignore (Brctl.create_port bridge name)
			end else begin
				if not (List.mem name (Sysfs.bridge_to_interfaces bridge)) then begin
					Linux_bonding.add_bond_master name;
					if mac <> "" then Ip.set_mac name mac;
					List.iter (fun name -> Interface.bring_down () ~name) interfaces;
					List.iter (Linux_bonding.add_bond_slave name) interfaces
				end;
				Interface.bring_up () ~name;
				ignore (Brctl.create_port bridge name)
			end

	let remove_port _ ~bridge ~name =
		debug "Removing port %s from bridge %s" name bridge;
		let config = get_config bridge in
		if List.mem_assoc name config.ports then begin
			let ports = List.remove_assoc name config.ports in
			update_config bridge {config with ports}
		end;
		match !kind with
		| Openvswitch ->
			ignore (Ovs.destroy_port name)
		| Bridge ->
			ignore (Brctl.destroy_port bridge name)

	let get_interfaces _ ~name =
		match !kind with
		| Openvswitch ->
			Ovs.bridge_to_interfaces name
		| Bridge ->
			Sysfs.bridge_to_interfaces name

	let get_bond_properties _ ~bridge ~name =
		[]

	let set_bond_properties _ ~bridge ~name ~params =
		debug "Setting bond properties on port %s of bridge %s: %s" name bridge
			(String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) params));
		let config = get_config bridge in
		let port, ports =
			if List.mem_assoc name config.ports then
				List.assoc name config.ports, List.remove_assoc name config.ports
			else
				default_port, config.ports
		in
		let ports = (name, {port with bond_properties = params}) :: ports in
		update_config bridge {config with ports};
		match !kind with
		| Openvswitch ->
			ignore (Ovs.set_bond_properties name params)
		| Bridge ->
			let params =
				if List.mem_assoc "mode" params && List.assoc "mode" params = "lacp" then
					List.replace_assoc "mode" "802.3ad" params
				else params
			in
			Linux_bonding.set_bond_properties name params

	let get_fail_mode _ ~name =
		match !kind with
		| Openvswitch ->
			begin match Ovs.get_fail_mode name with
			| "standalone" -> Some Standalone
			| "secure" -> Some Secure
			| _ -> None
			end
		| Bridge -> None

	let is_persistent _ ~name =
		(get_config name).persistent_b

	let set_persistent _ ~name ~value =
		debug "Making bridge %s %spersistent" name (if value then "" else "non-");
		update_config name {(get_config name) with persistent_b = value}

	let make_config _ ?(conservative=false) ~config () =
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
				let vlan_parents = List.filter_map (function
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
				let current = get_all () () in
				List.filter (function (name, _) -> not (List.mem name current)) config
			end else
				config
		in
		let config = List.sort vlans_go_last config in
		debug "** Configuring the following bridges: %s"
			(String.concat ", " (List.map (fun (name, _) -> name) config));
		List.iter (function (bridge_name, ({ports; vlan; bridge_mac; other_config; _} as c)) ->
			update_config bridge_name c;
			create () ?vlan ?mac:bridge_mac ~other_config ~name:bridge_name ();
			List.iter (fun (port_name, {interfaces; bond_properties; mac}) ->
				add_port () ~mac ~bridge:bridge_name ~name:port_name ~interfaces ();
				if bond_properties <> [] then
					set_bond_properties () ~bridge:bridge_name ~name:port_name ~params:bond_properties
			) ports
		) config
end

let on_startup () =
	Bridge.determine_backend ();
	(* Remove DNSDEV and GATEWAYDEV from Centos networking file, because the interfere
	 * with this daemon. *)
	(try
		let file = String.rtrim (Unixext.string_of_file "/etc/sysconfig/network") in
		let args = String.split '\n' file in
		let args = List.map (fun s -> match (String.split '=' s) with k :: [v] -> k, v | _ -> "", "") args in
		let args = List.filter (fun (k, v) -> k <> "DNSDEV" && k <> "GATEWAYDEV") args in
		let s = String.concat "\n" (List.map (fun (k, v) -> k ^ "=" ^ v) args) ^ "\n" in
		Unixext.write_string_to_file "/etc/sysconfig/network" s
	with _ -> ());
	try
		(* the following is best-effort *)
		read_config ();
		Bridge.make_config () ~conservative:true ~config:!config.bridge_config ();
		Interface.make_config () ~conservative:true ~config:!config.interface_config ();
		(* If there is still a network.dbcache file, move it out of the way. *)
		if (try Unix.access (Filename.concat Fhs.vardir "network.dbcache") [Unix.F_OK]; true with _ -> false) then
			Unix.rename (Filename.concat Fhs.vardir "network.dbcache") (Filename.concat Fhs.vardir "network.dbcache.bak");
	with e ->
		debug "Error while configuring networks on startup: %s\n%s"
			(Printexc.to_string e) (Printexc.get_backtrace ())

