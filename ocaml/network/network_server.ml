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

type interface = {
	ipv4_addr: ipv4;
	ipv4_gateway: Unix.inet_addr option;
	ipv6_addr: ipv6;
	ipv6_gateway: Unix.inet_addr option;
	dns: Unix.inet_addr list;
	mtu: int;
	persistent_i: bool;
}
and port = {
	interfaces: string list;
	bond_properties: (string * string) list;
	mac: string;
}
and bridge = {
	ports: (string * port) list;
	vlan: (string * int) option;
	bridge_mac: string option;
	fail_mode: fail_mode option;
	vlan_bug_workaround: bool option;
	persistent_b: bool;
}
and config_t = {
	interface_config: (string * interface) list;
	bridge_config: (string * bridge) list;
} with rpc

let config : config_t ref = ref {interface_config = []; bridge_config = []}

let read_management_conf () =
	let management_conf = Unixext.string_of_file (Xapi_globs.first_boot_dir ^ "data/management.conf") in
	let args = String.split '\n' (String.rtrim management_conf) in
	let args = List.map (fun s -> match (String.split '=' s) with k :: [v] -> k, String.strip ((=) '\'') v | _ -> "", "") args in
	debug "Firstboot file management.conf has: %s" (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) args));
	let device = List.assoc "LABEL" args in
	let bridge_name = Util_inventory.lookup Util_inventory._management_interface in
	debug "Management bridge in inventory file: %s" bridge_name;
	let ipv4_addr, ipv4_gateway, dns =
		match List.assoc "MODE" args with
		| "static" ->
			let ip = List.assoc "IP" args |> Unix.inet_addr_of_string in
			let prefixlen = List.assoc "NETMASK" args |> Unix.inet_addr_of_string |> netmask_to_prefixlen in
			let gateway =
				if List.mem_assoc "GATEWAY" args then
					Some (List.assoc "GATEWAY" args |> Unix.inet_addr_of_string)
				else None
			in
			let dns =
				if List.mem_assoc "DNS" args then
					List.map Unix.inet_addr_of_string (String.split ',' (List.assoc "DNS" args))
				else []
			in
			Static4 [ip, prefixlen], gateway, dns
		| "dhcp" | _ ->
			DHCP4 [`set_gateway; `set_dns], None, []
	in
	let interface = {
		ipv4_addr;
		ipv4_gateway;
		ipv6_addr = None6;
		ipv6_gateway = None;
		dns;
		mtu = 1500;
		persistent_i = true;
	} in
	let bridge = {
		ports = [device, {interfaces = [device]; bond_properties = []; mac = ""}];
		vlan = None;
		bridge_mac = None;
		fail_mode = None;
		vlan_bug_workaround = None;
		persistent_b = true
	} in
	{interface_config = [bridge_name, interface]; bridge_config = [bridge_name, bridge]}

let write_config () =
	let persistent_config = {
		interface_config = List.filter (fun (name, interface) -> interface.persistent_i) !config.interface_config;
		bridge_config = List.filter (fun (name, bridge) -> bridge.persistent_b) !config.bridge_config;
	} in
	let config_json = persistent_config |> rpc_of_config_t |> Jsonrpc.to_string in
	Unixext.write_string_to_file config_file_path config_json

let read_config () =
	try
		let config_json = Unixext.string_of_file config_file_path in
		config := config_json |> Jsonrpc.of_string |> config_t_of_rpc;
		debug "Read configuration from networkd.db file."
	with _ ->
		(* No configuration file found. Assume first-boot and try to get the initial
		 * network setup from the first-boot data written by the host installer. *)
		try
			config := read_management_conf ();
			debug "Read configuration from management.conf file."
		with _ ->
			debug "Could not interpret the configuration in management.conf"

let get_config config default name =
	if List.mem_assoc name config = false then
		default
	else
		List.assoc name config

let remove_config config name =
	if List.mem_assoc name config then
		List.remove_assoc name config
	else
		config

let update_config config name data =
	if List.mem_assoc name config then begin
		List.replace_assoc name data config
	end else
		(name, data) :: config

let on_shutdown signal =
	debug "xcp-networkd caught signal %d; performing cleanup actions." signal;
	write_config ();
	exit 0

let on_timer () =
	write_config ()

let reopen_logs _ () =
	try
		debug "Reopening logfiles";
		Logs.reopen ();
		debug "Logfiles reopened";
		true
	with _ -> false

module Interface = struct
	let default = {
		ipv4_addr = None4;
		ipv4_gateway = None;
		ipv6_addr = None6;
		ipv6_gateway = None;
		dns = [];
		mtu = 1500;
		persistent_i = false;
	}

	let get_config name =
		get_config !config.interface_config default name

	let update_config name data =
		config := {!config with interface_config = update_config !config.interface_config name data}

	let get_all _ () =
		Sysfs.list ()

	let get_mac _ name =
		Ip.get_mac name

	let is_up _ name =
		if List.mem name (Sysfs.list ()) then
			Ip.is_up name
		else
			false

	let get_ipv4_addr _ name =
		Ip.get_ipv4 name

	let set_ipv4_addr _ name addr =
		debug "Configuring IPv4 address for %s: %s" name (addr |> rpc_of_ipv4 |> Jsonrpc.to_string);
		update_config name {(get_config name) with ipv4_addr = addr};
		match addr with
		| None4 ->
			if List.mem name (get_all () ()) then begin
				if Dhclient.is_running name then
					ignore (Dhclient.stop name);
				Ip.flush_ip_addr name
			end
		| DHCP4 options ->
			if Dhclient.is_running name then
				ignore (Dhclient.stop name);
			ignore (Dhclient.start name options)
		| Static4 addrs ->
			if Dhclient.is_running name then
				ignore (Dhclient.stop name);
			List.iter (Ip.set_ip_addr name) addrs

	let get_ipv4_gateway _ name =
		let output = Ip.route_show ~version:Ip.V4 name in
		try
			let line = List.find (fun s -> String.startswith "default via" s) (String.split '\n' output) in
			let addr = List.nth (String.split ' ' line) 2 in
			Some (Unix.inet_addr_of_string addr)
		with Not_found -> None

	let set_ipv4_gateway _ name gateway =
		debug "Configuring IPv4 gateway for %s: %s" name (Unix.string_of_inet_addr gateway);
		update_config name {(get_config name) with ipv4_gateway = Some gateway};
		Ip.set_gateway name gateway

	let get_ipv6_addr _ name =
		Ip.get_ipv6 name

	let set_ipv6_addr _ name addr =
		debug "Configuring IPv6 address for %s: %s" name (addr |> rpc_of_ipv6 |> Jsonrpc.to_string);
		update_config name {(get_config name) with ipv6_addr = addr};
		match addr with
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

	let get_ipv6_gateway _ name =
		Some (Unix.inet_addr_of_string "fd::1")

	let set_ipv6_gateway _ name gateway =
		debug "Configuring IPv6 gateway for %s: %s" name (Unix.string_of_inet_addr gateway);
		update_config name {(get_config name) with ipv6_gateway = Some gateway};
		Ip.set_gateway name gateway

	let get_dns _ name =
		let servers = Unixext.file_lines_fold (fun servers line ->
			if String.startswith "nameserver" line then begin
				let server = List.nth (String.split_f String.isspace line) 1 in
				(Unix.inet_addr_of_string server) :: servers
			end else
				servers
		) [] resolv_conf in
		List.rev servers

	let set_dns _ name dns =
		debug "Configuring DNS for %s: %s" name (String.concat ", " (List.map Unix.string_of_inet_addr dns));
		update_config name {(get_config name) with dns};
		if dns <> [] then
			let lines = Unixext.file_lines_fold (fun lines line ->
				if not (String.startswith "nameserver" line) then
					line :: lines
				else
					lines
			) [] resolv_conf in
			let lines = List.rev lines in
			let lines = lines @ List.map (fun ip -> "nameserver " ^ (Unix.string_of_inet_addr ip)) dns in
			Unixext.write_string_to_file resolv_conf ((String.concat "\n" lines) ^ "\n")

	let get_mtu _ name =
		Ip.get_mtu name

	let set_mtu _ name mtu =
		debug "Configuring MTU for %s: %d" name mtu;
		update_config name {(get_config name) with mtu};
		ignore (Ip.link_set_mtu name mtu)

	let is_connected _ name =
		Sysfs.get_carrier name

	let is_physical _ name =
		Sysfs.is_physical name

	let bring_up _ name =
		debug "Bringing up interface %s" name;
		Ip.link_set_up name

	let bring_down _ name =
		debug "Bringing down interface %s" name;
		Ip.link_set_down name

	let is_persistent _ name =
		(get_config name).persistent_i

	let set_persistent _ name v =
		debug "Making interface %s %spersistent" name (if v then "" else "non-");
		update_config name {(get_config name) with persistent_i = v}

	let make_config () =
		let all = get_all () () in
		(* Do not touch physical interfaces that are already up *)
		let exclude = List.filter (fun interface -> is_up () interface && is_physical () interface) all in
		List.iter (function (name, {ipv4_addr; ipv4_gateway; ipv6_addr; ipv6_gateway; dns; mtu; _}) ->
			if not (List.mem name exclude) then begin
				(* best effort *)
				(try set_ipv4_addr () name ipv4_addr with _ -> ());
				(try match ipv4_gateway with None -> () | Some gateway -> set_ipv4_gateway () name gateway with _ -> ());
				(try set_ipv6_addr () name ipv6_addr with _ -> ());
				(try match ipv6_gateway with None -> () | Some gateway -> set_ipv6_gateway () name gateway with _ -> ());
				(try set_dns () name dns with _ -> ());
				(try set_mtu () name mtu with _ -> ());
				(try bring_up () name with _ -> ())
			end
		) !config.interface_config
end

module Bridge = struct
	let default_bridge = {
		ports = [];
		vlan = None;
		bridge_mac = None;
		fail_mode = None;
		vlan_bug_workaround = None;
		persistent_b = false;
	}
	let default_port = {
		interfaces = [];
		bond_properties = [];
		mac = "";
	}
	let kind = ref Openvswitch
	let add_default = ref []

	let get_config name =
		get_config !config.bridge_config default_bridge name

	let remove_config name =
		config := {!config with bridge_config = remove_config !config.bridge_config name}

	let update_config name data =
		config := {!config with bridge_config = update_config !config.bridge_config name data}

	let determine_backend () =
		let backend = String.strip String.isspace (Unixext.string_of_file "/etc/xensource/network.conf") in
		match backend with
		| "openvswitch" | "vswitch" -> kind := Openvswitch
		| "bridge" -> kind := Bridge
		| backend ->
			let error = Printf.sprintf "ERROR: network backend unknown (%s)" backend in
			debug "%s" error;
			failwith error

	let get_all _ () =
		match !kind with
		| Openvswitch -> Ovs.list_bridges ()
		| Bridge -> []

	let create _ ?vlan ?vlan_bug_workaround ?mac ?fail_mode name =
		debug "Creating bridge %s%s" name (match vlan with
			| None -> ""
			| Some (parent, vlan) -> Printf.sprintf " (VLAN %d on bridge %s)" vlan parent
		);
		update_config name {get_config name with vlan; bridge_mac=mac; fail_mode; vlan_bug_workaround};
		begin match !kind with
		| Openvswitch ->
			let fail_mode = match fail_mode with
				| None | Some Standalone -> "standalone"
				| Some Secure ->
					(try if Ovs.get_fail_mode name <> "secure" then
						add_default := name :: !add_default
					with _ -> ());
					"secure"
			in
			ignore (Ovs.create_bridge ?mac ~fail_mode vlan vlan_bug_workaround name)
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
				Interface.bring_up () vlan_name;
				Brctl.create_port name vlan_name
		end;
		Interface.bring_up () name

	let destroy _ ?(force=false) name =
		Interface.bring_down () name;
		match !kind with
		| Openvswitch ->
			if Ovs.get_vlans name = [] || force then begin
				debug "Destroying bridge %s" name;
				remove_config name;
				List.iter (fun dev ->
					Interface.set_ipv4_addr () dev None4;
					Interface.bring_down () dev
				) (Ovs.bridge_to_interfaces name);
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
					Interface.set_ipv4_addr () dev None4;
					Interface.bring_down () dev;
					if Sysfs.is_bond_device dev then
						Sysfs.remove_bond_master dev;
					if String.startswith "eth" dev && String.contains dev '.' then
						ignore (Ip.destroy_vlan dev)
				) ifs;
				ignore (Brctl.destroy_bridge name)
			end else
				debug "Not destroying bridge %s, because it has VLANs on top" name

	let get_kind _ name =
		!kind

	let get_ports _ name =
		match !kind with
		| Openvswitch -> Ovs.bridge_to_ports name
		| Bridge -> []

	let get_vlan _ name =
		match !kind with
		| Openvswitch -> Some (Ovs.bridge_to_vlan name)
		| Bridge -> None

	let add_default_flows _ bridge mac interfaces =
		match !kind with
		| Openvswitch -> Ovs.add_default_flows bridge mac interfaces
		| Bridge -> ()

	let add_port _ ?(mac="") bridge name interfaces =
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
				List.iter (Interface.bring_up ()) interfaces;
				ignore (Ovs.create_port (List.hd interfaces) bridge)
			end else begin
				ignore (Ovs.create_bond name interfaces bridge mac);
				List.iter (Interface.bring_up ()) interfaces;
				Interface.bring_up () name
			end;
			if List.mem bridge !add_default then begin
				add_default_flows () bridge mac interfaces;
				add_default := List.filter ((<>) bridge) !add_default
			end
		| Bridge ->
			if List.length interfaces = 1 then begin
				List.iter (Interface.bring_up ()) interfaces;
				ignore (Brctl.create_port bridge name)
			end else begin
				if not (List.mem name (Sysfs.bridge_to_interfaces bridge)) then begin
					Sysfs.add_bond_master name;
					if mac <> "" then Ip.set_mac name mac;
					List.iter (Interface.bring_down ()) interfaces;
					List.iter (Sysfs.add_bond_slave name) interfaces
				end;
				Interface.bring_up () name;
				ignore (Brctl.create_port bridge name)
			end

	let remove_port _ bridge name =
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

	let get_interfaces _ name =
		["eth0"]

	let get_bond_properties _ bridge name =
		[]

	let set_bond_properties _ bridge name props =
		debug "Setting bond properties on port %s of bridge %s: %s" name bridge
			(String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) props));
		let config = get_config bridge in
		let port, ports =
			if List.mem_assoc name config.ports then
				List.assoc name config.ports, List.remove_assoc name config.ports
			else
				default_port, config.ports
		in
		let ports = (name, {port with bond_properties = props}) :: ports in
		update_config bridge {config with ports};
		match !kind with
		| Openvswitch ->
			ignore (Ovs.set_bond_properties name props)
		| Bridge ->
			Interface.bring_down () name;
			Sysfs.set_bond_properties name props;
			Interface.bring_up () name

	let get_fail_mode _ bridge =
		match !kind with
		| Openvswitch ->
			begin match Ovs.get_fail_mode bridge with
			| "standalone" -> Some Standalone
			| "secure" -> Some Secure
			| _ -> None
			end
		| Bridge -> None

	let is_persistent _ name =
		(get_config name).persistent_b

	let set_persistent _ name v =
		debug "Making bridge %s %spersistent" name (if v then "" else "non-");
		update_config name {(get_config name) with persistent_b = v}

	let make_config () =
		let vlans_go_last (_, {vlan=vlan_of_a}) (_, {vlan=vlan_of_b}) =
			if vlan_of_a = None && vlan_of_b = None then 0
			else if vlan_of_a <> None && vlan_of_b = None then 1
			else if vlan_of_a = None && vlan_of_b <> None then -1
			else 0
		in
		let bridge_config = List.sort vlans_go_last !config.bridge_config in
		let current = get_all () () in
		List.iter (function (bridge_name, {ports; vlan; bridge_mac; fail_mode; _}) ->
			(* Do not try to recreate bridges that already exist *)
			if not (List.mem bridge_name current) then begin
				create () ?vlan ?mac:bridge_mac ?fail_mode bridge_name;
				List.iter (fun (port_name, {interfaces; bond_properties; mac}) ->
					add_port () ~mac bridge_name port_name interfaces;
					if bond_properties <> [] then
						set_bond_properties () bridge_name port_name bond_properties;
					if fail_mode = Some Secure && bridge_mac <> None then
					match fail_mode, bridge_mac with
					| Some Secure, Some mac -> add_default_flows () bridge_name mac interfaces
					| _ -> ()
				) ports
			end
		) bridge_config
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
		Bridge.make_config ();
		Interface.make_config ()
	with _ -> ()

