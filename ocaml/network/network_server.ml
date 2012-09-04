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

module D = Debug.Debugger(struct let name = "network_server" end)
open D

type context = unit

let config : config_t ref = ref empty_config

let legacy_management_interface_start () =
	try
		ignore (call_script "/etc/init.d/management-interface" ["start"]);
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
		if (try Unix.access (Filename.concat Fhs.vardir "network.dbcache") [Unix.F_OK]; true with _ -> false) then
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
	debug "Setting gateway interface to %s" name;
	config := {!config with gateway_interface = Some name}

let set_dns_interface _ dbg ~name =
	debug "Setting DNS interface to %s" name;
	config := {!config with dns_interface = Some name}

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
			Ip.get_mac name
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
				let gateway =
					if !config.gateway_interface = None || !config.gateway_interface = Some name then begin
						debug "%s is the default gateway interface" name;
						[`set_gateway]
					end else begin
						debug "%s is NOT the default gateway interface" name;
						[]
					end
				in
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
				if Dhclient.is_running name then
					ignore (Dhclient.stop name);
				Ip.flush_ip_addr name;
				List.iter (Ip.set_ip_addr name) addrs
		) ()

	let get_ipv4_gateway _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			let output = Ip.route_show ~version:Ip.V4 name in
			try
				let line = List.find (fun s -> String.startswith "default via" s) (String.split '\n' output) in
				let addr = List.nth (String.split ' ' line) 2 in
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
					Dhcp6c.stop name;
					Sysctl.set_ipv6_autoconf name false;
					Ip.flush_ip_addr ~ipv6:true name
				end
			| DHCP6 ->
				Dhcp6c.stop name;
				Sysctl.set_ipv6_autoconf name false;
				Ip.flush_ip_addr ~ipv6:true name;
				Dhcp6c.start name
			| Autoconf6 ->
				Dhcp6c.stop name;
				Ip.flush_ip_addr ~ipv6:true name;
				Sysctl.set_ipv6_autoconf name true;
				(* Cannot link set down/up due to CA-89882 - IPv4 default route cleared *)
			| Static6 addrs ->
				Dhcp6c.stop name;
				Sysctl.set_ipv6_autoconf name false;
				Ip.flush_ip_addr ~ipv6:true name;
				List.iter (Ip.set_ip_addr name) addrs
		) ()

	let get_ipv6_gateway _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			let output = Ip.route_show ~version:Ip.V6 name in
			try
				let line = List.find (fun s -> String.startswith "default via" s) (String.split '\n' output) in
				let addr = List.nth (String.split ' ' line) 2 in
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
		) ()

	let set_dns _ dbg ~name ~nameservers ~domains =
		Debug.with_thread_associated dbg (fun () ->
			update_config name {(get_config name) with dns = nameservers, domains};
			if (nameservers <> [] || domains <> []) then begin
				debug "Configuring DNS for %s: nameservers: %s; domains: %s" name
					(String.concat ", " (List.map Unix.string_of_inet_addr nameservers)) (String.concat ", " domains);
				if (!config.dns_interface = None || !config.dns_interface = Some name) then begin
					debug "%s is the DNS interface" name;
					let domains' = if domains <> [] then ["search " ^ (String.concat " " domains)] else [] in
					let nameservers' = List.map (fun ip -> "nameserver " ^ (Unix.string_of_inet_addr ip)) nameservers in
					let lines = domains' @ nameservers' in
					Unixext.write_string_to_file resolv_conf ((String.concat "\n" lines) ^ "\n")
				end else
					debug "%s is NOT the DNS interface" name
			end
		) ()

	let get_mtu _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Ip.get_mtu name
		) ()

	let set_mtu _ dbg ~name ~mtu =
		Debug.with_thread_associated dbg (fun () ->
			debug "Configuring MTU for %s: %d" name mtu;
			update_config name {(get_config name) with mtu};
			ignore (Ip.link_set_mtu name mtu)
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

	let is_connected _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Sysfs.get_carrier name
		) ()

	let is_physical _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			Sysfs.is_physical name
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
			debug "** Configuring the following interfaces: %s" (String.concat ", " (List.map (fun (name, _) -> name) config));
			let exec f = if conservative then (try f () with _ -> ()) else f () in
			List.iter (function (name, ({ipv4_conf; ipv4_gateway; ipv6_conf; ipv6_gateway; ipv4_routes; dns=nameservers,domains; mtu;
				ethtool_settings; ethtool_offload; _} as c)) ->
				update_config name c;
				exec (fun () -> set_ipv4_conf () dbg ~name ~conf:ipv4_conf);
				exec (fun () -> match ipv4_gateway with None -> () | Some gateway ->
					set_ipv4_gateway () dbg ~name ~address:gateway);
				(try set_ipv6_conf () dbg ~name ~conf:ipv6_conf with _ -> ());
				(try match ipv6_gateway with None -> () | Some gateway ->
					set_ipv6_gateway () dbg ~name ~address:gateway with _ -> ());
				exec (fun () -> set_ipv4_routes () dbg ~name ~routes:ipv4_routes);
				exec (fun () -> set_dns () dbg ~name ~nameservers ~domains);
				exec (fun () -> set_mtu () dbg ~name ~mtu);
				exec (fun () -> bring_up () dbg ~name);
				exec (fun () -> set_ethtool_settings () dbg ~name ~params:ethtool_settings);
				exec (fun () -> set_ethtool_offload () dbg ~name ~params:ethtool_offload)
			) config
		) ()
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

	let get_bond_links_up _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !kind with
			| Openvswitch -> Ovs.get_bond_links_up name
			| Bridge -> Proc.get_bond_links_up name
		) ()

	let get_all _ dbg () =
		Debug.with_thread_associated dbg (fun () ->
			match !kind with
			| Openvswitch -> Ovs.list_bridges ()
			| Bridge -> Sysfs.get_all_bridges ()
		) ()

	let create _ dbg ?vlan ?mac ?(other_config=[]) ~name () =
		Debug.with_thread_associated dbg (fun () ->
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
					Interface.bring_up () dbg ~name:vlan_name;
					Brctl.create_port name vlan_name
			end;
			Interface.bring_up () dbg ~name
		) ()

	let destroy _ dbg ?(force=false) ~name () =
		Debug.with_thread_associated dbg (fun () ->
			Interface.bring_down () dbg ~name;
			match !kind with
			| Openvswitch ->
				if Ovs.get_vlans name = [] || force then begin
					debug "Destroying bridge %s" name;
					remove_config name;
					List.iter (fun dev ->
						Interface.set_ipv4_conf () dbg ~name:dev ~conf:None4;
						Interface.bring_down () dbg ~name:dev
					) (Ovs.bridge_to_interfaces name);
					Interface.set_ipv4_conf () dbg ~name ~conf:None4;
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
						Interface.set_ipv4_conf () dbg ~name:dev ~conf:None4;
						Interface.bring_down () dbg ~name:dev;
						if Linux_bonding.is_bond_device dev then
							Linux_bonding.remove_bond_master dev;
						if String.startswith "eth" dev && String.contains dev '.' then
							ignore (Ip.destroy_vlan dev)
					) ifs;
					Interface.set_ipv4_conf () dbg ~name ~conf:None4;
					ignore (Brctl.destroy_bridge name)
				end else
					debug "Not destroying bridge %s, because it has VLANs on top" name
		) ()

	let get_kind _ dbg () =
		Debug.with_thread_associated dbg (fun () ->
			!kind
		) ()

	let get_ports _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !kind with
			| Openvswitch -> Ovs.bridge_to_ports name
			| Bridge -> raise Not_implemented
		) ()

	let get_all_ports _ dbg ?(from_cache=false) () =
		Debug.with_thread_associated dbg (fun () ->
			if from_cache then
				let ports = List.concat (List.map (fun (_, {ports}) -> ports) !config.bridge_config) in
				List.map (fun (port, {interfaces}) -> port, interfaces) ports
			else
				match !kind with
				| Openvswitch -> List.concat (List.map Ovs.bridge_to_ports (Ovs.list_bridges ()))
				| Bridge -> raise Not_implemented
		) ()

	let get_bonds _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !kind with
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
				match !kind with
				| Openvswitch -> List.concat (List.map Ovs.bridge_to_ports (Ovs.list_bridges ()))
				| Bridge -> raise Not_implemented
		) ()

	let get_vlan _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !kind with
			| Openvswitch -> Ovs.bridge_to_vlan name
			| Bridge -> raise Not_implemented
		) ()

	let add_default_flows _ dbg bridge mac interfaces =
		Debug.with_thread_associated dbg (fun () ->
			match !kind with
			| Openvswitch -> Ovs.add_default_flows bridge mac interfaces
			| Bridge -> ()
		) ()

	let add_port _ dbg ?bond_mac ~bridge ~name ~interfaces ?(bond_properties=[]) () =
		Debug.with_thread_associated dbg (fun () ->
			let config = get_config bridge in
			let ports =
				if List.mem_assoc name config.ports then
					List.remove_assoc name config.ports
				else
					config.ports
			in
			let ports = (name, {interfaces; bond_mac; bond_properties}) :: ports in
			update_config bridge {config with ports};
			debug "Adding port %s to bridge %s with interfaces %s%s" name bridge
				(String.concat ", " interfaces)
				(match bond_mac with Some mac -> " and MAC " ^ mac | None -> "");
			match !kind with
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
				if List.length interfaces = 1 then begin
					List.iter (fun name -> Interface.bring_up () dbg ~name) interfaces;
					ignore (Brctl.create_port bridge name)
				end else begin
					if not (List.mem name (Sysfs.bridge_to_interfaces bridge)) then begin
						Linux_bonding.add_bond_master name;
						begin match bond_mac with
							| Some mac -> Ip.set_mac name mac
							| None -> warn "No MAC address specified for the bond"
						end;
						List.iter (fun name -> Interface.bring_down () dbg ~name) interfaces;
						List.iter (Linux_bonding.add_bond_slave name) interfaces;
						let bond_properties =
							if List.mem_assoc "mode" bond_properties && List.assoc "mode" bond_properties = "lacp" then
								List.replace_assoc "mode" "802.3ad" bond_properties
							else bond_properties
						in
						Linux_bonding.set_bond_properties name bond_properties
					end;
					Interface.bring_up () dbg ~name;
					ignore (Brctl.create_port bridge name)
				end
		) ()

	let remove_port _ dbg ~bridge ~name =
		Debug.with_thread_associated dbg (fun () ->
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
		) ()

	let get_interfaces _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !kind with
			| Openvswitch ->
				Ovs.bridge_to_interfaces name
			| Bridge ->
				Sysfs.bridge_to_interfaces name
		) ()

	let get_fail_mode _ dbg ~name =
		Debug.with_thread_associated dbg (fun () ->
			match !kind with
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
					let current = get_all () dbg () in
					List.filter (function (name, _) -> not (List.mem name current)) config
				end else
					config
			in
			let config = List.sort vlans_go_last config in
			debug "** Configuring the following bridges: %s"
				(String.concat ", " (List.map (fun (name, _) -> name) config));
			List.iter (function (bridge_name, ({ports; vlan; bridge_mac; other_config; _} as c)) ->
				update_config bridge_name c;
				create () dbg ?vlan ?mac:bridge_mac ~other_config ~name:bridge_name ();
				List.iter (fun (port_name, {interfaces; bond_properties; bond_mac}) ->
					add_port () dbg ?bond_mac ~bridge:bridge_name ~name:port_name ~interfaces ~bond_properties ()
				) ports
			) config
		) ()
end

let on_startup () =
	let dbg = "startup" in
	Debug.with_thread_associated dbg (fun () ->
		Bridge.determine_backend ();
		let remove_centos_config () =
			(* Remove DNSDEV and GATEWAYDEV from Centos networking file, because the interfere
			 * with this daemon. *)
			try
				let file = String.rtrim (Unixext.string_of_file "/etc/sysconfig/network") in
				let args = String.split '\n' file in
				let args = List.map (fun s -> match (String.split '=' s) with k :: [v] -> k, v | _ -> "", "") args in
				let args = List.filter (fun (k, v) -> k <> "DNSDEV" && k <> "GATEWAYDEV") args in
				let s = String.concat "\n" (List.map (fun (k, v) -> k ^ "=" ^ v) args) ^ "\n" in
				Unixext.write_string_to_file "/etc/sysconfig/network" s
			with _ -> ()
		in
		try
			(* the following is best-effort *)
			read_config ();
			remove_centos_config ();
			Bridge.make_config () dbg ~conservative:true ~config:!config.bridge_config ();
			Interface.make_config () dbg ~conservative:true ~config:!config.interface_config ();
			(* If there is still a network.dbcache file, move it out of the way. *)
			if (try Unix.access (Filename.concat Fhs.vardir "network.dbcache") [Unix.F_OK]; true with _ -> false) then
				Unix.rename (Filename.concat Fhs.vardir "network.dbcache") (Filename.concat Fhs.vardir "network.dbcache.bak");
		with e ->
			debug "Error while configuring networks on startup: %s\n%s"
				(Printexc.to_string e) (Printexc.get_backtrace ())
	) ()

