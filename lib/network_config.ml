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

open Network_interface

module D = Debug.Make(struct let name = "network_config" end)
open D

exception Read_error
exception Write_error

let config_file_path = "/var/lib/xcp/networkd.db"

let bridge_naming_convention (device: string) =
	if Astring.String.is_prefix ~affix:"eth" device
		then ("xenbr" ^ (String.sub device 3 (String.length device - 3)))
	else ("br" ^ device)

let read_management_conf () =
	try
		let management_conf = Xapi_stdext_unix.Unixext.string_of_file ("/etc/firstboot.d/data/management.conf") in
		let args = Astring.String.cuts ~empty:false ~sep:"\n" (String.trim management_conf) in
		let args = List.map (fun s ->
			match (Astring.String.cuts ~empty:false ~sep:"=" s) with
			| k :: [v] -> k, Astring.String.trim ~drop:((=) '\'') v
			| _ -> "", ""
		) args in
		debug "Firstboot file management.conf has: %s" (String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) args));
		let device = List.assoc "LABEL" args in
		let vlan = if List.mem_assoc "VLAN" args then Some (List.assoc "VLAN" args) else None in
		Inventory.reread_inventory ();
		let bridge_name = Inventory.lookup Inventory._management_interface in
		debug "Management bridge in inventory file: %s" bridge_name;
		let mac = Network_utils.Ip.get_mac device in
		let ipv4_conf, ipv4_gateway, dns =
			match List.assoc "MODE" args with
			| "static" ->
				let ip = List.assoc "IP" args |> Unix.inet_addr_of_string in
				let prefixlen = List.assoc "NETMASK" args |> netmask_to_prefixlen in
				let gateway =
					if List.mem_assoc "GATEWAY" args then
						Some (List.assoc "GATEWAY" args |> Unix.inet_addr_of_string)
					else None
				in
				let nameservers =
					if List.mem_assoc "DNS" args && List.assoc "DNS" args <> "" then
						List.map Unix.inet_addr_of_string (Astring.String.cuts ~empty:false ~sep:"," (List.assoc "DNS" args))
					else []
				in
				let domains =
					if List.mem_assoc "DOMAIN" args && List.assoc "DOMAIN" args <> "" then
						Astring.String.cuts ~empty:false ~sep:" " (List.assoc "DOMAIN" args)
					else []
				in
				let dns = nameservers, domains in
				Static4 [ip, prefixlen], gateway, dns
			| "dhcp" | _ ->
				DHCP4, None, ([], [])
		in
		let phy_interface = {default_interface with persistent_i = true} in
		let bridge_interface = {default_interface with ipv4_conf; ipv4_gateway; persistent_i = true} in
		let interface_config, bridge_config =
			let primary_bridge_conf = {default_bridge with
				bridge_mac = Some mac;
				ports = [device, {default_port with interfaces = [device]}];
				persistent_b = true
				} in
			if bridge_name = "" then
				[], []
			else begin
				match vlan with
				| None ->
					[device, phy_interface; bridge_name, bridge_interface],
					[bridge_name, primary_bridge_conf]
				| Some vlan ->
					let parent = bridge_naming_convention device in
					let secondary_bridge_conf = {default_bridge with
						vlan = Some (parent, int_of_string vlan);
						bridge_mac = (Some mac);
						persistent_b = true
					} in
					let parent_bridge_interface = {default_interface with persistent_i = true} in
					[device, phy_interface; parent, parent_bridge_interface; bridge_name, bridge_interface],
					[parent, primary_bridge_conf; bridge_name, secondary_bridge_conf]
			end
		in
		{interface_config = interface_config; bridge_config = bridge_config;
			gateway_interface = Some bridge_name; dns_interface = Some bridge_name}
	with e ->
		error "Error while trying to read firstboot data: %s\n%s"
			(Printexc.to_string e) (Printexc.get_backtrace ());
		raise Read_error

let write_config config =
	try
		let config_json = config |> rpc_of_config_t |> Jsonrpc.to_string in
		Xapi_stdext_unix.Unixext.write_string_to_file config_file_path config_json
	with e ->
		error "Error while trying to write networkd configuration: %s\n%s"
			(Printexc.to_string e) (Printexc.get_backtrace ());
		raise Write_error

let read_config () =
	try
		let config_json = Xapi_stdext_unix.Unixext.string_of_file config_file_path in
		config_json |> Jsonrpc.of_string |> config_t_of_rpc
	with
		| Unix.Unix_error (Unix.ENOENT, _, file) ->
			info "Cannot read networkd configuration file %s because it does not exist." file;
			raise Read_error
		| e ->
			info "Error while trying to read networkd configuration: %s\n%s"
				(Printexc.to_string e) (Printexc.get_backtrace ());
			raise Read_error
