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
open Listext

(** {2 Helper functions} *)

module Unix = struct
	include Unix
	let inet_addr_of_rpc rpc = Unix.inet_addr_of_string (Rpc.string_of_rpc rpc)
	let rpc_of_inet_addr inet = Rpc.rpc_of_string (Unix.string_of_inet_addr inet)
end

(** {2 Types} *)

type iface = string
type port = string
type bridge = string
type dhcp_options = [`set_gateway | `set_dns]
type ipv4 = None4 | DHCP4 of dhcp_options list | Static4 of (Unix.inet_addr * int) list
type ipv6 = None6 | DHCP6 of dhcp_options list | Autoconf6 | Static6 of (Unix.inet_addr * int) list

type duplex = Duplex_unknown | Duplex_half | Duplex_full

let string_of_duplex = function
	| Duplex_unknown -> "unknown"
	| Duplex_half    -> "half"
	| Duplex_full    -> "full"

let duplex_of_string = function
	| "full"    -> Duplex_full
	| "half"    -> Duplex_half
	| _         -> Duplex_unknown

type interface_config_t = {
	ipv4_conf: ipv4;
	ipv4_gateway: Unix.inet_addr option;
	ipv6_conf: ipv6;
	ipv6_gateway: Unix.inet_addr option;
	ipv4_routes: (Unix.inet_addr * int * Unix.inet_addr) list;
	dns: Unix.inet_addr list * string list;
	mtu: int;
	ethtool_settings: (string * string) list;
	ethtool_offload: (string * string) list;
	persistent_i: bool;
}
type port_config_t = {
	interfaces: iface list;
	bond_properties: (string * string) list;
	mac: string;
}
type bridge_config_t = {
	ports: (port * port_config_t) list;
	vlan: (bridge * int) option;
	bridge_mac: string option;
	other_config: (string * string) list;
	persistent_b: bool;
}
type config_t = {
	interface_config: (iface * interface_config_t) list;
	bridge_config: (bridge * bridge_config_t) list;
	gateway_interface: iface option;
	dns_interface: iface option;
}

(** {2 Default configuration} *)

let default_interface = {
	ipv4_conf = None4;
	ipv4_gateway = None;
	ipv6_conf = None6;
	ipv6_gateway = None;
	ipv4_routes = [];
	dns = [], [];
	mtu = 1500;
	ethtool_settings = [];
	ethtool_offload = ["gro", "off"; "lro", "off"];
	persistent_i = false;
}
let default_bridge = {
	ports = [];
	vlan = None;
	bridge_mac = None;
	other_config = [];
	persistent_b = false;
}
let default_port = {
	interfaces = [];
	bond_properties = [];
	mac = "";
}

(** {2 Configuration manipulation} *)

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

(** {2 API functions} *)

external reopen_logs: unit -> bool = ""
external clear_state: unit -> unit = ""
external reset_state: unit -> unit = ""

module Interface = struct
	external get_all : unit -> iface list = ""
	external exists : name:iface -> bool = ""
	external get_mac : name:iface -> string = ""
	external is_up : name:iface -> bool = ""
	external get_ipv4_addr : name:iface -> (Unix.inet_addr * int) list = ""
	external set_ipv4_conf : name:iface -> conf:ipv4 -> unit = ""
	external get_ipv4_gateway : name:iface -> Unix.inet_addr option = ""
	external set_ipv4_gateway : name:iface -> address:Unix.inet_addr -> unit = ""
	external get_ipv6_addr : name:iface -> (Unix.inet_addr * int) list = ""
	external set_ipv6_conf : name:iface -> conf:ipv6 -> unit = ""
	external get_ipv6_gateway : name:iface -> Unix.inet_addr option = ""
	external set_ipv6_gateway : name:iface -> address:Unix.inet_addr -> unit = ""
	external set_ipv4_routes : name:iface -> routes:(Unix.inet_addr * int * Unix.inet_addr) list -> unit = ""
	external get_dns : name:iface -> Unix.inet_addr list * string list = ""
	external set_dns : name:iface -> nameservers:Unix.inet_addr list -> domains:string list -> unit = ""
	external get_mtu : name:iface -> int = ""
	external set_mtu : name:iface -> mtu:int -> unit = ""
	external set_ethtool_settings : name:iface -> params:(string * string) list -> unit = ""
	external set_ethtool_offload : name:iface -> params:(string * string) list -> unit = ""
	external is_connected : name:iface -> bool = ""
	external is_physical : name:iface -> bool = ""
	external bring_up : name:iface -> unit = ""
	external bring_down : name:iface -> unit = ""
	external is_persistent : name:iface -> bool = ""
	external set_persistent : name:iface -> value:bool -> unit = ""
	external make_config : ?conservative:bool -> config:(iface * interface_config_t) list-> unit -> unit = ""
end

type kind = Openvswitch | Bridge
type bond_mode = Balance_slb | Active_backup | Lacp
type fail_mode = Standalone | Secure

module Bridge = struct
	external get_all : unit -> bridge list = ""
	external get_bond_links_up : name:port -> int = ""
	external create : ?vlan:(bridge * int) ->
		?mac:string -> ?other_config:(string * string) list -> name:bridge -> unit -> unit = ""
	external destroy : ?force:bool -> name:bridge -> unit -> unit = ""
	external get_kind : unit -> kind = ""
	external get_ports : name:bridge -> (port * iface list) list = ""
	external get_all_ports : ?from_cache:bool -> unit -> (port * iface list) list = ""
	external get_bonds : name:bridge -> (port * iface list) list = ""
	external get_all_bonds : ?from_cache:bool -> unit -> (port * iface list) list = ""
	external is_persistent : name:bridge -> bool = ""
	external set_persistent : name:bridge -> value:bool -> unit = ""
	external get_vlan : name:bridge -> (bridge * int) option = ""
	external add_port : ?mac:string -> bridge:bridge -> name:port -> interfaces:iface list -> unit -> unit = ""
	external remove_port : bridge:bridge -> name:port -> unit = ""
	external get_interfaces : name:bridge -> iface list = ""
	external get_bond_properties : bridge:bridge -> name:string -> (string * string) list = ""
	external set_bond_properties : bridge:bridge -> name:string -> params:(string * string) list -> unit = ""
	external get_fail_mode : name:bridge -> fail_mode option = ""
	external make_config : ?conservative:bool -> config:(bridge * bridge_config_t) list-> unit -> unit = ""
end

