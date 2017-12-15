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

(** {2 Helper functions} *)

let service_name = "networkd"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir service_name)

let uri () = "file:" ^ !default_path

let comp f g x = f (g x)
let (++) f g x = comp f g x

module Unix = struct
	include Unix
	let inet_addr_of_rpc rpc = Unix.inet_addr_of_string (Rpc.string_of_rpc rpc)
	let rpc_of_inet_addr inet = Rpc.rpc_of_string (Unix.string_of_inet_addr inet)
end

let netmask_to_prefixlen netmask =
	Scanf.sscanf netmask "%d.%d.%d.%d" (fun a b c d ->
		let rec length l x =
			if x > 0 then
				length (succ l) (x lsr 1)
			else
				l
		in
		let masks = List.map ((-) 255) [a; b; c; d] in
		32 - (List.fold_left length 0 masks)
	)

let prefixlen_to_netmask len =
	let mask l =
		if l <= 0 then
			0
		else if l > 8 then
			255
		else
			256 - (1 lsl (8 - l))
	in
	let lens = [len; len - 8; len - 16; len - 24] in
	let masks = List.map (string_of_int ++ mask) lens in
	String.concat "." masks

(** {2 Exceptions} *)

exception Script_missing of string
exception Script_error of (string * string) list
exception Read_error of string
exception Write_error of string
exception Not_implemented
exception Vlan_in_use of (string * int)

(** {2 Types} *)

type debug_info = string
type iface = string
type port = string
type bridge = string
type dhcp_options = [`set_gateway | `set_dns]
type ipv4 = None4 | DHCP4 | Static4 of (Unix.inet_addr * int) list
type ipv6 = None6 | Linklocal6 | DHCP6 | Autoconf6 | Static6 of (Unix.inet_addr * int) list

type duplex = Duplex_unknown | Duplex_half | Duplex_full

let string_of_duplex = function
	| Duplex_unknown -> "unknown"
	| Duplex_half    -> "half"
	| Duplex_full    -> "full"

let duplex_of_string = function
	| "full"    -> Duplex_full
	| "half"    -> Duplex_half
	| _         -> Duplex_unknown

type port_kind =
	| Basic
	| PVS_proxy

let string_of_port_kind = function
	| Basic -> "basic"
	| PVS_proxy -> "PVS proxy"

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
	bond_mac: string option;
	kind: port_kind;
}
type bridge_config_t = {
	ports: (port * port_config_t) list;
	vlan: (bridge * int) option;
	bridge_mac: string option;
	igmp_snooping: bool option;
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
	ethtool_offload = ["lro", "off"];
	persistent_i = false;
}
let default_bridge = {
	ports = [];
	vlan = None;
	bridge_mac = None;
	igmp_snooping = None;
	other_config = [];
	persistent_b = false;
}
let default_port = {
	interfaces = [];
	bond_properties = [];
	bond_mac = None;
	kind = Basic;
}
let default_config = {
	interface_config = [];
	bridge_config = [];
	gateway_interface = None;
	dns_interface = None
}

(** {2 RPC functions} *)

let interface_config_t_add_defaults rpc =
	Rpc.struct_extend rpc (rpc_of_interface_config_t default_interface)

let port_config_t_add_defaults rpc =
	Rpc.struct_extend rpc (rpc_of_port_config_t default_port)

let bridge_config_t_add_defaults rpc =
	(* This needs some special treatment, because bridge_config_t contains a list of port_config_t records
	 * that may need to have defaults inserted. Rpc.struct_extend does not currently support this. *)
	let open Rpc in
	let rpc' = Rpc.struct_extend rpc (rpc_of_bridge_config_t default_bridge) in
	match rpc' with
	| Dict r ->
		Dict (List.map (fun (k, v) ->
			match k, v with
			| "ports", Dict v' ->
				k, Dict (List.map (fun (name, config) -> name, port_config_t_add_defaults config) v')
			| x -> x
		) r)
	| x -> x

let config_t_add_defaults rpc =
	(* This needs some special treatment, because config_t contains lists of bridge_config_t and
	 * interface_config_t records that may need to have defaults inserted. Rpc.struct_extend does
	 * not currently support this. *)
	let open Rpc in
	let rpc' = Rpc.struct_extend rpc (rpc_of_config_t default_config) in
	match rpc' with
	| Dict r ->
		Dict (List.map (fun (k, v) ->
			match k, v with
			| "bridge_config", Dict v' ->
				k, Dict (List.map (fun (name, config) -> name, bridge_config_t_add_defaults config) v')
			| "interface_config", Dict v' ->
				k, Dict (List.map (fun (name, config) -> name, interface_config_t_add_defaults config) v')
			| x -> x
		) r)
	| x -> x

let interface_config_t_of_rpc rpc = rpc |> interface_config_t_add_defaults |> interface_config_t_of_rpc
let port_config_t_of_rpc rpc =      rpc |> port_config_t_add_defaults      |> port_config_t_of_rpc
let bridge_config_t_of_rpc rpc =    rpc |> bridge_config_t_add_defaults    |> bridge_config_t_of_rpc
let config_t_of_rpc rpc =           rpc |> config_t_add_defaults           |> config_t_of_rpc

(** {2 Configuration manipulation} *)

let empty_config = default_config

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
	let replace_assoc key new_value existing =
		(key, new_value) :: (List.filter (fun (k, _) -> k <> key) existing) in

	if List.mem_assoc name config then begin
		replace_assoc name data config
	end else
		(name, data) :: config

(** {2 API functions} *)

external clear_state: unit -> unit = ""
external reset_state: unit -> unit = ""

external set_gateway_interface: debug_info -> name:iface -> unit = ""
external set_dns_interface: debug_info -> name:iface -> unit = ""

module Interface = struct
	external get_all : debug_info -> unit -> iface list = ""
	external exists : debug_info -> name:iface -> bool = ""
	external get_mac : debug_info -> name:iface -> string = ""
	external is_up : debug_info -> name:iface -> bool = ""
	external get_ipv4_addr : debug_info -> name:iface -> (Unix.inet_addr * int) list = ""
	external set_ipv4_conf : debug_info -> name:iface -> conf:ipv4 -> unit = ""
	external get_ipv4_gateway : debug_info -> name:iface -> Unix.inet_addr option = ""
	external get_ipv6_addr : debug_info -> name:iface -> (Unix.inet_addr * int) list = ""
	external get_dns : debug_info -> name:iface -> Unix.inet_addr list * string list = ""
	external get_mtu : debug_info -> name:iface -> int = ""
	external get_capabilities : debug_info -> name:iface -> string list = ""
	external is_connected : debug_info -> name:iface -> bool = ""
	external is_physical : debug_info -> name:iface -> bool = ""
	external has_vlan: debug_info -> name:iface -> vlan:int -> bool = ""
	external bring_down : debug_info -> name:iface -> unit = ""
	external set_persistent : debug_info -> name:iface -> value:bool -> unit = ""
	external make_config : debug_info -> ?conservative:bool -> config:(iface * interface_config_t) list-> unit -> unit = ""
end

type kind = Openvswitch | Bridge
type bond_mode = Balance_slb | Active_backup | Lacp
type fail_mode = Standalone | Secure

let string_of_kind = function
	| Openvswitch -> "openvswitch"
	| Bridge -> "bridge"

module Bridge = struct
	external get_all : debug_info -> unit -> bridge list = ""
	external create : debug_info -> ?vlan:(bridge * int) ->
		?mac:string -> ?igmp_snooping:bool -> ?other_config:(string * string) list -> name:bridge -> unit -> unit = ""
	external destroy : debug_info -> ?force:bool -> name:bridge -> unit -> unit = ""
	external get_kind : debug_info -> unit -> kind = ""
	external get_all_ports : debug_info -> ?from_cache:bool -> unit -> (port * iface list) list = ""
	external get_all_bonds : debug_info -> ?from_cache:bool -> unit -> (port * iface list) list = ""
	external set_persistent : debug_info -> name:bridge -> value:bool -> unit = ""
	external add_port : debug_info -> ?bond_mac:string -> bridge:bridge -> name:port -> interfaces:iface list ->
		?bond_properties:(string * string) list -> ?kind:port_kind -> unit -> unit = ""
	external remove_port : debug_info -> bridge:bridge -> name:port -> unit = ""
	external get_interfaces : debug_info -> name:bridge -> iface list = ""
	external get_physical_interfaces : debug_info -> name:bridge -> iface list = ""
	external make_config : debug_info -> ?conservative:bool -> config:(bridge * bridge_config_t) list-> unit -> unit = ""
end

exception PVS_proxy_connection_error

module PVS_proxy = struct
	module Server = struct
		type t = {
			uuid: string;
			addresses: Unix.inet_addr list;
			first_port: int;
			last_port: int;
		}
	end

	module Client = struct
		type t = {
			uuid: string;
			mac: string;
			interface: string;
			prepopulate: bool;
		}
	end

	type t = {
		site_uuid: string;
		site_name: string;
		servers: Server.t list;
		clients: Client.t list;
		vdi: string;
	}
	
	external configure_site : debug_info -> PVS_proxy.t -> unit = ""
	external remove_site : debug_info -> string -> unit = ""
end
