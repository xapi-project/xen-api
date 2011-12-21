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

module Unix = struct
	include Unix
	let inet_addr_of_rpc rpc = Unix.inet_addr_of_string (Rpc.string_of_rpc rpc)
	let rpc_of_inet_addr inet = Rpc.rpc_of_string (Unix.string_of_inet_addr inet)
end

type dhcp_options = [`set_gateway | `set_dns]
type ipv4 = None4 | DHCP4 of dhcp_options list | Static4 of (Unix.inet_addr * int) list
type ipv6 = None6 | DHCP6 of dhcp_options list | Autoconf6 | Static6 of (Unix.inet_addr * int) list

external reopen_logs: unit -> bool = ""

module Interface = struct
	external get_all: unit -> string list = ""
	external get_mac: string -> string = ""
	external is_up: string -> bool = ""
	external get_ipv4_addr: string -> (Unix.inet_addr * int) list = ""
	external set_ipv4_addr: string -> ipv4 -> unit = ""
	external get_ipv4_gateway: string -> Unix.inet_addr option = ""
	external set_ipv4_gateway: string -> Unix.inet_addr -> unit = ""
	external get_ipv6_addr: string -> (Unix.inet_addr * int) list = ""
	external set_ipv6_addr: string -> ipv6 -> unit = ""
	external get_ipv6_gateway: string -> Unix.inet_addr option = ""
	external set_ipv6_gateway: string -> Unix.inet_addr -> unit = ""
	external get_dns: string -> Unix.inet_addr list = ""
	external set_dns: string -> Unix.inet_addr list -> unit = ""
	external get_mtu: string -> int = ""
	external set_mtu: string -> int -> unit = ""
	external is_connected: string -> bool = ""
	external is_physical: string -> bool = ""
	external bring_up: string -> unit = ""
	external bring_down: string -> unit = ""
	external is_persistent: string -> bool = ""
	external set_persistent: string -> bool -> unit = ""
end

type kind = Openvswitch | Bridge
type bond_mode = Balance_slb | Active_backup | Lacp
type fail_mode = Standalone | Secure

module Bridge = struct
	external get_all: unit -> string list = ""
	external create: ?vlan:(string * int) -> ?vlan_bug_workaround:bool ->
		?mac:string -> ?fail_mode:fail_mode -> string -> unit = ""
	external destroy: ?force:bool -> string -> unit = ""
	external get_kind: string -> kind = ""
	external get_ports: string -> string list = ""
	external is_persistent: string -> bool = ""
	external set_persistent: string -> bool -> unit = ""
	external get_vlan: string -> (string * int) option = ""
	external add_port: ?mac:string -> string -> string -> string list -> unit = ""
	external remove_port: string -> string -> unit = ""
	external get_interfaces: string -> string list = ""
	external get_bond_properties: string -> string -> (string * string) list = ""
	external set_bond_properties: string -> string -> (string * string) list -> unit = ""
	external get_fail_mode: string -> fail_mode option = ""
end
