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

open Xapi_stdext_std.Xstringext
open Xapi_stdext_unix
open Forkhelpers

type kind = Bridge | Vswitch

type network_ops = { 
  kind: kind;
  add: string -> ?uuid:string -> unit;
  del: string -> unit;
  list: unit -> string list;

  exists: string -> bool;

  intf_add: string -> string -> unit;
  intf_del: string -> string -> unit;
  intf_list: string -> string list;

  get_bridge: string -> string;
  is_on_bridge: string -> bool;

  set_forward_delay: string -> int -> unit;
}

exception Unknown_network_backend of string
exception Invalid_network_backend_operation of string * kind

let string_of_kind kind = match kind with
  | Bridge -> "bridge"
  | Vswitch -> "openvswitch"

let kind_of_string s = match s with
  | "bridge" -> Bridge
  | "vswitch" -> Vswitch
  | "openvswitch" -> Vswitch
  | _ -> raise (Unknown_network_backend s)

module Internal = struct

let control_socket () =
	try
		Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0
	with
	exn ->
		try
			Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0
		with
		exn ->
			Unix.socket Unix.PF_INET6 Unix.SOCK_DGRAM 0

let with_fd f =
	let fd = control_socket () in
	let r = begin try
		f fd
	with
	exn ->
		Unix.close fd;
		raise exn
	end in
	Unix.close fd;
	r

let exec cmd =
	let ret = Sys.command cmd in
	if ret <> 0 then
		failwith (Printf.sprintf "cmd returned %d" ret)

let read_one_line file =
	let inchan = open_in file in
	try
		let result = input_line inchan in
		close_in inchan;
		result
	with exn -> close_in inchan; raise exn

let write_one_line file l =
	let outchan = open_out file in
	try
		output_string outchan (l ^ "\n");
		close_out outchan
	with
		exn -> close_out outchan; raise exn
end

module Bridge = struct

external _add : Unix.file_descr -> string -> unit = "stub_bridge_add"
external _del : Unix.file_descr -> string -> unit = "stub_bridge_del"

let add name ?uuid = 
	Internal.with_fd (fun fd -> _add fd name)

let del name =
	Internal.with_fd (fun fd -> _del fd name)

let list () =
	let dirs = Array.to_list (Sys.readdir "/sys/class/net") in
	List.filter (fun dir ->
		Sys.file_exists ("/sys/class/net/" ^ dir ^ "/bridge")) dirs

let exists name =
	try Sys.file_exists ("/sys/class/net/" ^ name ^ "/bridge")
	with _ -> false

let set name obj v =
	let file = "/sys/class/net/" ^ name ^ "/bridge/" ^ obj in
	let outchan = open_out file in
	output_string outchan v;
	output_char outchan '\n';
	close_out outchan

let get name obj = Internal.read_one_line ("/sys/class/net/" ^ name ^ "/bridge/" ^ obj) 
	  
let _forward_delay = "forward_delay"
let _hello_time = "hello_time"
let _max_age = "max_age"
let _ageing_time = "ageing_time"
let _stp_state = "stp_state"
let _priority = "priority"
let _bridge_id = "bridge_id"

let get_id name = 
	get name _bridge_id

let set_forward_delay name v =
	set name _forward_delay (string_of_int v)

let get_forward_delay name =
	int_of_string (get name _forward_delay)

let set_hello_time name v =
	set name _hello_time (string_of_int v)

let get_hello_time name =
	int_of_string (get name _hello_time)

let set_max_age name v =
	set name _max_age (string_of_int v)

let get_max_age name = 
	int_of_string (get name _max_age)

let set_ageing_time name v =
	set name _ageing_time (string_of_int v)

let get_ageing_time name = 
	int_of_string (get name _ageing_time)

let set_stp_state name v =
	set name _stp_state (if v then "1" else "0")

let get_stp_state name = 
	get name _stp_state <> "0"

let set_priority name v =
	set name _priority (string_of_int v)

let get_priority name = 
	int_of_string (get name _priority)

(* bridge interfaces control function *)
external _intf_add : Unix.file_descr -> string -> string -> unit
                   = "stub_bridge_intf_add"
external _intf_del : Unix.file_descr -> string -> string -> unit
                   = "stub_bridge_intf_del"

let intf_add name intf =
	Internal.with_fd (fun fd -> _intf_add fd name intf)

let intf_del name intf =
	Internal.with_fd (fun fd -> _intf_del fd name intf)

let intf_list name =
	Array.to_list (Sys.readdir ("/sys/class/net/" ^ name ^ "/brif/"))

let getpath dev attr = Printf.sprintf "/sys/class/net/%s/%s" dev attr

let is_on_bridge name = try Unix.access (getpath name "brport") [ Unix.F_OK ]; true with _ -> false

let get_bridge name = Filename.basename (Unix.readlink ((getpath name "brport") ^ "/bridge"))

let ops = {
  kind = Bridge;

  add = add;
  del = del;
  list = list;

  exists = exists;

  intf_add = intf_add;
  intf_del = intf_del;
  intf_list = intf_list;

  get_bridge = get_bridge;
  is_on_bridge = is_on_bridge;

  set_forward_delay = set_forward_delay;
}

end

module Vswitch = struct

let vsctl_script = "/usr/bin/ovs-vsctl"

let vsctl args =
  Unix.access vsctl_script [ Unix.X_OK ];
  let output, _ = Forkhelpers.execute_command_get_output vsctl_script args in
  let stripped = String.strip (fun c -> c='\n') output in
  match stripped with
    | "" -> []
    | s -> String.split '\n' s

let add name ?uuid = 
  let extra = match uuid with
    | Some uuid' -> ["--"; "br-set-external-id"; name; "xs-network-uuids"; uuid']
    | None -> ["--"; "foo"] in
  ignore(vsctl (["add-br" ; name] @ extra))
let del name = ignore(vsctl ["del-br" ; name])
let list () = vsctl [ "list-br" ]

let exists name = List.exists (fun x -> x = name) (list ())

let intf_add name intf = ignore(vsctl ["add-port"; name; intf])
let intf_del name intf = ignore(vsctl ["del-port"; name; intf])
let intf_list name = vsctl [ "list-ports"; name ]

let get_bridge name = 
  match vsctl [ "port-to-br"; name ] with
  | l::[] -> l
  | [] -> failwith ("ovs-vsctl port-to-br: did not return a bridge for port " ^ name)
  | _ -> failwith ("ovs-vsctl port-to-br: returned an unexpected number of results for port " ^ name)

let is_on_bridge name = 
  match vsctl [ "port-to-br"; name ] with
  | l::[] -> true
  | [] -> false
  | _ -> failwith ("ovs-vsctl port-to-br: returned an unexpected number of results for port " ^ name)

let ops = {
  kind = Vswitch;

  add = add;
  del = del;
  list = list;

  exists = exists;

  intf_add = intf_add;
  intf_del = intf_del;
  intf_list = intf_list;

  get_bridge = get_bridge;
  is_on_bridge = is_on_bridge;

  set_forward_delay = fun name v -> raise (Invalid_network_backend_operation ("set_forward_delay", Vswitch))
}

end

module Link = struct

type speed = int (* see CA-24610 *)
type duplex = Duplex_unknown | Duplex_half | Duplex_full

let string_of_duplex = function
	| Duplex_unknown -> "unknown"
	| Duplex_half    -> "half"
	| Duplex_full    -> "full"

let duplex_of_string = function
	| "full"    -> Duplex_full
	| "half"    -> Duplex_half
	| _         -> Duplex_unknown

let int_of_speed x = x
let speed_of_int x = x
let speed_unknown = 0

external _up : Unix.file_descr -> string -> unit = "stub_link_up"
external _is_up : Unix.file_descr -> string -> bool = "stub_link_is_up"
external _down : Unix.file_descr -> string -> unit = "stub_link_down"
external _multicast : Unix.file_descr -> string -> bool -> unit = "stub_link_multicast"
external _arp : Unix.file_descr -> string -> bool -> unit = "stub_link_arp"
external _change_name : Unix.file_descr -> string -> string -> unit = "stub_link_change_name"
external _get_status : Unix.file_descr -> string -> speed * duplex = "stub_link_get_status"

let up name =
	Internal.with_fd (fun fd -> _up fd name)

let is_up name =
	Internal.with_fd (fun fd -> try _is_up fd name with _ -> false)

let down name =
	Internal.with_fd (fun fd -> _down fd name)

let multicast name v =
	Internal.with_fd (fun fd -> _multicast fd name v)

let arp name v =
	Internal.with_fd (fun fd -> _arp fd name v)

let change_name name newname =
	Internal.with_fd (fun fd -> _change_name fd name newname)

let set_addr name addr =
	(* temporary *)
	Internal.exec (Printf.sprintf "ip link set %s addr %s" name addr)

let get_status name =
	Internal.with_fd (fun fd -> _get_status fd name)

end

module Addr = struct

let flush name =
	Internal.exec (Printf.sprintf "ip addr flush %s" name)

external __get_all : unit -> (string * string * string * bool) list = "stub_if_getaddr"

type addr = IPV4 of string * string | IPV6 of string * string

let get_all () =
	List.map (fun (name, addr, netmask, inet6) -> name, if inet6 then IPV6 (addr,netmask) else IPV4 (addr,netmask))
	         (__get_all ())

let get_all_ipv4 () =
	let ipv4s = List.filter (fun (_, _, _, inet6) -> not inet6) (__get_all ()) in
	List.map (fun (name, addr, netmask, _) ->
		name, Unix.inet_addr_of_string addr, Unix.inet_addr_of_string netmask
		) ipv4s

let get name =
	List.map (fun (a,b,c) -> (b,c)) (List.filter (fun (dev, _, _) -> dev = name) (get_all_ipv4 ()))

end

let list () =
	Array.to_list (Sys.readdir "/sys/class/net")

let getpath dev attr = Printf.sprintf "/sys/class/net/%s/%s" dev attr

let get_address name =
	try
		let master_path = Unix.readlink (getpath name "master") in
		let master = List.hd (List.rev (String.split '/' master_path)) in
		let proc ac line =
			try
				let a = String.index line ':' in
				let k = String.sub line 0 a in
				let v = String.sub_to_end line (a + 2) in
				if k = "Slave Interface" && v = name then
					Some ""
				else if ac = Some "" && k = "Permanent HW addr" then
					Some v
				else
					ac
			with _ -> ac
		in
		match Unixext.file_lines_fold proc None ("/proc/net/bonding/" ^ master) with
		| None -> raise Not_found
		| Some address -> address
	with _ -> 
		Internal.read_one_line (getpath name "address")

let get_mtu name = Internal.read_one_line (getpath name "mtu")
let set_mtu name mtu =
	Internal.write_one_line (getpath name "mtu")
	                        (string_of_int mtu)

let get_by_address address = 
  List.filter
    (fun device ->
       (* CA-21402: Not everything returned by list() is guaranteed to be a directory containing an address;
	  so we have to make sure we catch exceptions here so we keep trying the next one and so on.. *)
       try String.lowercase_ascii (get_address device) = String.lowercase_ascii address with _ -> false)
    (list ()) 
  
let get_pcibuspath name =
	try
		let devpath = Unix.readlink (getpath name "device") in
		List.hd (List.rev (String.split '/' devpath))
	with exn -> "N/A"

let get_carrier name =
	let i = int_of_string (Internal.read_one_line (getpath name "carrier")) in
	match i with 1 -> true | 0 -> false | _ -> false

let get_ids name =
	let read_id_from path =
		try
			let l = Internal.read_one_line path in
			(* trim 0x *)
			String.sub l 2 (String.length l - 2)
		with _ -> ""
		in
	read_id_from (getpath name "device/vendor"),
	read_id_from (getpath name "device/device")

let is_physical name = 
  try 
	let link = Unix.readlink (getpath name "device") in
	(* filter out device symlinks which look like /../../../devices/xen-backend/vif- *)
	not(List.mem "xen-backend" (String.split '/' link))
  with _ -> false

let get_bios_name name =
	try
		let output, _ = Forkhelpers.execute_command_get_output "/sbin/biosdevname" ["-i"; name] in
		let output = String.strip String.isspace output in
		output
	with _ -> name

(* Dispatch network backend operations. *)

let network_config_file = "/etc/xensource/network.conf"
let network_backend = 
  try 
    kind_of_string (String.strip String.isspace (Unixext.string_of_file network_config_file))
  with
  | Unix.Unix_error(Unix.ENOENT, "open", _) -> Bridge
  | Unix.Unix_error(err, op, path) -> failwith (Printf.sprintf "Unix error: %s (%s,%s)\n" (Unix.error_message err) op path)

let network = match network_backend with
  | Bridge -> Bridge.ops
  | Vswitch -> Vswitch.ops
