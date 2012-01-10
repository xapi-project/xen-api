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
open Stringext
open Fun
open Network_interface

let service_name = "network"

module D = Debug.Debugger(struct let name = service_name end)

let print_debug = ref false

let iproute2 = "/sbin/ip"
let resolv_conf = "/etc/resolv.conf"
let dhclient = "/sbin/dhclient"
let sysctl = "/sbin/sysctl"
let ovs_vsctl = "/usr/bin/ovs-vsctl"
let ovs_ofctl = "/usr/bin/ovs-ofctl"
let ovs_vlan_bug_workaround = "/usr/sbin/ovs-vlan-bug-workaround"
let brctl = "/usr/sbin/brctl"
let modprobe = "/sbin/modprobe"

let debug (fmt: ('a , unit, string, unit) format4) =
	let time_of_float x = 
		let time = Unix.gmtime x in
		Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
			(time.Unix.tm_year+1900)
			(time.Unix.tm_mon+1)
			time.Unix.tm_mday
			time.Unix.tm_hour
			time.Unix.tm_min
			time.Unix.tm_sec in
	if !print_debug 
	then Printf.kprintf
		(fun s -> 
			Printf.printf "%s %s\n" (time_of_float (Unix.gettimeofday ()))  s; 
			flush stdout) fmt
	else Printf.kprintf (fun s -> D.debug "%s" s) fmt

let call_script ?(log_successful_output=true) script args =
	try
		Unix.access script [ Unix.X_OK ];
		(* Use the same $PATH as xapi *)
		let env = [| "PATH=" ^ (Sys.getenv "PATH") |] in
		let output, _ = Forkhelpers.execute_command_get_output ~env script args in
		if log_successful_output then
			debug "%s %s succeeded [ output = '%s' ]" script (String.concat " " args) output;
		output
	with
	| Unix.Unix_error (e, a, b) ->
		debug "Caught unix error: %s [%s, %s]" (Unix.error_message e) a b;
		debug "Assuming script %s doesn't exist" script;
		raise (RpcFailure ("SCRIPT_DOES_NOT_EXIST", ["script", script; "args", String.concat " " args]))
	| Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n)->
		debug "%s %s exitted with code %d [stdout = '%s'; stderr = '%s']" script (String.concat " " args) n stdout stderr;
		raise (RpcFailure ("SCRIPT_ERROR", ["script", script; "args", String.concat " " args; "code",
			string_of_int n; "stdout", stdout; "stderr", stderr]))

module Sysfs = struct
	let list () =
		let all = Array.to_list (Sys.readdir "/sys/class/net") in
		List.filter (fun name -> Sys.is_directory ("/sys/class/net/" ^ name)) all

	let getpath dev attr =
		Printf.sprintf "/sys/class/net/%s/%s" dev attr

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
		with exn -> close_out outchan; raise exn

	let is_physical name =
		try
			let link = Unix.readlink (getpath name "device") in
			(* filter out device symlinks which look like /../../../devices/xen-backend/vif- *)
			not(List.mem "xen-backend" (String.split '/' link))
		with _ -> false

	let get_carrier name =
		let i = int_of_string (read_one_line (getpath name "carrier")) in
		match i with 1 -> true | 0 -> false | _ -> false

	let get_pcibuspath name =
		try
			let devpath = Unix.readlink (getpath name "device") in
			List.hd (List.rev (String.split '/' devpath))
		with exn -> "N/A"

	(** Returns the name of the driver for network device [dev] *)
	let get_driver_name dev =
		try
			let symlink = getpath dev "device/driver" in
			let target = Unix.readlink symlink in
			try
				let slash = String.index target '/' in
				Some (String.sub_to_end target (slash + 1))
			with Not_found ->
				debug "target %s of symbolic link %s does not contain slash" target symlink;
				None
		with _ ->
			debug "%s: could not read netdev's driver name" dev;
			None

	(** Returns the features bitmap for the driver for [dev].
	 *  The features bitmap is a set of NETIF_F_ flags supported by its driver. *)
	let get_features dev =
		try
			Some (int_of_string (read_one_line (getpath dev "features")))
		with _ ->
			None

	(** Returns [true] if [dev] supports VLAN acceleration, [false] otherwise. *)
	let has_vlan_accel dev =
		let flag_NETIF_F_HW_VLAN_TX = 128 in
		let flag_NETIF_F_HW_VLAN_RX = 256 in
		let flag_NETIF_F_VLAN = flag_NETIF_F_HW_VLAN_TX lor flag_NETIF_F_HW_VLAN_RX in
		match get_features dev with
		| None -> false
		| Some features -> (features land flag_NETIF_F_VLAN) <> 0

	let bridge_to_interfaces bridge =
		try
			Array.to_list (Sys.readdir (getpath bridge "brif"))
		with _ -> []

	(** {2 NIC Bonding} *)

	let bonding_masters = "/sys/class/net/bonding_masters"

	let load_bonding_driver () =
		debug "Loading bonding driver";
		try
			ignore (call_script modprobe ["bonding"]);
			(* is_bond_device() uses the contents of sysfs_bonding_masters to work out which devices
			 * have already been created. Unfortunately the driver creates "bond0" automatically at
			 * modprobe init. Get rid of this now or our accounting will go wrong. *)
			write_one_line bonding_masters "-bond0"
		with _ ->
			debug "Failed to load bonding driver"

	let bonding_driver_loaded () =
		try
			Unix.access bonding_masters [Unix.F_OK];
			true
		with _ ->
			false

	let is_bond_device name =
		try
			List.exists ((=) name) (String.split ' ' (read_one_line bonding_masters))
		with _ -> false

	(** Ensures that a bond master device exists in the kernel. *)
	let add_bond_master name =
		if not (bonding_driver_loaded ()) then
			load_bonding_driver ();
		if is_bond_device name then
			debug "Bond master %s already exists, not creating" name
		else begin
			debug "Adding bond master %s" name;
			try
				write_one_line bonding_masters ("+" ^ name)
			with _ ->
				debug "Failed to add bond master %s" name
		end

	(** No, Mr. Bond, I expect you to die. *)
	let remove_bond_master name =
		if is_bond_device name then begin
			let rec destroy retries =
				debug "Removing bond master %s (%d attempts remain)" name retries;
				try
					write_one_line bonding_masters ("-" ^ name)
				with _ ->
					if retries > 0 then
						(Thread.delay 0.5; destroy (retries - 1))
					else
						debug "Failed to remove bond master %s" name
			in
			destroy 10
		end else
			debug "Bond master %s does not exist; cannot destroy it" name

	let set_bond_properties master properties =
		let known_props = ["mode"; "updelay"; "downdelay"; "miimon"; "use_carrier"] in
		if is_bond_device master then begin
			let set_prop (prop, value) =
				try
					debug "Setting %s=%s on bond %s" prop value master;
					write_one_line (getpath master ("bonding/" ^ prop)) value
				with _ ->
					debug "Failed to set property \"%s\" on bond %s" prop master
			in
			List.iter set_prop (List.filter (fun (prop, _) -> List.mem prop known_props) properties)
		end else
			debug "Bond %s does not exist; cannot set properties" master

	let add_bond_slave master slave =
		if is_bond_device master then
			try
				debug "Adding slave %s to bond %s" slave master;
				write_one_line (getpath master "bonding/slaves") ("+" ^ slave)
			with _ ->
				debug "Failed to add slave %s to bond %s" slave master
		else
			debug "Bond %s does not exist; cannot add slave" master

	let remove_bond_slave master slave =
		if is_bond_device master then
			try
				debug "Remove slave %s from bond %s" slave master;
				write_one_line (getpath master "bonding/slaves") ("-" ^ slave)
			with _ ->
				debug "Failed to remove slave %s from bond %s" slave master
		else
			debug "Bond %s does not exist; cannot remove slave" master
end

module Ip = struct
	type ipversion = V4 | V6 | V46

	let string_of_version = function
		| V4 -> ["-4"]
		| V6 -> ["-6"]
		| V46 -> []

	let call args =
		call_script iproute2 args

	let find output attr =
		let args = String.split_f String.isspace output in
		let indices = (List.position (fun s -> s = attr) args) in
		List.map (fun i -> List.nth args (succ i)) indices

	let get_link_flags dev =
		let output = call ["link"; "show"; "dev"; dev] in
		let i = String.index output '<' in
		let j = String.index output '>' in
		let flags = String.sub output (i + 1) (j - i - 1) in
		String.split ',' flags

	let is_up dev =
		try
			List.mem "UP" (get_link_flags dev)
		with _ -> false

	let link_set dev args =
		ignore (call ("link" :: "set" :: dev :: args))

	let link_set_mtu dev mtu =
		ignore (link_set dev ["mtu"; string_of_int mtu])

	let link_set_up dev =
		ignore (link_set dev ["up"])

	let link_set_down dev =
		if is_up dev then
			ignore (link_set dev ["down"])

	let addr ?(version=V46) dev attr =
		let v = string_of_version version in
		let output = call (v @ ["addr"; "show"; "dev"; dev]) in
		find output attr

	let get_mtu dev =
		int_of_string (List.hd (addr dev "mtu"))

	let get_state dev =
		match addr dev "state" with
		| "UP" :: _ -> true
		| _ -> false

	let get_mac dev =
		List.hd (addr dev "link/ether")

	let set_mac dev mac =
		try
			ignore (link_set dev ["address"; mac])
		with _ -> ()

	let split_addr addr =
		try
			let i = String.index addr '/' in
			let ip = Unix.inet_addr_of_string (String.sub addr 0 i) in
			let prefixlen = int_of_string (String.sub_to_end addr (i + 1)) in
			Some (ip, prefixlen)
		with Not_found -> None

	let get_ipv4 dev =
		let addrs = addr dev "inet" in
		List.filter_map split_addr addrs

	let get_ipv6 dev =
		let addrs = addr dev "inet6" in
		List.filter_map split_addr addrs

	let set_ip_addr dev (ip, prefixlen) =
		let addr = Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ip) prefixlen in
		try
			ignore (call ["addr"; "add"; addr; "dev"; dev])
		with _ -> ()

	let flush_ip_addr ?(ipv6=false) dev =
		let ipv6' = if ipv6 then ["-6"] else ["-4"] in
		ignore (call(ipv6' @ ["addr"; "flush"; "dev"; dev]))

	let route_show ?(version=V46) dev =
		let v = string_of_version version in
		call (v @ ["route"; "show"; "dev"; dev])

	let set_gateway dev gateway =
		try
			ignore (call ["route"; "replace"; "default"; "via"; Unix.string_of_inet_addr gateway; "dev"; dev])
		with _ -> ()

	let vlan_name interface vlan =
		Printf.sprintf "%s.%d" interface vlan

	let create_vlan interface vlan =
		if not (List.mem (vlan_name interface vlan) (Sysfs.list ())) then
			ignore (call ["link"; "add"; "link"; interface; "name"; vlan_name interface vlan;
				"type"; "vlan"; "id"; string_of_int vlan])

	let destroy_vlan name =
		if List.mem name (Sysfs.list ()) then
			ignore (call ["link"; "delete"; name])
end

module Dhclient = struct
	let pid_file ?(ipv6=false) interface =
		let ipv6' = if ipv6 then "6" else "" in
		Printf.sprintf "/var/run/dhclient%s-%s.pid" ipv6' interface

	let lease_file ?(ipv6=false) interface =
		let ipv6' = if ipv6 then "6" else "" in
		Printf.sprintf "/var/xapi/dhclient%s-%s.leases" ipv6' interface

	let conf_file ?(ipv6=false) interface =
		let ipv6' = if ipv6 then "6" else "" in
		Printf.sprintf "/var/xapi/dhclient%s-%s.conf" ipv6' interface

	let write_conf_file ?(ipv6=false) interface options =
		let minimal = ["subnet-mask"; "broadcast-address"; "time-offset"; "domain-name"; "host-name"] in
		let set_gateway = if List.mem `set_gateway options then ["routers"] else [] in
		let set_dns = if List.mem `set_dns options then ["domain-name-servers"] else [] in
		let request = minimal @ set_gateway @ set_dns in
		let conf = Printf.sprintf "interface \"%s\" {\n  request %s;\n}\n" interface (String.concat ", " request) in
		Unixext.write_string_to_file (conf_file ~ipv6 interface) conf

	let start ?(ipv6=false) interface options =
		write_conf_file ~ipv6 interface options;
		let ipv6' = if ipv6 then ["-6"] else [] in
		call_script dhclient (ipv6' @ ["-q";
			"-pf"; pid_file ~ipv6 interface;
			"-lf"; lease_file ~ipv6 interface;
			"-cf"; conf_file ~ipv6 interface;
			interface])

	let stop ?(ipv6=false) interface =
		try
			ignore (call_script dhclient ["-r";
				"-pf"; pid_file ~ipv6 interface;
				interface]);
			Unix.unlink (pid_file ~ipv6 interface)
		with _ -> ()

	let is_running ?(ipv6=false) interface =
		try
			Unix.access (pid_file ~ipv6 interface) [Unix.F_OK];
			true
		with Unix.Unix_error _ ->
			false
end

module Sysctl = struct
	let write variable value =
		call_script sysctl ["-q"; "-w"; variable ^ "=" ^ value]

	let set_ipv6_autoconf interface value =
		let variable = "net.ipv6.conf." ^ interface ^ ".autoconf" in
		let value' = if value then "1" else "0" in
		write variable value'
end

module Ovs = struct
	let call args =
		call_script ovs_vsctl ("--timeout=20" :: args)

	let ofctl args =
		call_script ovs_ofctl args

	let handle_vlan_bug_workaround override bridge =
		(* This is a list of drivers that do support VLAN tx or rx acceleration, but
		 * to which the VLAN bug workaround should not be applied. This could be
		 * because these are known-good drivers (that is, they do not have any of
		 * the bugs that the workaround avoids) or because the VLAN bug workaround
		 * will not work for them and may cause other problems.
		 *
		 * This is a very short list because few drivers have been tested. *)
		let no_vlan_workaround_drivers = ["bonding"] in
		let phy_interfaces =
			try
				let interfaces = String.split '\n' (String.rtrim (call ["list-ifaces"; bridge])) in
				List.filter Sysfs.is_physical interfaces
			with _ -> []
		in
		List.iter (fun interface ->
			let do_workaround =
				match override with
				| Some value -> value
				| None ->
					match Sysfs.get_driver_name interface with
					| None ->
						Sysfs.has_vlan_accel interface
					| Some driver ->
						if List.mem driver no_vlan_workaround_drivers then
							false
						else
							Sysfs.has_vlan_accel interface
			in
			let setting = if do_workaround then "on" else "off" in
			(try
				ignore (call_script ovs_vlan_bug_workaround [interface; setting]);
			with _ -> ());
		) phy_interfaces

	let create_bridge ?mac ~fail_mode vlan vlan_bug_workaround name =
		let vlan_arg = match vlan with
			| None -> []
			| Some (parent, tag) ->
				handle_vlan_bug_workaround vlan_bug_workaround parent;
				[parent; string_of_int tag]
		in
		let mac_arg = match mac with
			| None -> []
			| Some mac -> ["--"; "set"; "bridge"; name; Printf.sprintf "other-config:hwaddr=\"%s\"" (String.escaped mac)]
		in
		let fail_mode_arg =
			if vlan = None then ["--"; "set"; "bridge"; name; "fail_mode=" ^ fail_mode] else [] in
		call (["--"; "--may-exist"; "add-br"; name] @ vlan_arg @ mac_arg @ fail_mode_arg)

	let destroy_bridge name =
		call ["--"; "--if-exists"; "del-br"; name]

	let list_bridges () =
		String.split '\n' (call ["list-br"])

	let get_vlans name =
		try
			let vlans_with_uuid =
				let raw = call ["--bare"; "-f"; "table"; "--"; "--columns=name,_uuid"; "find"; "port"; "fake_bridge=true"] in
				let lines = String.split '\n' (String.rtrim raw) in
				List.map (fun line -> Scanf.sscanf line "%s %s" (fun a b-> a, b)) lines
			in
			let bridge_ports =
				let raw = call ["get"; "bridge"; name; "ports"] in
				let raw = String.rtrim raw in
				if raw <> "[]" then
					let raw_list = (String.split ',' (String.sub raw 1 (String.length raw - 2))) in
					List.map (String.strip String.isspace) raw_list
				else
					[]
			in
			let vlans_on_bridge = List.filter (fun (_, br) -> List.mem br bridge_ports) vlans_with_uuid in
			List.map (fun (n, _) -> n) vlans_on_bridge
		with _ -> []

	let bridge_to_ports name =
		try
			String.split '\n' (String.rtrim (call ["list-ports"; name]))
		with _ -> []

	let bridge_to_interfaces name =
		try
			String.split '\n' (String.rtrim (call ["list-ifaces"; name]))
		with _ -> []

	let bridge_to_vlan name =
		call ["br-to-parent"; name],
		int_of_string (call ["br-to-vlan"; name])

	let create_port name bridge =
		call ["--"; "--may-exist"; "add-port"; bridge; name]

	let create_bond name interfaces bridge mac =
		call (["--"; "--fake-iface"; "--may-exist"; "add-bond"; bridge; name] @ interfaces @
			["--"; "set"; "port"; name; "MAC=\"" ^ (String.escaped mac) ^ "\""])

	let destroy_port name =
		call ["--"; "--with-iface"; "--if-exists"; "del-port"; name]

	let port_to_bridge name =
		call ["port-to-br"; name]

	let set_bond_properties name properties =
		let known_props = ["mode"; "hashing-algorithm"; "updelay"; "downdelay"; "miimon"; "use_carrier"] in
		let mode_args =
			let mode = if List.mem_assoc "mode" properties then List.assoc "mode" properties else "balance-slb" in
			let halgo = if List.mem_assoc "hashing-algorithm" properties then List.assoc "hashing-algorithm" properties else "" in
			if mode = "lacp" then "lacp=active" ::
				(if halgo = "src_mac" then ["bond_mode=balance-slb"]
				else if halgo = "tcpudp_ports" then ["bond_mode=balance-tcp"]
				else begin
					debug "bond %s has invalid bond-hashing-algorithm '%s'" name halgo;
					["bond_mode=balance-slb"]
				end)
			else
				["lacp=off"; "bond_mode=" ^ mode]
		in
		let get_prop (prop, ovs_key) =
			if List.mem_assoc prop properties then
				let value = int_of_string (List.assoc prop properties) in
				if value < 0 then begin
					debug "bond %s has invalid %s '%d'" name prop value;
					[]
				end else if prop = "use_carrier" then
					[ovs_key ^ "=" ^ (if value > 0 then "carrier" else "miimon")]
				else
					[ovs_key ^ "=" ^ (string_of_int value)]
			else
				[]
		in
		let extra_args = List.flatten (List.map get_prop ["updelay", "bond_updelay"; "downdelay", "bond_downdelay";
			"miimon", "other-config:bond-miimon-interval"; "use_carrier", "other-config:bond-detect-mode"]) in
		let other_args = List.filter_map (fun (k, v) ->
			if List.mem k known_props then None
			else Some (Printf.sprintf "other-config:\"%s\"=\"%s\"" (String.escaped ("bond-" ^ k)) (String.escaped v))
		) properties in
		let args = ["--"; "set"; "port"; name] @ mode_args @ extra_args @ other_args in
		call args

	let get_fail_mode bridge =
		call ["get-fail-mode"; bridge]

	let add_default_flows bridge mac interfaces =
		let ports = List.map (fun interface -> call ["get"; "interface"; interface; "ofport"]) interfaces in
		let flows = match ports with
			| [port] ->
				[Printf.sprintf "idle_timeout=0,priority=0,in_port=%s,arp,nw_proto=1,actions=local" port;
				Printf.sprintf "idle_timeout=0,priority=0,in_port=local,arp,dl_src=%s,actions=%s" mac port;
				Printf.sprintf "idle_timeout=0,priority=0,in_port=%s,dl_dst=%s,actions=local" port mac;
				Printf.sprintf "idle_timeout=0,priority=0,in_port=local,dl_src=%s,actions=%s" mac port]
			| ports ->
				List.flatten (List.map (fun port ->
					[Printf.sprintf "idle_timeout=0,priority=0,in_port=local,arp,dl_src=%s,actions=NORMAL" mac;
					Printf.sprintf "idle_timeout=0,priority=0,in_port=local,dl_src=%s,actions=NORMAL" mac;
					Printf.sprintf "idle_timeout=0,priority=0,in_port=%s,arp,nw_proto=1,actions=local" port;
					Printf.sprintf "idle_timeout=0,priority=0,in_port=%s,dl_dst=%s,actions=local" port mac]
				) ports)
		in
		List.iter (fun flow -> ignore (ofctl ["add-flow"; bridge; flow])) flows
end

module Brctl = struct
	let call args =
		call_script brctl args

	let create_bridge name =
		if not (List.mem name (Sysfs.list ())) then
			ignore (call ["addbr"; name])

	let destroy_bridge name =
		if List.mem name (Sysfs.list ()) then
			ignore (call ["delbr"; name])

	let create_port bridge name =
		if not (List.mem name (Sysfs.bridge_to_interfaces bridge)) then
			ignore (call ["addif"; bridge; name])

	let destroy_port bridge name =
		if List.mem name (Sysfs.bridge_to_interfaces bridge) then
			ignore (call ["delif"; bridge; name])

end

