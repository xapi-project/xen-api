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

module D = Debug.Debugger(struct let name = "network_utils" end)
open D

let iproute2 = "/sbin/ip"
let resolv_conf = "/etc/resolv.conf"
let dhclient = "/sbin/dhclient"
let sysctl = "/sbin/sysctl"
let ovs_vsctl = "/usr/bin/ovs-vsctl"
let ovs_ofctl = "/usr/bin/ovs-ofctl"
let ovs_appctl = "/usr/bin/ovs-appctl"
let ovs_vlan_bug_workaround = "/usr/sbin/ovs-vlan-bug-workaround"
let brctl = match Fhs.distroty with | Fhs.Debianlike -> "/sbin/brctl" | Fhs.Centoslike -> "/usr/sbin/brctl"
let modprobe = "/sbin/modprobe"
let ethtool = "/sbin/ethtool"
let bonding_dir = "/proc/net/bonding/"
let dhcp6c = "/sbin/dhcp6c"

let call_script ?(log_successful_output=false) script args =
	try
		Unix.access script [ Unix.X_OK ];
		(* Use the same $PATH as xapi *)
		let env = [| "PATH=" ^ (Sys.getenv "PATH") |] in
		let output, _ = Forkhelpers.execute_command_get_output ~env script args in
		if log_successful_output then
			debug "Call '%s %s' succeeded [output = '%s']" script (String.concat " " args) output;
		output
	with
	| Unix.Unix_error (e, a, b) ->
		error "Caught unix error: %s [%s, %s]" (Unix.error_message e) a b;
		error "Assuming script %s doesn't exist" script;
		raise (Script_missing script)
	| Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n)->
		error "Call '%s %s' exited with code %d [stdout = '%s'; stderr = '%s']" script
			(String.concat " " args) n stdout stderr;
		raise (Script_error ["script", script; "args", String.concat " " args; "code",
			string_of_int n; "stdout", stdout; "stderr", stderr])

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
		with exn -> close_in inchan; raise (Read_error file)

	let write_one_line file l =
		let outchan = open_out file in
		try
			output_string outchan (l ^ "\n");
			close_out outchan
		with exn -> close_out outchan; raise (Write_error file)

	let is_physical name =
		try
			let link = Unix.readlink (getpath name "device") in
			(* filter out device symlinks which look like /../../../devices/xen-backend/vif- *)
			not(List.mem "xen-backend" (String.split '/' link))
		with _ -> false

	let get_carrier name =
		try
			let i = int_of_string (read_one_line (getpath name "carrier")) in
			match i with 1 -> true | 0 -> false | _ -> false
		with _ -> false

	let get_pcibuspath name =
		try
			let devpath = Unix.readlink (getpath name "device") in
			List.hd (List.rev (String.split '/' devpath))
		with exn -> "N/A"

	let get_pci_ids name =
		let read_id_from path =
			try
				let l = read_one_line path in
				(* trim 0x *)
				String.sub l 2 (String.length l - 2)
			with _ -> ""
			in
		read_id_from (getpath name "device/vendor"),
		read_id_from (getpath name "device/device")

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

	let get_all_bridges () =
		let ifaces = list () in
		List.filter (fun name -> Sys.file_exists (getpath name "bridge")) ifaces
end

module Ip = struct
	type ipversion = V4 | V6 | V46

	let string_of_version = function
		| V4 -> ["-4"]
		| V6 -> ["-6"]
		| V46 -> []

	let call ?(log=false) args =
		call_script ~log_successful_output:log iproute2 args

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
		ignore (call ~log:true ("link" :: "set" :: dev :: args))

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
		let broadcast =
			(* Set the broadcast address when adding an IPv4 address *)
			if String.contains addr '.' then
				["broadcast"; "+"]
			else []
		in
		try
			ignore (call ~log:true (["addr"; "add"; addr; "dev"; dev] @ broadcast))
		with _ -> ()

	let flush_ip_addr ?(ipv6=false) dev =
		try
			if ipv6 then begin
				ignore (call ~log:true ["-6"; "addr"; "flush"; "dev"; dev; "scope"; "global"]);
				ignore (call ~log:true ["-6"; "addr"; "flush"; "dev"; dev; "scope"; "site"])
			end else
				ignore (call ~log:true ["-4"; "addr"; "flush"; "dev"; dev])
		with _ -> ()

	let route_show ?(version=V46) dev =
		let v = string_of_version version in
		call (v @ ["route"; "show"; "dev"; dev])

	let set_route ?network dev gateway =
		try
			match network with
			| None ->
				ignore (call ~log:true ["route"; "replace"; "default"; "via"; Unix.string_of_inet_addr gateway; "dev"; dev])
			| Some (ip, prefixlen) ->
				let addr = Printf.sprintf "%s/%d" (Unix.string_of_inet_addr ip) prefixlen in
				ignore (call ~log:true ["route"; "replace"; addr; "via"; Unix.string_of_inet_addr gateway; "dev"; dev])
		with _ -> ()

	let set_gateway dev gateway = set_route dev gateway

	let vlan_name interface vlan =
		Printf.sprintf "%s.%d" interface vlan

	let create_vlan interface vlan =
		if not (List.mem (vlan_name interface vlan) (Sysfs.list ())) then
			ignore (call ~log:true ["link"; "add"; "link"; interface; "name"; vlan_name interface vlan;
				"type"; "vlan"; "id"; string_of_int vlan])

	let destroy_vlan name =
		if List.mem name (Sysfs.list ()) then
			ignore (call ~log:true ["link"; "delete"; name])
end

module Linux_bonding = struct
	let bonding_masters = "/sys/class/net/bonding_masters"

	let load_bonding_driver () =
		debug "Loading bonding driver";
		try
			ignore (call_script modprobe ["bonding"]);
			(* is_bond_device() uses the contents of sysfs_bonding_masters to work out which devices
			 * have already been created. Unfortunately the driver creates "bond0" automatically at
			 * modprobe init. Get rid of this now or our accounting will go wrong. *)
			Sysfs.write_one_line bonding_masters "-bond0"
		with _ ->
			error "Failed to load bonding driver"

	let bonding_driver_loaded () =
		try
			Unix.access bonding_masters [Unix.F_OK];
			true
		with _ ->
			false

	let is_bond_device name =
		try
			List.exists ((=) name) (String.split ' ' (Sysfs.read_one_line bonding_masters))
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
				Sysfs.write_one_line bonding_masters ("+" ^ name)
			with _ ->
				error "Failed to add bond master %s" name
		end

	(** No, Mr. Bond, I expect you to die. *)
	let remove_bond_master name =
		if is_bond_device name then begin
			let rec destroy retries =
				debug "Removing bond master %s (%d attempts remain)" name retries;
				try
					Sysfs.write_one_line bonding_masters ("-" ^ name)
				with _ ->
					if retries > 0 then
						(Thread.delay 0.5; destroy (retries - 1))
					else
						error "Failed to remove bond master %s" name
			in
			destroy 10
		end else
			error "Bond master %s does not exist; cannot destroy it" name

	let known_props = ["mode"; "updelay"; "downdelay"; "miimon"; "use_carrier"]

	let get_bond_properties master =
		if is_bond_device master then begin
			let get_prop prop =
				try
					Some (prop, Sysfs.read_one_line (Sysfs.getpath master ("bonding/" ^ prop)))
				with _ ->
					debug "Failed to get property \"%s\" on bond %s" prop master;
					None
			in
			List.filter_map get_prop known_props
		end else begin
			debug "Bond %s does not exist; cannot get properties" master;
			[]
		end

	let set_bond_properties master properties =
		if is_bond_device master then begin
			let current_props = get_bond_properties master in
			(* Find out which properties are known, but different from the current state,
			 * and only continue if there is at least one of those. *)
			let props_to_update = List.filter (fun (prop, value) ->
				not (List.mem (prop, value) current_props) && List.mem prop known_props) properties in
			if props_to_update <> [] then
				let set_prop (prop, value) =
					try
						debug "Setting %s=%s on bond %s" prop value master;
						Sysfs.write_one_line (Sysfs.getpath master ("bonding/" ^ prop)) value
					with _ ->
						error "Failed to set property \"%s\" on bond %s" prop master
				in
				Ip.link_set_down master;
				List.iter set_prop props_to_update;
				Ip.link_set_up master
		end else
			error "Bond %s does not exist; cannot set properties" master

	let add_bond_slave master slave =
		if is_bond_device master then
			try
				debug "Adding slave %s to bond %s" slave master;
				Sysfs.write_one_line (Sysfs.getpath master "bonding/slaves") ("+" ^ slave)
			with _ ->
				error "Failed to add slave %s to bond %s" slave master
		else
			error "Bond %s does not exist; cannot add slave" master

	let remove_bond_slave master slave =
		if is_bond_device master then
			try
				debug "Remove slave %s from bond %s" slave master;
				Sysfs.write_one_line (Sysfs.getpath master "bonding/slaves") ("-" ^ slave)
			with _ ->
				error "Failed to remove slave %s from bond %s" slave master
		else
			error "Bond %s does not exist; cannot remove slave" master
end

module Dhclient = struct
	let pid_file ?(ipv6=false) interface =
		let ipv6' = if ipv6 then "6" else "" in
		Printf.sprintf "/var/run/dhclient%s-%s.pid" ipv6' interface

	let lease_file ?(ipv6=false) interface =
		let ipv6' = if ipv6 then "6" else "" in
		Filename.concat Fhs.vardir (Printf.sprintf "dhclient%s-%s.leases" ipv6' interface)

	let conf_file ?(ipv6=false) interface =
		let ipv6' = if ipv6 then "6" else "" in
		Filename.concat Fhs.vardir (Printf.sprintf "dhclient%s-%s.conf" ipv6' interface)

	let generate_conf ?(ipv6=false) interface options =
		let minimal = ["subnet-mask"; "broadcast-address"; "time-offset"; "host-name"; "nis-domain";
			"nis-servers"; "ntp-servers"; "interface-mtu"] in
		let set_gateway = if List.mem `set_gateway options then ["routers"] else [] in
		let set_dns = if List.mem `set_dns options then ["domain-name"; "domain-name-servers"] else [] in
		let request = minimal @ set_gateway @ set_dns in
		Printf.sprintf "interface \"%s\" {\n  request %s;\n}\n" interface (String.concat ", " request)

	let read_conf_file ?(ipv6=false) interface =
		let file = conf_file ~ipv6 interface in
		try Some (Unixext.string_of_file file) with _ -> None

	let write_conf_file ?(ipv6=false) interface options =
		let conf = generate_conf ~ipv6 interface options in
		Unixext.write_string_to_file (conf_file ~ipv6 interface) conf

	let start ?(ipv6=false) interface options =
		write_conf_file ~ipv6 interface options;
		let ipv6' = if ipv6 then ["-6"] else [] in
		call_script ~log_successful_output:true dhclient (ipv6' @ ["-q";
			"-pf"; pid_file ~ipv6 interface;
			"-lf"; lease_file ~ipv6 interface;
			"-cf"; conf_file ~ipv6 interface;
			interface])

	let stop ?(ipv6=false) interface =
		try
			ignore (call_script ~log_successful_output:true dhclient ["-r";
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

	let ensure_running ?(ipv6=false) interface options =
		if not(is_running ~ipv6 interface) then
			(* dhclient is not running, so we need to start it. *)
			ignore (start ~ipv6 interface options)
		else begin
			(* dhclient is running - if the config has changed, update the config file and restart. *)
			let current_conf = read_conf_file ~ipv6 interface in
			let new_conf = generate_conf ~ipv6 interface options in
			if current_conf <> (Some new_conf) then begin
				ignore (stop ~ipv6 interface);
				ignore (start ~ipv6 interface options)
			end
		end
end

module Sysctl = struct
	let write value variable =
		ignore (call_script ~log_successful_output:true sysctl ["-q"; "-w"; variable ^ "=" ^ value])

	let set_ipv6_autoconf interface value =
		try
			let variables = [
				"net.ipv6.conf." ^ interface ^ ".autoconf";
				"net.ipv6.conf." ^ interface ^ ".accept_ra"
			] in
			let value' = if value then "1" else "0" in
			List.iter (write value') variables
		with
		| e when value = true -> raise e
		| _ -> ()
end

module Proc = struct
	let get_bond_links_up name = 
		try
			let raw = Unixext.string_of_file (bonding_dir ^ name) in
			let lines = String.split '\n' raw in
			let check_lines lines =
			let rec loop acc = function
				| [] -> acc
				| line1 :: line2 :: tail ->
					if (String.startswith "Slave Interface:" line1)
						&& (String.startswith "MII Status:" line2)
						&& (String.endswith "up" line2)
					then
						loop (acc + 1) tail
					else
						loop acc (line2 :: tail)
			  | _ :: [] -> acc in
			loop 0 lines in
			check_lines lines
		with e ->
			error "Error: could not read %s." (bonding_dir ^ name);
			0

end

module Ovs = struct
	let vsctl ?(log=false) args =
		call_script ~log_successful_output:log ovs_vsctl ("--timeout=20" :: args)

	let ofctl ?(log=false) args =
		call_script ~log_successful_output:log ovs_ofctl args

	let appctl ?(log=false) args =
		call_script ~log_successful_output:log ovs_appctl args

	let port_to_interfaces name =
		try
			let raw = vsctl ["get"; "port"; name; "interfaces"] in
			let raw = String.rtrim raw in
			if raw <> "[]" then
				let raw_list = (String.split ',' (String.sub raw 1 (String.length raw - 2))) in
				let uuids = List.map (String.strip String.isspace) raw_list in
				List.map (fun uuid ->
					let raw = String.rtrim (vsctl ["get"; "interface"; uuid; "name"]) in
					String.sub raw 1 (String.length raw - 2)) uuids
			else
				[]
		with _ -> []

	let bridge_to_ports name =
		try
			let ports = String.rtrim (vsctl ["list-ports"; name]) in
			let ports' =
				if ports <> "" then
					String.split '\n' ports
				else
					[]
			in
			List.map (fun port -> port, port_to_interfaces port) ports'
		with _ -> []

	let bridge_to_interfaces name =
		try
			let ifaces = String.rtrim (vsctl ["list-ifaces"; name]) in
			if ifaces <> "" then
				String.split '\n' ifaces
			else
				[]
		with _ -> []

	let bridge_to_vlan name =
		try
			Some (vsctl ["br-to-parent"; name], int_of_string (vsctl ["br-to-vlan"; name]))
		with _ -> None

	let get_bond_links_up name =
		try
			let check_line line =
				if (String.startswith "slave" line) && (String.endswith "enabled" line) then 1 else 0
			in
			let raw = appctl ["bond/show"; name] in
			let lines = String.split '\n' raw in
			let nb_links = List.fold_left (fun acc line -> acc + (check_line line)) 0 lines in
			nb_links
		with _ -> 0

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
				let interfaces = bridge_to_interfaces bridge in
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
				ignore (call_script ~log_successful_output:true ovs_vlan_bug_workaround [interface; setting]);
			with _ -> ());
		) phy_interfaces

	let create_bridge ?mac ?external_id ?disable_in_band ~fail_mode vlan vlan_bug_workaround name =
		let vlan_arg = match vlan with
			| None -> []
			| Some (parent, tag) ->
				handle_vlan_bug_workaround vlan_bug_workaround parent;
				[parent; string_of_int tag]
		in
		let mac_arg = match mac with
			| None -> []
			| Some mac ->
				if vlan = None then
					["--"; "set"; "bridge"; name; Printf.sprintf "other-config:hwaddr=\"%s\"" (String.escaped mac)]
				else
					["--"; "set"; "interface"; name; Printf.sprintf "MAC=\"%s\"" (String.escaped mac)]
		in
		let fail_mode_arg =
			if vlan = None then ["--"; "set"; "bridge"; name; "fail_mode=" ^ fail_mode] else [] in
		let external_id_arg = match external_id with
			| None -> []
			| Some (key, value) ->
				match vlan with
				| None -> ["--"; "br-set-external-id"; name; key; value]
				| Some (parent, _) -> ["--"; "br-set-external-id"; parent; key; value]
		in
		let disable_in_band_arg =
			if vlan = None then
				match disable_in_band with
				| None -> []
				| Some None -> ["--"; "remove"; "bridge"; name; "other_config"; "disable-in-band"]
				| Some (Some dib) -> ["--"; "set"; "bridge"; name; "other_config:disable-in-band=" ^ dib]
			else
				[]
		in
		let vif_arg =
			let existing_vifs = List.filter (fun iface -> not (Sysfs.is_physical iface)) (bridge_to_interfaces name) in
			List.flatten (List.map (fun vif -> ["--"; "--may-exist"; "add-port"; name; vif]) existing_vifs)
		in
		let del_old_arg =
			if vlan <> None then
				(* This is to handle the case that a "real" bridge (not a "fake" VLAN bridge) already exists *)
				["--"; "--if-exists"; "del-br"; name]
			else
				[]
		in
		vsctl ~log:true (del_old_arg @ ["--"; "--may-exist"; "add-br"; name] @
			vlan_arg @ mac_arg @ fail_mode_arg @ disable_in_band_arg @ external_id_arg @ vif_arg)

	let destroy_bridge name =
		vsctl ~log:true ["--"; "--if-exists"; "del-br"; name]

	let list_bridges () =
		let bridges = String.rtrim (vsctl ["list-br"]) in
		if bridges <> "" then
			String.split '\n' bridges
		else
			[]

	let get_vlans name =
		try
			let vlans_with_uuid =
				let raw = vsctl ["--bare"; "-f"; "table"; "--"; "--columns=name,_uuid"; "find"; "port"; "fake_bridge=true"] in
				if raw <> "" then
					let lines = String.split '\n' (String.rtrim raw) in
					List.map (fun line -> Scanf.sscanf line "%s %s" (fun a b-> a, b)) lines
				else
					[]
			in
			let bridge_ports =
				let raw = vsctl ["get"; "bridge"; name; "ports"] in
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

	let create_port name bridge =
		vsctl ~log:true ["--"; "--may-exist"; "add-port"; bridge; name]

	let destroy_port name =
		vsctl ~log:true ["--"; "--with-iface"; "--if-exists"; "del-port"; name]

	let port_to_bridge name =
		vsctl ~log:true ["port-to-br"; name]

	let make_bond_properties name properties =
		let known_props = ["mode"; "hashing-algorithm"; "updelay"; "downdelay"; "miimon"; "use_carrier"; "rebalance-interval"] in
		let mode_args =
			let mode = if List.mem_assoc "mode" properties then List.assoc "mode" properties else "balance-slb" in
			let halgo = if List.mem_assoc "hashing-algorithm" properties then List.assoc "hashing-algorithm" properties else "" in
			if mode = "lacp" then "lacp=active" ::
				(if halgo = "src_mac" then ["bond_mode=balance-slb"]
				else if halgo = "tcpudp_ports" then ["bond_mode=balance-tcp"]
				else begin
					debug "bond %s has invalid bond-hashing-algorithm '%s'; defaulting to balance-tcp" name halgo;
					["bond_mode=balance-tcp"]
				end)
			else
				["lacp=off"; "bond_mode=" ^ mode]
		in
		let get_prop (prop, ovs_key) =
			if List.mem_assoc prop properties then
				let value = List.assoc prop properties in
				let value' = try int_of_string value with _ -> -1 in
				if value' < 0 then begin
					debug "bond %s has invalid %s '%s'" name prop value;
					[]
				end else if prop = "use_carrier" then
					[ovs_key ^ "=" ^ (if value' > 0 then "carrier" else "miimon")]
				else
					[ovs_key ^ "=" ^ (string_of_int value')]
			else
				[]
		in
		let extra_args = List.flatten (List.map get_prop ["updelay", "bond_updelay"; "downdelay", "bond_downdelay";
			"miimon", "other-config:bond-miimon-interval"; "use_carrier", "other-config:bond-detect-mode";
			"rebalance-interval", "other-config:bond-rebalance-interval"]) in
		let other_args = List.filter_map (fun (k, v) ->
			if List.mem k known_props then None
			else Some (Printf.sprintf "other-config:\"%s\"=\"%s\"" (String.escaped ("bond-" ^ k)) (String.escaped v))
		) properties in
		mode_args @ extra_args @ other_args

	let create_bond ?mac name interfaces bridge properties =
		let args = make_bond_properties name properties in
		let mac_args = match mac with
			| None -> []
			| Some mac -> ["--"; "set"; "port"; name; "MAC=\"" ^ (String.escaped mac) ^ "\""]
		in
		vsctl ~log:true (["--"; "--may-exist"; "add-bond"; bridge; name] @ interfaces @
			mac_args @ args)

	let get_fail_mode bridge =
		vsctl ["get-fail-mode"; bridge]

	let add_default_flows bridge mac interfaces =
		let ports = List.map (fun interface -> vsctl ["get"; "interface"; interface; "ofport"]) interfaces in
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
		List.iter (fun flow -> ignore (ofctl ~log:true ["add-flow"; bridge; flow])) flows
end

module Brctl = struct
	let call ?(log=false) args =
		call_script ~log_successful_output:log brctl args

	let create_bridge name =
		if not (List.mem name (Sysfs.list ())) then
			ignore (call ~log:true ["addbr"; name])

	let destroy_bridge name =
		if List.mem name (Sysfs.list ()) then
			ignore (call ~log:true ["delbr"; name])

	let create_port bridge name =
		if not (List.mem name (Sysfs.bridge_to_interfaces bridge)) then
			ignore (call ~log:true ["addif"; bridge; name])

	let destroy_port bridge name =
		if List.mem name (Sysfs.bridge_to_interfaces bridge) then
			ignore (call ~log:true ["delif"; bridge; name])

end

module Ethtool = struct
	let call ?(log=false) args =
		call_script ~log_successful_output:log ethtool args

	let set_options name options =
		if options <> [] then
			ignore (call ~log:true ("-s" :: name :: (List.concat (List.map (fun (k, v) -> [k; v]) options))))

	let set_offload name options =
		if options <> [] then
			ignore (call ~log:true ("-K" :: name :: (List.concat (List.map (fun (k, v) -> [k; v]) options))))
end

module Bindings = struct
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

	external _get_status : Unix.file_descr -> string -> int * duplex = "stub_link_get_status"

	(** Returns speed and duplex for a given network interface.
	 *  Note: from kernel 2.6.33, this information is also present in sysfs. *)
	let get_status name =
		try
			with_fd (fun fd -> _get_status fd name)
		with _ -> raise (Read_error "stub_link_get_status")
end

module Dhcp6c = struct
	let pid_file interface =
		Printf.sprintf "/var/run/dhcp6c-%s.pid" interface

	let start interface =
		ignore (call_script dhcp6c [interface])

	let stop interface =
		ignore (call_script dhcp6c ["-r"; "all"; interface])
end
