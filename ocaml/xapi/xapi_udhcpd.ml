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
(* Interface to udhcpd *)

open Stringext

module D = Debug.Debugger(struct let name="xapi_udhcpd" end)
open D

open Forkhelpers
open Pervasiveext
open Threadext

let ip_begin_key = "ip_begin"
let ip_end_key = "ip_end"

let udhcpd_conf = "/var/xapi/udhcpd.conf"
let udhcpd_skel = "/var/xapi/udhcpd.skel"
let pidfile = "/var/run/udhcpd.pid"
let command = "/opt/xensource/libexec/udhcpd"

module Ip = struct
	type t = int * int * int * int

	exception Invalid_ip of t

	let check ((a, b, c, d) as ip) = 
		if a >= 256 || b >= 256 || c >= 256 || d >=256 then raise (Invalid_ip ip) else ip
			
	let string_of (a, b, c, d) = Printf.sprintf "%d.%d.%d.%d" a b c d
	let of_string s = Scanf.sscanf s "%d.%d.%d.%d" (fun a b c d -> check (a,b,c,d))

	(** [succ ip] returns the "next" address after [ip] *)
	let succ (a, b, c, d) =
		let (a, b, c, d) = (a, b, c, d + 1) in
		let (a, b, c, d) = if d < 256 then (a, b, c, d) else (a, b, c + 1, 0) in
		let (a, b, c, d) = if c < 256 then (a, b, c, d) else (a, b + 1, 0, d) in
		let (a, b, c, d) = if b < 256 then (a, b, c, d) else (a + 1, 0, c, d) in
		check (a, b, c, d)

	(** [gt a b] returns true iff [a] is later than [b] in the sequence *)
	let gt (a, b, c, d) (a', b', c', d') =
		(a > a') || ((a = a') && (b > b')) || ((a = a') && (b = b') && (c > c')) || ((a = a') && (b = b') && (c = c') && (d > d'))
	
	(** [first a b f] returns [Some x] where [x] is the first address in the sequence from
		[a] to [b] where [f x] is true if it exists, and [None] otherwise. *)
	let rec first a b f =
		if gt a b then None
		else
			if f a then Some a
			else first (succ a) b f
end

type static_lease = { 
	mac : string;
	ip : Ip.t;
	vif : API.ref_VIF; 
}

let assigned = ref [] 

module Udhcpd_conf = struct
	type t = {
		interface: string;
		subnet: string;
		router: Ip.t;
		leases: static_lease list;
	}
			
	let make ~__context leases router =
      let network = Helpers.get_guest_installer_network ~__context in
      let interface = Db.Network.get_bridge ~__context ~self:network in
      let other_config = Db.Network.get_other_config ~__context ~self:network in
      let subnet = List.assoc "netmask" other_config in
	  {
		  interface = interface;
		  subnet = subnet;
		  router = router;
		  leases = leases
	  }
	
	let to_string t =
		let skel = Unixext.string_of_file udhcpd_skel in
		let interface = Printf.sprintf "interface\t%s" t.interface in
		let subnet = Printf.sprintf "option\tsubnet\t%s" t.subnet in
		let router = Printf.sprintf "option\trouter\t%s" (Ip.string_of t.router) in
		let string_of_lease l =
			Printf.sprintf "static_lease\t%s\t%s # %s\n" l.mac (Ip.string_of l.ip) (Ref.string_of l.vif) in
		let leases = List.map string_of_lease t.leases in
		String.concat "\n" (skel :: interface :: subnet :: router :: leases)

end

let write_config_nolock ~__context ip_router =
	let config = Udhcpd_conf.make ~__context (!assigned) ip_router in
	Unixext.unlink_safe udhcpd_conf;
	Unixext.write_string_to_file udhcpd_conf (Udhcpd_conf.to_string config)
  
let restart_nolock () =
	let pid = try Unixext.pidfile_read pidfile with _ -> None in
	Opt.iter Unixext.kill_and_wait pid;
	let (_: string * string) = execute_command_get_output command [ udhcpd_conf ] in
	()

let find_lease_nolock vif =
	try 
		Some (List.find (fun l -> l.vif = vif) !assigned)
	with Not_found ->
		None

let maybe_add_lease_nolock ~__context vif =
	let network = Helpers.get_host_internal_management_network ~__context in
	if network = Db.VIF.get_network ~__context ~self:vif then begin
		let other_config = Db.Network.get_other_config ~__context ~self:network in
		if not(List.mem_assoc ip_begin_key other_config) || not(List.mem_assoc ip_end_key other_config)
		then failwith (Printf.sprintf "Host internal management network %s other_config has no ip_begin/ip_end keys" (Ref.string_of network));

		let ip_begin = Ip.of_string (List.assoc ip_begin_key other_config)
		and ip_end = Ip.of_string (List.assoc ip_end_key other_config) in
		match find_lease_nolock vif with
			| Some l ->
				info "VIF %s on host-internal management network already has lease: %s" (Ref.string_of vif) (Ip.string_of l.ip)
			| None -> begin
				let mac = Db.VIF.get_MAC ~__context ~self:vif in
				(* NB ip_begin is the address on the bridge itself *)
				match Ip.first (Ip.succ ip_begin) ip_end 
					(fun ip -> List.filter (fun l -> l.ip = ip) !assigned = []) with
						| Some ip ->
							assigned := {mac=mac; ip=ip; vif=vif} :: !assigned;
							write_config_nolock ~__context ip_begin;
							restart_nolock ()
						| None ->
							error "VM on guest installer network, but not IPs available"; 
							failwith "No IP addresses left"
			end
	end

let mutex = Mutex.create ()

let maybe_add_lease ~__context vif =
	Helpers.log_exn_continue (Printf.sprintf "maybe_add_lease VIF:%s" (Ref.string_of vif)) 
		(fun () ->
			Mutex.execute mutex
				(fun () ->
					maybe_add_lease_nolock ~__context vif
				)
		) ()

(* Don't bother restarting udhcpd *)
let maybe_remove_lease ~__context vif =
  Mutex.execute mutex
	  (fun () ->
		  assigned := List.filter (fun lease -> lease.vif <> vif) !assigned
	  )

let get_ip ~__context vif =
	Mutex.execute mutex
		(fun () -> Opt.map (fun l -> l.ip) (find_lease_nolock vif))
