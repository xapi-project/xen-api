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

let udhcpd_conf = "/var/xapi/udhcpd.conf"
let udhcpd_skel = "/var/xapi/udhcpd.skel"
let pidfile = "/var/run/udhcpd.pid"
let command = "/opt/xensource/libexec/udhcpd"

type static_lease = { 
	mac : string;
	ip : (int*int*int*int);
	vif : API.ref_VIF; 
}

let assigned = ref [] 
let mutex = Mutex.create ()


let write_config ~__context ip_router =
	Mutex.execute mutex
    (fun () ->
      Unixext.unlink_safe udhcpd_conf;
      let oc = Unix.openfile udhcpd_conf [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
      
      (* Write the pre-existing config file first *)
      let ic = Unix.openfile udhcpd_skel [Unix.O_RDONLY] 0o644 in
      ignore_int64 (Unixext.copy_file ic oc);
      Unix.close ic;
      
      let network = Helpers.get_guest_installer_network ~__context in
      let bridge = Db.Network.get_bridge ~__context ~self:network in
      Unixext.really_write_string oc (Printf.sprintf "interface\t%s\n" bridge);
      let other_config = Db.Network.get_other_config ~__context ~self:network in
      let netmask = List.assoc "netmask" other_config in
      Unixext.really_write_string oc (Printf.sprintf "option\tsubnet\t%s\n" netmask);
      Unixext.really_write_string oc (Printf.sprintf "option\trouter\t%s\n" ip_router);
      
      let list = !assigned in
      List.iter (fun lease -> 
	let (a,b,c,d) = lease.ip in
	ignore (Unixext.really_write_string oc (Printf.sprintf "static_lease\t%s\t%d.%d.%d.%d # %s\n" lease.mac a b c d (Ref.string_of lease.vif)))) list;
      Unix.close oc)
  
let run () =
	let pid = try Unixext.pidfile_read pidfile with _ -> None in
	Opt.iter Unixext.kill_and_wait pid;
	execute_command_get_output command [ udhcpd_conf ]

let find_unused_ip ip_begin ip_end =
  let (a,b,c,d) = Scanf.sscanf ip_begin "%d.%d.%d.%d" (fun a b c d -> (a,b,c,d)) in
  let (_,_,c',d') = Scanf.sscanf ip_end "%d.%d.%d.%d" (fun a b c d -> (a,b,c,d)) in 
  let check ip = 
    List.length (List.filter (fun lease -> lease.ip = ip) !assigned) = 0 in
  let rec scan myc myd =
    if myd>d' 
    then 
      begin
	if myc=c' then
	  (error "VM on guest installer network, but not IPs available"; failwith "No IP addresses left")
	else
	  scan (myc+1) d
      end
    else
      let ip = (a,b,myc,myd) in
      if check ip then ip else scan myc (myd+1)  
  in
  scan c (d+1) (* d+1 because d is used by the bridge itself! *)

(* Slightly odd - we call add_lease with the VIF rather than the VM so that it can be hooked into the create_vif call *)
(* in vmops, but we call remove_lease with the VM *)
let maybe_add_lease ~__context vif =
  let network = Helpers.get_guest_installer_network ~__context in
  if network=Db.VIF.get_network ~__context ~self:vif then
    try 
      debug "Adding lease";
      let mac = Db.VIF.get_MAC ~__context ~self:vif in
      let other_config = Db.Network.get_other_config ~__context ~self:network in
      let ip_begin = List.assoc "ip_begin" other_config in
      let ip_end = List.assoc "ip_end" other_config in
      Mutex.execute mutex
	  (fun () -> 
	  if List.exists (fun lease -> lease.vif=vif) !assigned then () else
	    let ip = find_unused_ip ip_begin ip_end in
	    let (a,b,c,d) = ip in
	    debug "ip=%d.%d.%d.%d" a b c d;
	    assigned := {mac=mac; ip=ip; vif=vif} :: !assigned;
	    List.iter (fun lease -> let (a,b,c,d) = lease.ip in debug "lease: mac=%s ip=%d.%d.%d.%d vif=%s" lease.mac a b c d (Ref.string_of vif)) !assigned);
      write_config ~__context ip_begin;
      ignore(run ())
    with e -> (debug "exception caught: %s" (Printexc.to_string e); log_backtrace ())

(* Don't bother restarting udhcpd *)
let maybe_remove_lease ~__context vif =
  Mutex.execute mutex
	  (fun () ->
		  assigned := List.filter (fun lease -> lease.vif <> vif) !assigned
	  )

let get_ip ~__context vif =
	Mutex.execute mutex
		(fun () ->
			try 
				Some ((List.find (fun l -> l.vif = vif) !assigned).ip)
			with Not_found ->
				None
		)
