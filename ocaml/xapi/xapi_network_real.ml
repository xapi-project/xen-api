(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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

module D=Debug.Debugger(struct let name="xapi" end)
open D

open Xapi_network_types
open Forkhelpers
open Threadext
open Pervasiveext

let server = ref None
let m = Mutex.create ()

let port_to_forward = Xapi_globs.http_port
let forward_via_https = true (** encrypt forwarded traffic to other hosts *)

(* Only use HTTPs if we're (a) forwarding; and (b) we want to *)
let use_https master_ip = 
	forward_via_https && master_ip <> "127.0.0.1" 

(* Proxy data from the guest to the master's management IP *)
let make_proxy bridge master_ip ip =
	let addr = Unix.inet_addr_of_string ip in
	let sockaddr = Unix.ADDR_INET (addr, port_to_forward) in
	
	let handler _ fromfd =
		(* NB 'fromfd' is accepted within the server_io module and it expects us to close it *)
		(* NB NB Unixext.proxy closes all fds passed to it *)
		finally
			(fun () ->
				let use_https = use_https master_ip in
  				let dest_port = if use_https then !Xapi_globs.https_port else Xapi_globs.http_port in
		
				if use_https then begin
					debug "Forwarding connection to %s over SSL" master_ip;
					let stunnel = Stunnel.connect master_ip dest_port in
					finally
						(fun () -> Unixext.proxy (Unix.dup fromfd) (Unix.dup stunnel.Stunnel.fd))
						(fun () -> Stunnel.disconnect stunnel)
				end else begin
					debug "Forwarding connection to %s plaintext" master_ip;
					let tofd = Unixext.open_connection_fd master_ip dest_port in
					Unixext.proxy (Unix.dup fromfd) tofd;
				end;
				debug "Connection forwarding to %s finished" master_ip
			)(fun () -> Unix.close fromfd)
	in
	try
		Mutex.execute m
			(fun () ->
				(* shutdown any server which currently exists *)
				maybe (fun server -> server.Server_io.shutdown ()) !server;
				(* Make sure we don't try to double-close the server *)
				server := None;
				let handler = { Server_io.name = "repeater"; body = handler } in
				let sock = Xapi_http.bind sockaddr in
				let s = Server_io.server handler sock in
				server := Some s
			)
	with e ->
		error "Caught exception setting up proxy from internal network: %s" (ExnHelper.string_of_exn e);
		raise e
			
let maybe_start bridge other_config =
	if not(List.mem_assoc "ip_begin" other_config)
	then error "Cannot setup host internal management network: no other-config:i_begin"
	else begin
		let ip = List.assoc "ip_begin" other_config in
		let address = Helpers.get_main_ip_address () in
		debug "Setting up repeater from %s:%d -> %s %s" ip port_to_forward address (if use_https address then "over SSL" else "plaintext");
		(* Set the ip address of the bridge *)
		ignore(Forkhelpers.execute_command_get_output "/sbin/ifconfig" [ bridge; ip; "up" ]);
		ignore(Thread.create (fun () -> make_proxy bridge address ip) ())
	end

let maybe_stop bridge =
	debug "maybe_stop %s" bridge;
	Mutex.execute m
		(fun () -> 
			maybe (fun server -> server.Server_io.shutdown ()) !server;
			server := None
		)

(* If a bridge exists in dom0 which corresponds to a host management internal
   then restart the proxy on xapi start *)
let on_server_start () = 
  Server_helpers.exec_with_new_task "Starting host internal management network"
      (fun __context ->
		  let nets = Db.Network.get_all_records ~__context in
		  let mnets = List.filter (fun (_, n) ->
			  let oc = n.API.network_other_config in
			  (List.mem_assoc Xapi_globs.is_guest_installer_network oc)
			  && (List.assoc Xapi_globs.is_guest_installer_network oc = "true"))
			  nets in
		  let br_to_oc = List.map (fun (r, n) -> 
			  n.API.network_bridge, n.API.network_other_config) 
			  mnets in
		  
		  let brs = Netdev.network.Netdev.list () in
		  List.iter
			  (fun brname ->
				  if List.mem_assoc brname br_to_oc 
				  then maybe_start brname (List.assoc brname br_to_oc))
			  brs
	  )
