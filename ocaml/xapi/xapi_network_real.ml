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

open Xmlrpc_client
open Forkhelpers
open Threadext
open Pervasiveext

let himn_addr = ref None
let server = ref None
let m = Mutex.create ()

let port_to_forward = Xapi_globs.http_port
let forward_via_https = true (** encrypt forwarded traffic to other hosts *)

(* Only use HTTPs if we're (a) forwarding; and (b) we want to *)
let use_https master_ip = 
	forward_via_https && master_ip <> "127.0.0.1" 

let http_proxy master_ip ip = 
	let transport =
		if use_https master_ip
		then SSL(SSL.make(), master_ip, !Xapi_globs.https_port)
		else TCP(master_ip, Xapi_globs.http_port) in
	let tcp_connection _ fromfd =
		(* NB 'fromfd' is accepted within the server_io module and it expects us to close it *)
		finally
			(fun () ->
				let bio = Buf_io.of_fd fromfd in
				let request = Http_svr.request_of_bio bio in

				let proxy_one request fromfd s =
					(* We can only proxy certain types of request properly *)
					match request.Http.Request.m with
						| Http.Get | Http.Post | Http.Put ->
							(* Set Connection:close if it's not already set *)
							request.Http.Request.close <- true;
							(* Transmit request headers to master *)
							Unixext.really_write_string s (Http.Request.to_wire_string request);
							let limit = match request.Http.Request.m with
								| Http.Get -> Some 0L
								| _ -> request.Http.Request.content_length in
							let (_: int64) = Unixext.copy_file ?limit fromfd s in
							(* Receive response headers from master *)
							let response = Opt.default Http.Response.internal_error (Http_client.response_of_fd s) in
							(* Transmit response headers to client *)
							Unixext.really_write_string fromfd (Http.Response.to_wire_string response);
							if response.Http.Response.code = "200" then begin
								(* If there is a request payload then transmit *)								
								let (_: int64) = Unixext.copy_file ?limit:response.Http.Response.content_length s fromfd in
								()
							end;
						| m ->
							error "Proxy doesn't support: %s" (Http.string_of_method_t m);
							Http_svr.response_forbidden ~req:request fromfd
				in
				Opt.iter
					(fun request ->
						with_transport transport (proxy_one request fromfd)
					) request;
			) (fun () -> Unix.close fromfd)
	in
	try
		let addr = Unix.inet_addr_of_string ip in
		let sockaddr = Unix.ADDR_INET (addr, port_to_forward) in
		Mutex.execute m
			(fun () ->
				(* shutdown any server which currently exists *)
				maybe (fun server -> server.Server_io.shutdown ()) !server;
				(* Make sure we don't try to double-close the server *)
				server := None;
				let handler = { Server_io.name = "http_proxy"; body = tcp_connection } in
				let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
				Unix.setsockopt sock Unix.SO_REUSEADDR true;
				Unix.bind sock sockaddr;
				Unixext.set_tcp_nodelay sock true;
				Unix.listen sock 5;
				let s = Server_io.server handler sock in
				server := Some s
			)
	with e ->
		error "Caught exception setting up proxy from internal network: %s" (ExnHelper.string_of_exn e);
		raise e
			
let maybe_start bridge other_config =
	if not(List.mem_assoc "ip_begin" other_config)
	then error "Cannot setup host internal management network: no other-config:ip_begin"
	else begin
		let ip = List.assoc "ip_begin" other_config in
		let address = Helpers.get_main_ip_address () in
		debug "Setting up repeater from %s:%d -> %s %s" ip port_to_forward address (if use_https address then "over SSL" else "plaintext");
		(* Set the ip address of the bridge *)
		ignore(Forkhelpers.execute_command_get_output "/sbin/ifconfig" [ bridge; ip; "up" ]);
		himn_addr := Some ip;
		ignore(Thread.create (fun () -> http_proxy address ip) ())
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
