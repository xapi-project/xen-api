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

module D=Debug.Make(struct let name="http_proxy" end)
open D

open Xmlrpc_client
open Xapi_stdext_monadic
open Xapi_stdext_threads.Threadext
open Xapi_stdext_pervasives.Pervasiveext

let one request fromfd s =
	let open Xapi_stdext_unix in
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

let server = ref None
let m = Mutex.create ()

let http_proxy src_ip src_port transport = 
	let tcp_connection _ fromfd =
		(* NB 'fromfd' is accepted within the server_io module and it expects us to close it *)
		finally
			(fun () ->
				let bio = Buf_io.of_fd fromfd in
				let request = Http_svr.request_of_bio bio in
				Opt.iter
					(fun request ->
						with_transport transport (one request fromfd)
					) request;
			) (fun () -> Unix.close fromfd)
	in
	try
		let addr = Unix.inet_addr_of_string src_ip in
		let sockaddr = Unix.ADDR_INET (addr, src_port) in
		Mutex.execute m
			(fun () ->
				(* shutdown any server which currently exists *)
				maybe (fun server -> server.Server_io.shutdown ()) !server;
				(* Make sure we don't try to double-close the server *)
				server := None;
				let handler = { Server_io.name = "http_proxy"; body = tcp_connection } in
				let sock = Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
				begin
					(* Make sure exceptions cause the socket to be closed *)
					try
						Unix.set_close_on_exec sock;
						Unix.setsockopt sock Unix.SO_REUSEADDR true;
						(match sockaddr with
						| Unix.ADDR_INET _ -> Xapi_stdext_unix.Unixext.set_tcp_nodelay sock true
						| _ -> ());
						Unix.bind sock sockaddr;
						Unix.listen sock 128
					with e ->
						debug "Caught exception in Http_svr.bind (closing socket): %s" (Printexc.to_string e);
						Unix.close sock;
						raise e
				end;
				let s = Server_io.server handler sock in
				server := Some s
			)
	with e ->
		error "Caught exception setting up proxy from internal network: %s" (Printexc.to_string e);
		raise e
			
