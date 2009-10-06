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

(* v6 licensing daemon *)
open Stringext

module D=Debug.Debugger(struct let name="v6daemon" end)
open D

let xmlrpc_handler process req bio =
	let path = match String.split '/' req.Http.uri with
	| x::path::_ -> path
	| _ -> failwith "Unknown path"
	in
	debug "path=%s" path;
	let body = Http_svr.read_body req bio in
	debug "Request: %s" body;
	let s = Buf_io.fd_of bio in
	let xml = Xml.parse_string body in
	let result = process xml in
	let str = Xml.to_string result in
	debug "Response: %s" str;
	Http_svr.response_str req s str


let daemon = ref false
let pidfile = ref ""

(* A lot of this boilerplate ought to go into a utility library *)
let startup post_daemonize_hook process =
	(* Parse command-line arguments *)
	Arg.parse [ "-daemon", Arg.Set daemon, "Daemonize";
				"-pidfile", Arg.Set_string pidfile, "pidfile"]
		(fun x -> warn "Ignoring argument: %s" x)
		"v6 licensing daemon";

	if !daemon then
		Unixext.daemonize ();

	if !pidfile <> "" then
		Unixext.pidfile_write !pidfile;

	post_daemonize_hook ();
	
	(* unix socket *)
	let unix_socket_path = "/var/xapi/v6" in
	Unixext.mkdir_safe (Filename.dirname unix_socket_path) 0o700;
	Unixext.unlink_safe unix_socket_path;
	let domain_sock = Http_svr.bind (Unix.ADDR_UNIX(unix_socket_path)) in
	ignore(Http_svr.start (domain_sock, "unix-RPC"));
	Http_svr.add_handler Http.Post "/" (Http_svr.BufIO (xmlrpc_handler process));

	(* TCP socket: only use for testing! *)
(*	let localhost = Unix.inet_addr_of_string "127.0.0.1" in
	let localhost_sock = Http_svr.bind (Unix.ADDR_INET(localhost, 4094)) in
	Unix.setsockopt localhost_sock Unix.SO_REUSEADDR true;
	ignore(Http_svr.start (localhost_sock, "inet-RPC"));*)

	(* keep daemon alive *)
	Threadext.keep_alive ()
	
