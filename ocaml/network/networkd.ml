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

let name = "xcp-networkd"

open Pervasiveext
open Fun
open Network_utils

module D = Debug.Debugger(struct let name = "networkd" end)
open D

let server = Http_svr.Server.empty ()

let path = Filename.concat Fhs.vardir name

module Server = Network_interface.Server(Network_server)

let xmlrpc_handler process req bio context =
	let body = Http_svr.read_body req bio in
	let s = Buf_io.fd_of bio in
	let rpc = Xmlrpc.call_of_string body in
	try
		let result = process context rpc in
		let str = Xmlrpc.string_of_response result in
		Http_svr.response_str req s str
	with e ->
		debug "Caught %s" (Printexc.to_string e);
		debug "Backtrace: %s" (Printexc.get_backtrace ());
		Http_svr.response_unauthorised ~req (Printf.sprintf "Go away: %s" (Printexc.to_string e)) s

let start_server path process =
	Http_svr.Server.add_handler server Http.Post "/" (Http_svr.BufIO (xmlrpc_handler process));

	Unixext.mkdir_safe (Filename.dirname path) 0o700;
	Unixext.unlink_safe path;
	let domain_sock = Http_svr.bind (Unix.ADDR_UNIX(path)) "unix_rpc" in
	Http_svr.start server domain_sock;

	let localhost = Unix.inet_addr_of_string "127.0.0.1" in
	let localhost_sock = Http_svr.bind (Unix.ADDR_INET(localhost, 4094)) "inet-RPC" in
	Http_svr.start server localhost_sock;

	()

let start () =
	Network_monitor_thread.start ();
	Network_server.on_startup ();
	start_server path Server.process

let stop signal =
	Network_server.on_shutdown signal;
	Network_monitor_thread.stop ();
	exit 0

let handle_shutdown () =
	Sys.set_signal Sys.sigterm (Sys.Signal_handle stop);
	Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
	Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let _ =
	let pidfile = ref "" in
	let daemonize = ref false in
	Debug.set_facility Syslog.Local5;

	Arg.parse (Arg.align [
			"-daemon", Arg.Set daemonize, "Create a daemon";
			"-pidfile", Arg.Set_string pidfile, Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
		])
		(fun _ -> failwith "Invalid argument")
		(Printf.sprintf "Usage: %s [-daemon] [-pidfile filename]" name);

	debug "%s" (String.concat ", " (Debug.get_all_debug_keys()));

	if !daemonize then
		Unixext.daemonize ()
	else
		Debug.log_to_stdout ();

	if !pidfile <> "" then begin
		Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
		Unixext.pidfile_write !pidfile;
	end;

	handle_shutdown ();
	Debug.with_thread_associated "main" start ();
	while true do
		Thread.delay 300.;
		Network_server.on_timer ()
	done

