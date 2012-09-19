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
open Pervasiveext 
open Squeezed_state
open Xenops_helpers
open Xenstore
open Memory_interface

module D = Debug.Debugger(struct let name = Memory_interface.service_name end)
open D

let name = "squeezed"

(* Server configuration. We have built-in (hopefully) sensible defaults,
   together with command-line arguments and a configuration file. They
   are applied in order: (latest takes precedence)
      defaults < arguments < config file
*)
let config_file = ref (Printf.sprintf "/etc/%s.conf" name)
let pidfile = ref (Printf.sprintf "/var/run/%s.pid" name)
let log_destination = ref "syslog:daemon"
let daemon = ref false
let balance_check_interval = ref 10.

let config_spec = [
	"pidfile", Config.Set_string pidfile;
	"log", Config.Set_string log_destination;
	"daemon", Config.Set_bool daemon;
	"balance-check-interval", Config.Set_float balance_check_interval;
	"disable-logging-for", Config.String
		(fun x ->
			try
				let open Stringext in
				let modules = String.split_f String.isspace x in
				List.iter Debug.disable modules
			with e ->
				error "Processing disabled-logging-for = %s: %s" x (Printexc.to_string e)
		);
]

let read_config_file () =
	let unknown_key k v = debug "Unknown key/value pairs: (%s, %s)" k v in
	if Sys.file_exists !config_file then begin
		(* Will raise exception if config is mis-formatted. It's up to the
		  caller to inspect and handle the failure.
		*)
		Config.read !config_file config_spec unknown_key;
		debug "Read global variables successfully from %s" !config_file
	end

let dump_config_file () : unit =
	debug "pidfile = %s" !pidfile;
	debug "log = %s" !log_destination;
	debug "daemon = %b" !daemon

(* Apply a binary message framing protocol where the first 16 bytes are an integer length
   stored as an ASCII string *)
let binary_handler call_of_string string_of_response process (* no req *) this_connection context =
	(* Read a 16 byte length encoded as a string *)
	let len_buf = Unixext.really_read_string this_connection 16 in
	let len = int_of_string len_buf in
	let msg_buf = Unixext.really_read_string this_connection len in
	let request = call_of_string msg_buf in
	let result = process context request in
	let msg_buf = string_of_response result in
	let len_buf = Printf.sprintf "%016d" (String.length msg_buf) in
	Unixext.really_write_string this_connection len_buf;
	Unixext.really_write_string this_connection msg_buf

let accept_forever sock f =
	let (_: Thread.t) = Thread.create
		(fun () ->
			while true do
				let this_connection, _ = Unix.accept sock in
				let (_: Thread.t) = Thread.create
					(fun () ->
						finally
							(fun () -> f this_connection)
							(fun () -> Unix.close this_connection)
					) () in
				()
			done
		) () in
	()

(* Start accepting connections on sockets before we daemonize *)
let prepare_sockets path =
	(* Start receiving local binary messages *)
	Unixext.unlink_safe json_path;
	let json_sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind json_sock (Unix.ADDR_UNIX json_path);
	Unix.listen json_sock 5;

	json_sock

let server = Http_svr.Server.empty ()

let start json_sock process =
	Http_svr.Server.enable_fastpath server;
	debug "Listening on %s" json_path;
	accept_forever json_sock
		(fun this_connection ->
			let context = () in
			binary_handler Jsonrpc.call_of_string Jsonrpc.string_of_response process (* no req *) this_connection context
		)


let _ = 
	Debug.set_facility Syslog.Local5;

  let daemonize = ref false in
 
  Arg.parse (Arg.align [
	       "-daemon", Arg.Set daemonize, "Create a daemon";
	       "-pidfile", Arg.Set_string pidfile, Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
		"-config", Arg.Set_string config_file, Printf.sprintf "Read configuration from the specified config file (default \"%s\")" !config_file;
	     ])
    (fun _ -> failwith "Invalid argument")
    "Usage: squeezed [-daemon] [-pidfile filename] [-config filename]";

	read_config_file ();

	dump_config_file ();

  begin
    try Xapi_globs.read_external_config ()
    with e -> debug "Read global variables config from %s failed: %s. Continue with default setting." Xapi_globs.xapi_globs_conf (Printexc.to_string e)
  end;

	(* Accept connections before we have daemonized *)
	let sockets = prepare_sockets path in

  if !daemonize then Unixext.daemonize ();

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  Unixext.pidfile_write !pidfile;

	debug "Starting daemon listening on %s with idle_timeout = %.0f" _service !balance_check_interval;

	let module Server = Memory_interface.Server(Memory_server) in
	Debug.with_thread_associated "main" (start sockets) Server.process;

	Memory_server.start_balance_thread balance_check_interval;
	Squeeze_xen.Domain.start_watch_xenstore_thread ();

	while true do
		try
			Thread.delay 60.
		with e ->
			debug "Thread.delay caught: %s" (Printexc.to_string e)
	done


