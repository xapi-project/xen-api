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
open Memory_interface

module D = Debug.Make(struct let name = Memory_interface.service_name end)
open D

let name = "squeezed"
let major_version = 0
let minor_version = 1

(* Server configuration. We have built-in (hopefully) sensible defaults,
   together with command-line arguments and a configuration file. They
   are applied in order: (latest takes precedence)
      defaults < arguments < config file
*)
let sockets_group = ref "xapi"
let config_file = ref (Printf.sprintf "/etc/%s.conf" name)
let pidfile = ref (Printf.sprintf "/var/run/%s.pid" name)
let log_destination = ref "syslog:daemon"
let daemon = ref false
let balance_check_interval = ref 10.

let config_spec = [
	"sockets-group", Arg.Set_string sockets_group, (fun () -> !sockets_group), "Group to allow access to the control socket";
	"pidfile", Arg.Set_string pidfile, (fun () -> !pidfile), "Filename to write process PID";
	"log", Arg.Set_string log_destination, (fun () -> !log_destination), "Where to write log messages";
	"daemon", Arg.Bool (fun x -> daemon := x), (fun () -> string_of_bool !daemon), "True if we are to daemonise";
	"balance-check-interval", Arg.Set_float balance_check_interval, (fun () -> string_of_float !balance_check_interval), "Seconds between memory balancing attempts";
	"disable-logging-for", Arg.String
		(fun x ->
			try
				let modules = Re_str.split (Re_str.regexp "[ ]+") x in
				List.iter Debug.disable modules
			with e ->
				error "Processing disabled-logging-for = %s: %s" x (Printexc.to_string e)
		), (fun () -> String.concat " " (!Debug.disabled_modules)), "A space-separated list of debug modules to suppress logging from";
	"config", Arg.Set_string config_file, (fun () -> !config_file), "Location of configuration file";
]

let arg_spec = List.map (fun (a, b, _, c) -> "-" ^ a, b, c) config_spec

let read_config_file () =
	if Sys.file_exists !config_file then begin
		(* Will raise exception if config is mis-formatted. It's up to the
		   caller to inspect and handle the failure.
		*)
		Config_parser.parse_file !config_file config_spec;
		debug "Read global variables successfully from %s" !config_file
	end

(* Apply a binary message framing protocol where the first 16 bytes are an integer length
   stored as an ASCII string *)
let binary_handler call_of_string string_of_response process s context =
	let ic = Unix.in_channel_of_descr s in
	let oc = Unix.out_channel_of_descr s in
	(* Read a 16 byte length encoded as a string *)
	let len_buf = String.make 16 '\000' in
	really_input ic len_buf 0 (String.length len_buf);
	let len = int_of_string len_buf in
	let msg_buf = String.make len '\000' in
	really_input ic msg_buf 0 (String.length msg_buf);
	let (request: Rpc.call) = call_of_string msg_buf in
	let (result: Rpc.response) = process context request in
	let msg_buf = string_of_response result in
	let len_buf = Printf.sprintf "%016d" (String.length msg_buf) in
	output_string oc len_buf;
	output_string oc msg_buf;
	flush oc

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
	(try Unix.unlink json_path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
	let json_sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind json_sock (Unix.ADDR_UNIX json_path);
	Unix.listen json_sock 5;

	json_sock

let start json_sock process =
	debug "Listening on %s" json_path;
	accept_forever json_sock
		(fun this_connection ->
			let context = () in
			binary_handler Jsonrpc.call_of_string Jsonrpc.string_of_response process (* no req *) this_connection context
		)


let _ = 
	debug "squeezed version %d.%d starting" major_version minor_version;

	Arg.parse (Arg.align arg_spec)
		(fun _ -> failwith "Invalid argument")
		(Printf.sprintf "Usage: %s [-config filename]" name);

	read_config_file ();
	Config_parser.dump config_spec;

	(* Check the sockets-group exists *)
	if try ignore(Unix.getgrnam !sockets_group); false with _ -> true then begin
		error "Group %s doesn't exist." !sockets_group;
		error "Either create the group, or select a different group by modifying the config file:";
		error "# Group which can access the control socket";
		error "sockets-group=<some group name>";
		exit 1
	end;

	(* Accept connections before we have daemonized *)
	let sockets = prepare_sockets path in

	if !daemon then begin
		debug "About to daemonize";
		Debug.output := Debug.syslog "squeezed" ();
		Unixext.daemonize();
        end;

	Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
	Unixext.pidfile_write !pidfile;

	debug "Starting daemon listening on %s with idle_timeout = %.0f" _service !balance_check_interval;

	let module Server = Memory_interface.Server(Memory_server) in

	Memory_server.start_balance_thread balance_check_interval;
	Squeeze_xen.Domain.start_watch_xenstore_thread ();

	let (_: Thread.t) = Thread.create (fun () -> start sockets Server.process) () in

	while true do
		try
			Thread.delay 60.
		with e ->
			debug "Thread.delay caught: %s" (Printexc.to_string e)
	done


