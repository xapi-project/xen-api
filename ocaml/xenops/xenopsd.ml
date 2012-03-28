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
open Xenops_utils
open Pervasiveext 
open Fun

module D = Debug.Debugger(struct let name = service_name end)
open D

let name = "xenopsd"

let major_version = 0
let minor_version = 9

(* Server configuration. We have built-in (hopefully) sensible defaults,
   together with command-line arguments and a configuration file. They
   are applied in order: (latest takes precedence)
      defaults < arguments < config file
*)
let config_file = ref (Printf.sprintf "/etc/%s.conf" name)
let pidfile = ref (Printf.sprintf "/var/run/%s.pid" name)
let log_destination = ref "syslog:daemon"
let simulate = ref false
let persist = ref true
let daemon = ref false
let worker_pool_size = ref 4

let config_spec = [
	"pidfile", Config.Set_string pidfile;
	"log", Config.Set_string log_destination;
	"simulate", Config.Set_bool simulate;
	"persist", Config.Set_bool persist;
	"daemon", Config.Set_bool daemon;
	"disable-logging-for", Config.String
		(fun x ->
			try
				let open Stringext in
				let modules = String.split_f String.isspace x in
				List.iter Debug.disable modules
			with e ->
				error "Processing disabled-logging-for = %s: %s" x (Printexc.to_string e)
		);
	"worker-pool-size", Config.Set_int worker_pool_size;
	"database-path", Config.Set_string Xenops_utils.root;
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

let dump_config_file () =
	debug "pidfile = %s" !pidfile;
	debug "log = %s" !log_destination;
	debug "simulate = %b" !simulate;
	debug "persist = %b" !persist;
	debug "daemon = %b" !daemon

let socket = ref None
let server = Http_svr.Server.empty (Xenops_server.make_context ())

let path = "/var/xapi/xenopsd"
let forwarded_path = path  ^ ".forwarded" (* receive an authenticated fd from xapi *)

module Server = Xenops_interface.Server(Xenops_server)

let xmlrpc_handler process req bio context =
    let body = Http_svr.read_body req bio in
    let s = Buf_io.fd_of bio in
    let rpc = Xmlrpc.call_of_string body in
	(* Xenops_utils.debug "Request: %s %s" rpc.Rpc.name (Jsonrpc.to_string (List.hd rpc.Rpc.params)); *)
	try
		let result = process context rpc in
		(* Xenops_utils.debug "Response: success:%b %s" result.Rpc.success (Jsonrpc.to_string result.Rpc.contents); *)
		let str = Xmlrpc.string_of_response result in
		Http_svr.response_str req s str
	with Xenops_utils.Exception e ->
		let rpc = Xenops_interface.rpc_of_error_response (None, Some e) in
		debug "Caught %s" (Jsonrpc.to_string rpc);
		let str = Xmlrpc.string_of_response { Rpc.success = true; contents = rpc } in
		Http_svr.response_str req s str
	| e ->
		debug "Caught %s" (Printexc.to_string e);
		debug "Backtrace: %s" (Printexc.get_backtrace ());
		Http_svr.response_unauthorised ~req (Printf.sprintf "Go away: %s" (Printexc.to_string e)) s


let get_handler req bio _ =
	let s = Buf_io.fd_of bio in
	Http_svr.response_str req s "<html><body>Hello there</body></html>"

(* Start accepting connections on sockets before we daemonize *)
let prepare_sockets path =
    Unixext.mkdir_safe (Filename.dirname path) 0o700;
    Unixext.unlink_safe path;
    let domain_sock = Http_svr.bind (Unix.ADDR_UNIX(path)) "unix_rpc" in

	(* Start receiving forwarded /file descriptors/ from xapi *)
	Unixext.unlink_safe forwarded_path;
	let forwarded_sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind forwarded_sock (Unix.ADDR_UNIX forwarded_path);
	Unix.listen forwarded_sock 5;

	domain_sock, forwarded_sock

let start (domain_sock, forwarded_sock) process =
    Http_svr.Server.add_handler server Http.Post "/" (Http_svr.BufIO (xmlrpc_handler process));
	Http_svr.Server.add_handler server Http.Put  "/services/xenops/memory" (Http_svr.FdIO Xenops_server.VM.receive_memory);
    Http_svr.Server.add_handler server Http.Get "/" (Http_svr.BufIO get_handler);
    Http_svr.start server domain_sock;
	socket := Some domain_sock;

	let (_: Thread.t) = Thread.create
		(fun () ->
			debug "Listening on %s" forwarded_path;
			(* XXX: need some error handling here *)
			while true do
				let msg_size = 16384 in
				let buf = String.make msg_size '\000' in
				debug "Calling Unix.accept()";
				let this_connection, _ = Unix.accept forwarded_sock in
				debug "Unix.accept() ok";
				let (_: Thread.t) = Thread.create
					(fun () ->
						finally
							(fun () ->
								debug "Calling Unixext.recv_fd()";
								let len, _, received_fd = Unixext.recv_fd this_connection buf 0 msg_size [] in
								debug "Unixext.recv_fd ok (len = %d)" len;
								finally
									(fun () ->
										let req = String.sub buf 0 len |> Jsonrpc.of_string |> Http.Request.t_of_rpc in
										debug "Received request = [%s]\n%!" (req |> Http.Request.rpc_of_t |> Jsonrpc.to_string);
										req.Http.Request.close <- true;
										let context = {
											Xenops_server.transferred_fd = Some received_fd
										} in
										let (_: bool) = Http_svr.handle_one server received_fd context req in
										()
									) (fun () -> Unix.close received_fd)
							) (fun () -> Unix.close this_connection)
					) () in
				()
			done
		) () in
	()

let _ = 
	Debug.set_facility Syslog.Local5;

	debug "xenopsd version %d.%d starting" major_version minor_version;

  Arg.parse (Arg.align [
	       "-daemon", Arg.Set daemon, "Create a daemon";
	       "-pidfile", Arg.Set_string pidfile, Printf.sprintf "Set the pid file (default \"%s\")" !pidfile;
		   "-simulate", Arg.Set simulate, "Use the simulator backend (default is the xen backend)";
		   "-config", Arg.Set_string config_file, Printf.sprintf "Read configuration from the specified config file (default \"%s\")" !config_file;
	     ])
    (fun _ -> failwith "Invalid argument")
    (Printf.sprintf "Usage: %s [-daemon] [-pidfile filename]" name);
  read_config_file ();

  dump_config_file ();

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  (* Accept connections before we have daemonized *)
  let sockets = prepare_sockets path in

  if !daemon
  then Unixext.daemonize ()
  else Debug.log_to_stdout ();

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  Unixext.pidfile_write !pidfile;

  if not(!persist) then Xenops_utils.empty_database ();

  Xenops_server.register_objects();
  Xenops_server.set_backend
	  (Some (if !simulate
	  then (module Xenops_server_simulator: Xenops_server_plugin.S)
	  else (module Xenops_server_xen: Xenops_server_plugin.S)));

  Debug.with_thread_associated "main" (start sockets) Server.process;
  Xenops_utils.Scheduler.start ();
  Xenops_server.WorkerPool.start !worker_pool_size;
  while true do
	  Thread.delay 60.
  done


