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
open Squeezed_rpc
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


(* We assume only one instance of a named service logs in at a time and therefore can use
   the service name as a session_id. *)

(* val login: service -> session_id *)
let login args = 
  if not(List.mem_assoc _service_name args)
  then [ _code, _error_missing_argument_code; _description, _service_name ]
  else begin
    let service_name = List.assoc _service_name args in
    Debug.with_thread_associated (Printf.sprintf "login(%s)" service_name)
      (fun () ->
	 (* remove any existing reservations associated with this service *)
	 with_xc_and_xs
	   (fun xc xs ->
	      xs.Xs.rm (state_path _service ^ "/" ^ service_name)
	   )
      ) ();
    [ _session_id, service_name ]
  end

let handle_memory_errors f = 
  try
    f ()
  with
  | Squeeze.Cannot_free_this_much_memory (needed, free) ->
      (* NB both needed and free have been inflated by the lowmem_emergency_pool etc *)
      let needed = Int64.sub needed Squeeze_xen.target_host_free_mem_kib 
      and free = Int64.sub free Squeeze_xen.target_host_free_mem_kib in
      [ _code, _error_cannot_free_this_much_memory_code; _description, Printf.sprintf "%Ld,%Ld" needed free ]
  | Squeeze.Domains_refused_to_cooperate domids ->
      [ _code, _error_domains_refused_to_cooperate_code; _description, String.concat "," (List.map string_of_int domids) ]

(* val reserve_memory: session_id -> kib -> reservation_id *)
let reserve_memory args = 
  if not(List.mem_assoc _session_id args)
  then [ _code, _error_missing_argument_code; _description, _session_id ]
  else if not(List.mem_assoc _kib args)
  then [ _code, _error_missing_argument_code; _description, _kib ]
  else begin
    let session_id = List.assoc _session_id args 
    and kib = Int64.of_string (List.assoc _kib args) in
    let reservation_id = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    if kib < 0L
    then [ _code, _error_invalid_memory_value; _description, _kib ]
    else
      handle_memory_errors
	(fun () ->
	   Debug.with_thread_associated (Printf.sprintf "reserve_memory(%s, %Ld)" session_id kib)
	     (fun () ->
		with_xc_and_xs
		  (fun xc xs ->
		     Squeeze_xen.free_memory ~xc ~xs kib;
		     debug "reserved %Ld kib for reservation %s" kib reservation_id;
		     add_reservation xs _service session_id reservation_id (Int64.to_string kib)
		  )
	     ) ();
	   [ _reservation_id, reservation_id ]
	)
  end

(* val reserve_memory_range: session_id -> min -> max -> reservation_id *)
let reserve_memory_range args = 
  if not(List.mem_assoc _session_id args)
  then [ _code, _error_missing_argument_code; _description, _session_id ]
  else if not(List.mem_assoc _min args)
  then [ _code, _error_missing_argument_code; _description, _min ]
  else if not(List.mem_assoc _max args)
  then [ _code, _error_missing_argument_code; _description, _max ]
  else begin
    let session_id = List.assoc _session_id args in
    let min = Int64.of_string (List.assoc _min args) in
    let max = Int64.of_string (List.assoc _max args) in
    let reservation_id = Uuid.string_of_uuid (Uuid.make_uuid ()) in
    if min < 0L 
    then [ _code, _error_invalid_memory_value; _description, _min ]
    else 
      if max < 0L
      then [ _code, _error_invalid_memory_value; _description, _max ]
      else 
	if max < min
	then [ _code, _error_invalid_memory_value; _description, _min ]
	else
	  handle_memory_errors
	    (fun () ->
	       Debug.with_thread_associated (Printf.sprintf "reserve_memory_range(%s, %Ld, %Ld)" session_id min max)
		 (fun () ->
		    with_xc_and_xs
		      (fun xc xs ->
			 let amount = Squeeze_xen.free_memory_range ~xc ~xs min max in
			 debug "reserved %Ld kib for reservation %s" amount reservation_id;
			 add_reservation xs _service session_id reservation_id (Int64.to_string amount);
			 [ _kib, Int64.to_string amount; _reservation_id, reservation_id ]
		      )
		 ) ()
	    )
  end
  

(* val delete_reservation: reservation_id -> unit *)
let delete_reservation args = 
  if not(List.mem_assoc _session_id args)
  then [ _code, _error_missing_argument_code; _description, _session_id ]
  else if not(List.mem_assoc _reservation_id args)
  then [ _code, _error_missing_argument_code; _description, _reservation_id ]
  else begin
    let session_id = List.assoc _session_id args 
    and reservation_id = List.assoc _reservation_id args in
    Debug.with_thread_associated (Printf.sprintf "delete_reservation(%s)" reservation_id)
      (fun () ->
	 with_xc_and_xs
	   (fun xc xs ->
	      del_reservation xs _service session_id reservation_id;
	      []
	   )
      ) ()
  end

(* val transfer_reservation_to_domain: session_id -> reservation_id -> domid -> unit *)
let transfer_reservation_to_domain args = 
  if not(List.mem_assoc _session_id args)
  then [ _code, _error_missing_argument_code; _description, _session_id ]
  else if not(List.mem_assoc _reservation_id args)
  then [ _code, _error_missing_argument_code; _description, _reservation_id ]
  else if not(List.mem_assoc _domid args)
  then [ _code, _error_missing_argument_code; _description, _domid ]
  else begin
    let session_id = List.assoc _session_id args 
    and reservation_id = List.assoc _reservation_id args 
    and domid = int_of_string (List.assoc _domid args) in
    try
      Debug.with_thread_associated (Printf.sprintf "transfer_reservation_to_domain(%s, %d)" reservation_id domid)
	(fun () ->
	   with_xc_and_xs
	     (fun xc xs ->
		let kib = xs.Xs.read (reservation_path _service session_id reservation_id) in
		(* This code is single-threaded, no need to make this transactional: *)
		xs.Xs.write (xs.Xs.getdomainpath domid ^ "/memory/initial-reservation") kib;
							maybe
								(fun maxmem -> Squeeze_xen.Domain.set_maxmem_noexn (xc, xs) domid maxmem)
								(try Some (Int64.of_string kib) with _ -> None);
		del_reservation xs _service session_id reservation_id;
		[]
	     )
	) ()
    with Xenbus.Xb.Noent ->
      [ _code, _error_unknown_reservation; _description, reservation_id ]
  end

(* val balance_memory: unit -> unit *)
let balance_memory args = 
  Debug.with_thread_associated "balance"
    (fun () ->
       with_xc_and_xs
	 (fun xc xs ->
	    Squeeze_xen.balance_memory ~xc ~xs
	 )
    ) ();
  []

(* val reopen_logs: unit -> unit *)
let reopen_logs _ = ()

let function_table = [
  "echo", (fun x -> x);
  _login, login;
  _reserve_memory, reserve_memory;
  _reserve_memory_range, reserve_memory_range;
  _transfer_reservation_to_domain, transfer_reservation_to_domain;
  _delete_reservation, delete_reservation;
  _balance_memory, balance_memory;
]

(** Called periodically to look for unbalanced memory and take corrective action *)
let idle_callback ~xc ~xs () = 
  if Squeeze_xen.is_host_memory_unbalanced ~xc ~xs
  then Debug.with_thread_associated "auto-balance" (fun () -> Squeeze_xen.balance_memory ~xc ~xs) ()
  
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
  debug "Writing reserved-host-memory=%Ld KiB" Squeeze_xen.target_host_free_mem_kib;
  let txt = Int64.to_string Squeeze_xen.target_host_free_mem_kib in
  with_xc_and_xs (fun _ xs -> xs.Xs.write (reserved_host_memory_path _service) txt);
  let path = reserved_host_memory_filename _service in
  Unixext.mkdir_rec (Filename.dirname path) 0o755;
  Unixext.write_string_to_file path txt;

	(* Accept connections before we have daemonized *)
	let sockets = prepare_sockets path in

  if !daemonize then Unixext.daemonize ();

  Unixext.mkdir_rec (Filename.dirname !pidfile) 0o755;
  Unixext.pidfile_write !pidfile;

	debug "Starting daemon listening on %s with idle_timeout = %.0f" _service !balance_check_interval;

	let module Server = Memory_interface.Server(Memory_server) in
	Debug.with_thread_associated "main" (start sockets) Server.process;

	while true do
		try
			Thread.delay 60.
		with e ->
			debug "Thread.delay caught: %s" (Printexc.to_string e)
	done


