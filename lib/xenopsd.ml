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

module D = Debug.Make(struct let name = "xenopsd" end)
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
let sockets_path = ref "/var/xapi"
let persist = ref true
let daemon = ref false
let worker_pool_size = ref 4

let config_spec = [
	"sockets-path", Arg.Set_string sockets_path, "Directory to create listening sockets";
    "pidfile", Arg.Set_string pidfile, "Location to store the process pid";
    "persist", Arg.Bool (fun b -> persist := b), "True if we want to persist metadata across restarts";
    "daemon", Arg.Bool (fun b -> daemon := b), "True if we want to daemonize";
    "disable-logging-for", Arg.String
        (fun x ->
            try
                let modules = Re_str.split (Re_str.regexp "[ ]+") x in
                List.iter Debug.disable modules
            with e ->
				error "Processing disabled-logging-for = %s: %s" x (Printexc.to_string e)
        ), "A space-separated list of debug modules to suppress logging from";
    "worker-pool-size", Arg.Set_int worker_pool_size, "Number of threads for the worker pool";
    "database-path", Arg.Set_string Xenops_utils.root, "Location to store the metadata";
	"config", Arg.Set_string config_file, "Location of configuration file";
]

let arg_spec = List.map (fun (a, b, c) -> "-" ^ a, b, c) config_spec

let read_config_file () =
    if Sys.file_exists !config_file then begin
		(* Will raise exception if config is mis-formatted. It's up to the
           caller to inspect and handle the failure.
        *)
        Config.parse_file !config_file config_spec;
		debug "Read global variables successfully from %s" !config_file
    end

let dump_config_file () : unit =
    debug "pidfile = %s" !pidfile;
    debug "persist = %b" !persist;
    debug "daemon = %b" !daemon;
    debug "worker-pool-size = %d" !worker_pool_size;
    debug "database-path = %s" !Xenops_utils.root

let path () = Filename.concat !sockets_path "xenopsd"
let forwarded_path () = path () ^ ".forwarded" (* receive an authenticated fd from xapi *)
let json_path () = path () ^ ".json"

module Server = Xenops_interface.Server(Xenops_server)

(* Normal HTTP POST and GET *)
let http_handler s (context: Xenops_server.context) =
	let ic = Unix.in_channel_of_descr s in
	let oc = Unix.out_channel_of_descr s in
	let module Request = Cohttp.Request.Make(Cohttp_posix_io.Buffered_IO) in
	let module Response = Cohttp.Response.Make(Cohttp_posix_io.Buffered_IO) in
	match Request.read ic with
		| None ->
			debug "Failed to read HTTP request"
		| Some req ->
			begin match Request.meth req, Uri.path (Request.uri req) with
				| `GET, "/" ->
					let response_txt = "<html><body>Hello there</body></html>" in
					let headers = Cohttp.Header.of_list [
						"user-agent", "xenopsd";
						"content-length", string_of_int (String.length response_txt)
					] in
					let response = Response.make ~version:`HTTP_1_1 ~status:`OK ~headers () in
					Response.write (fun t oc -> Response.write_body t oc response_txt) response oc
				| `POST, "/" ->
					begin match Request.header req "content-length" with
						| None ->
							debug "Failed to read content-length"
						| Some content_length ->
							let content_length = int_of_string content_length in
							let request_txt = String.make content_length '\000' in
							really_input ic request_txt 0 content_length;
							let rpc_call = Jsonrpc.call_of_string request_txt in
							let rpc_response = Server.process context rpc_call in
							let response_txt = Jsonrpc.string_of_response rpc_response in
							let headers = Cohttp.Header.of_list [
								"user-agent", "xenopsd";
								"content-length", string_of_int (String.length response_txt)
							] in
							let response = Response.make ~version:`HTTP_1_1 ~status:`OK ~headers () in
							Response.write (fun t oc -> Response.write_body t oc response_txt) response oc
					end
				| _, _ ->
					let headers = Cohttp.Header.of_list [
						"user-agent", "xenopsd";
					] in
					let response = Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers () in
					Response.write (fun t oc -> ()) response oc
			end


(* Apply a binary message framing protocol where the first 16 bytes are an integer length
   stored as an ASCII string *)
let binary_handler s (context: Xenops_server.context) =
	let ic = Unix.in_channel_of_descr s in
	let oc = Unix.out_channel_of_descr s in
	(* Read a 16 byte length encoded as a string *)
	let len_buf = String.make 16 '\000' in
	really_input ic len_buf 0 (String.length len_buf);
	let len = int_of_string len_buf in
	let msg_buf = String.make len '\000' in
	really_input ic msg_buf 0 (String.length msg_buf);
	let (request: Rpc.call) = Jsonrpc.call_of_string msg_buf in
	let (result: Rpc.response) = Server.process context request in
	let msg_buf = Jsonrpc.string_of_response result in
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

let start (domain_sock, forwarded_sock, json_sock)  =
	(* JSON/HTTP over domain_sock, no fd passing *)
	accept_forever domain_sock
		(fun s ->
			let context = { Xenops_server.transferred_fd = None } in
			http_handler s context
		);

	accept_forever forwarded_sock
		(fun this_connection ->
			let msg_size = 16384 in
			let buf = String.make msg_size '\000' in
			debug "Calling recv_fd()";
			let len, _, received_fd = Fd_send_recv.recv_fd this_connection buf 0 msg_size [] in
			debug "recv_fd ok (len = %d)" len;
			finally
				(fun () ->
					let req = String.sub buf 0 len |> Jsonrpc.of_string |> Xenops_migrate.Forwarded_http_request.t_of_rpc in
					debug "Received request = [%s]\n%!" (req |> Xenops_migrate.Forwarded_http_request.rpc_of_t |> Jsonrpc.to_string);
					let expected_prefix = "/service/xenops/memory/" in
					let uri = req.Xenops_migrate.Forwarded_http_request.uri in
					if String.length uri < String.length expected_prefix || (String.sub uri 0 (String.length expected_prefix) <> expected_prefix) then begin
						error "Expected URI prefix %s, got %s" expected_prefix uri;
						let module Response = Cohttp.Response.Make(Cohttp_posix_io.Unbuffered_IO) in
						let headers = Cohttp.Header.of_list [
							"User-agent", "xenopsd"
						] in
						let response = Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers () in
						Response.write (fun _ _ -> ()) response this_connection;
					end else begin
						let context = {
							Xenops_server.transferred_fd = Some received_fd
						} in
						let uri = Uri.of_string req.Xenops_migrate.Forwarded_http_request.uri in
						Xenops_server.VM.receive_memory uri req.Xenops_migrate.Forwarded_http_request.cookie this_connection context
					end
				) (fun () -> Unix.close received_fd)
		);

	(* JSON/binary over json_sock, no fd passing *)
	accept_forever json_sock
		(fun s ->
			let context = { Xenops_server.transferred_fd = None } in
			binary_handler s context
		)

(* Start accepting connections on sockets before we daemonize *)
let prepare_sockets () =
	let path = path () in
    Unixext.mkdir_safe (Filename.dirname path) 0o700;
    Unixext.unlink_safe path;
	let domain_sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind domain_sock (Unix.ADDR_UNIX path);
	Unix.listen domain_sock 5;

	(* Start receiving forwarded /file descriptors/ from xapi *)
	let forwarded_path = forwarded_path () in
	Unixext.unlink_safe forwarded_path;
	let forwarded_sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind forwarded_sock (Unix.ADDR_UNIX forwarded_path);
	Unix.listen forwarded_sock 5;

	(* Start receiving local binary messages *)
	let json_path = json_path () in
	Unixext.unlink_safe json_path;
	let json_sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.bind json_sock (Unix.ADDR_UNIX json_path);
	Unix.listen json_sock 5;

	domain_sock, forwarded_sock, json_sock
