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

let sockets_path = ref Xenops_interface.default_sockets_dir
let persist = ref true
let worker_pool_size = ref 4

let run_hotplug_scripts = ref true
let hotplug_timeout = ref 300.
let qemu_dm_ready_timeout = ref 300.

let watch_queue_length = ref 1000

let options = [
    "sockets-path", Arg.Set_string sockets_path, (fun () -> !sockets_path), "Directory to create listening sockets";
    "persist", Arg.Bool (fun b -> persist := b), (fun () -> string_of_bool !persist), "True if we want to persist metadata across restarts";
    "worker-pool-size", Arg.Set_int worker_pool_size, (fun () -> string_of_int !worker_pool_size), "Number of threads for the worker pool";
    "database-path", Arg.Set_string Xenops_utils.root, (fun () -> !Xenops_utils.root), "Location to store the metadata";
    "run_hotplug_scripts", Arg.Bool (fun x -> run_hotplug_scripts := x), (fun () -> string_of_bool !run_hotplug_scripts), "True if xenopsd should execute the hotplug scripts directly";
    "hotplug_timeout", Arg.Set_float hotplug_timeout, (fun () -> string_of_float !hotplug_timeout), "Time before we assume hotplug scripts have failed";
    "qemu_dm_ready_timeout", Arg.Set_float qemu_dm_ready_timeout, (fun () -> string_of_float !qemu_dm_ready_timeout), "Time before we assume qemu has become stuck";
    "watch_queue_length", Arg.Set_int watch_queue_length, (fun () -> string_of_int !watch_queue_length), "Maximum number of unprocessed xenstore watch events before we restart";
]

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

let raw_fn s =
	let context = { Xenops_server.transferred_fd = None } in
	http_handler s context

let rpc_fn call =
	let context = { Xenops_server.transferred_fd = None } in
	Server.process context call

let handle_received_fd this_connection =
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

let main backend =
	debug "xenopsd version %d.%d starting" major_version minor_version;

	Xcp_service.configure ~options ~resources:Path.resources ();

	(* Listen for arbitrary HTTP *)
	let domain_server = Xcp_service.make_socket_server (path ()) raw_fn in

	(* Listen for transferred file descriptors *)
	let forwarded_server = Xcp_service.make_socket_server (forwarded_path ())
		handle_received_fd in

	(* Listen for regular API calls *)
	let json_server = Xcp_service.make
		~path:(json_path ())
		~queue_name:Xenops_interface.queue_name
		~raw_fn
		~rpc_fn in

	Xcp_service.maybe_daemonize ();

	Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

	Xenops_utils.set_fs_backend
		(Some (if !persist
			then (module Xenops_utils.FileFS: Xenops_utils.FS)
			else (module Xenops_utils.MemFS: Xenops_utils.FS)));

	Xenops_server.register_objects();
	Xenops_server.set_backend (Some backend);

	Debug.with_thread_associated "main"
	(fun () ->
		let (_: Thread.t) = Thread.create (fun () -> Xcp_service.serve_forever domain_server) () in
		let (_: Thread.t) = Thread.create (fun () -> Xcp_service.serve_forever forwarded_server) () in
		let (_: Thread.t) = Thread.create (fun () -> Xcp_service.serve_forever json_server) () in
		()
	) ();
	Scheduler.start ();
	Xenops_server.WorkerPool.start !worker_pool_size;
	while true do
		try
			Thread.delay 60.
		with e ->
			debug "Thread.delay caught: %s" (Printexc.to_string e)
	done

(* Verify the signature matches *)
module S = (Xenops_server_skeleton : Xenops_server_plugin.S)
