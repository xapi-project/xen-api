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
open Printf
open Threadext

(** Start the XML-RPC server. *)
let _ =
	let http_port = ref Xapi_globs.default_cleartext_port in
	Arg.parse ([
		"-log", Arg.String (fun s ->
			if s = "all" then
				Logs.set_default Log.Debug [ "stderr" ]
			else
				Logs.add s [ "stderr" ]),
		        "open a logger to stderr to the argument key name";
		"-http-port", Arg.Set_int http_port, "set http port";
	] @ Debug.args )(fun x -> printf "Warning, ignoring unknown argument: %s" x)
	  "Receive file uploads by HTTP";

	printf "Starting server on port %d\n%!" !http_port;
	let server = Http_svr.Server.empty in
	try
	  Http_svr.add_handler server Put "/upload" (Http_svr.BufIO Fileupload.upload_file);
	  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string Xapi_globs.ips_to_listen_on, !http_port) in
	  let inet_sock = Http_svr.bind sockaddr "inet_rpc" in
	  Http_svr.start server inet_sock;
	  print_endline "Receiving upload requests on:";
	  Printf.printf "http://%s:%d/upload\n" (Helpers.get_main_ip_address ()) !http_port;
	  flush stdout;
	with
	  | exn -> (eprintf "Caught exception: %s\n!"
		      (ExnHelper.string_of_exn exn))
