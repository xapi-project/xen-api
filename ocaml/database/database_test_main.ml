(*
 * Copyright (C) 2010 Citrix Systems Inc.
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

open Database_test

let path = ref "./database"

let rpc_common url content_type request = 
	let version = "1.1" in
	let content_length = String.length request in
	let request = Http.Request.make ~version ~content_type:"text/json"
		~user_agent:"database_test"
		~length:(Int64.of_int content_length) Http.Post url in
	let open Xmlrpc_client in
	with_transport (Unix !path)
		(with_http request
			(fun (response, fd) ->
				match response.Http.Response.content_length with
					| None -> failwith "Need a content-length"
					| Some l -> Db_interface.String
						  (Unixext.really_read_string fd (Int64.to_int l))
			)
		)

module Client_v1 = Db_rpc_client_v1.Make(struct
	let initialise () = ()
	let rpc request = rpc_common "/post_remote_db_access" "text/xml" request
end)

module Client_v2 = Db_rpc_client_v2.Make(struct
	let initialise () = ()
	let rpc request = rpc_common "/post_remote_db_access_v2" "text/json" request
end)

module T = Tests(Client_v2)


let _ = 
	Printexc.record_backtrace true;
	Arg.parse [ 
		"--connect-to", Arg.Set_string path, Printf.sprintf "connect to server on path (default %s)" !path;
		] (fun x -> Printf.fprintf stderr "Ignoring unknown parameter: %s\n%!" x)
		"query a database server";
	ignore(T.main true)

