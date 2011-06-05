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
	let headers = [
		Printf.sprintf "POST %s HTTP/%s" url version;
		Printf.sprintf "User-Agent: xapi/%s" Xapi_globs.api_version_string;
		"Content-Type: text/json";
		Printf.sprintf "Content-length: %d" content_length;
	] in
	Xmlrpcclient.do_http_rpc "" 0 headers ~unixsock:(Some (!path)) request
		(fun content_length _ fd ->
			let buffer = String.make content_length '\000' in
			Unixext.really_read fd buffer 0 content_length;
			buffer)

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

