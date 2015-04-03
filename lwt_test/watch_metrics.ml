(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

open Lwt

open Xen_api
open Xen_api_lwt_unix

let uri = ref "http://127.0.0.1/"
let username = ref "root"
let password = ref "password"

let exn_to_string = function
	| Api_errors.Server_error(code, params) ->
		Printf.sprintf "%s %s" code (String.concat " " params)
	| e -> Printexc.to_string e

let main () =
	let rpc = make !uri in
	lwt session_id = Session.login_with_password rpc !username !password "1.0" in
	try_lwt
		lwt hosts = Host.get_all rpc session_id in
		let host = List.hd hosts in

		let open Cohttp_lwt_unix in
		let uri = Xen_api_metrics.Updates.uri
			~host:(Uri.of_string !uri) ~authentication:(`UserPassword(!username, !password))
			~start:0 ~include_host:true () in
		let b = Cohttp.Auth.string_of_credential (`Basic (!username, !password)) in
		let headers = Cohttp.Header.of_list ["authorization", b] in

  	Client.call ~headers `GET uri >>= fun (res, body) ->
  	let headers = Response.headers res in
  	Cohttp.Header.iter
    	(fun k v -> List.iter (Printf.eprintf "%s: %s\n%!" k) v) headers;
		Cohttp_lwt_body.to_string body
		>>= fun s ->
		Printf.eprintf "chunk: %s\n%!" s;
		return ()
	finally
		Session.logout rpc session_id

let _ =
	Arg.parse [
		"-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
		"-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
		"-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Simple example which watches metrics updates from a host";

	Lwt_main.run (main ())
