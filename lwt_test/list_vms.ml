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

module Client = Client.ClientF(Lwt)
open Lwt

open Xen_api
open Xen_api_lwt_unix

let server = ref "127.0.0.1"
let username = ref "root"
let password = ref "password"

let exn_to_string = function
	| Api_errors.Server_error(code, params) ->
		Printf.sprintf "%s %s" code (String.concat " " params)
	| e -> Printexc.to_string e

let main () =
	let addr =
		try
			Unix.inet_addr_of_string !server
		with _ ->
			Printf.fprintf stderr "Failed to parse IP address: %s\n%!" !server;
			exit 1 in
	let connection = Xen_api_lwt_unix.make (Unix.ADDR_INET(addr, 80)) in
	let rpc x =
		lwt result = Xen_api_lwt_unix.rpc connection x in
		match result with
			| Ok x -> return x
			| Error e ->
				Printf.fprintf stderr "Caught: %s\n%!" (exn_to_string e);
				fail e in
	lwt session_id = Client.Session.login_with_password rpc !username !password "1.0" in
	try_lwt
		lwt vms = Client.VM.get_all_records rpc session_id in
		List.iter
			(fun (vm, vm_rec) ->
				Printf.printf "VM %s\n" vm_rec.API.vM_name_label
			) vms;
		return ()
	finally
		Client.Session.logout rpc session_id

let _ =
	Arg.parse [
		"-s", Arg.Set_string server, (Printf.sprintf "Address of server to connect to (default %s)" !server);
		"-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
		"-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Simple example which lists VMs found on a pool";

	Lwt_main.run (main ())
