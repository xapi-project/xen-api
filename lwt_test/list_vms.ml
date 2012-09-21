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

let server = ref "127.0.0.1"
let username = ref "root"
let password = ref "password"

let main () =
	let addr =
		try
			Unix.inet_addr_of_string !server
		with _ ->
			Printf.fprintf stderr "Failed to parse IP address: %s\n%!" !server;
			exit 1 in
	let connection = Xen_api_lwt_unix.of_sockaddr (Unix.ADDR_INET(addr, 80)) in
	let rpc = Xen_api_lwt_unix.rpc connection in
	lwt session_id =
		try_lwt Client.Session.login_with_password rpc !username !password "1.0"
		with e ->
			Printf.fprintf stderr "login_with_password caught: %s\n%!" (Printexc.to_string e);
			fail e in
	try_lwt
		lwt vms = Client.VM.get_all_records rpc session_id in
		List.iter
			(fun (vm, vm_rec) ->
				Printf.printf "VM %s\n" vm_rec.API.vM_name_label
			) vms;
		return ()
	with
		| Api_errors.Server_error(code, params) as e ->
			Printf.fprintf stderr "%s %s\n%!" code (String.concat " " params);
			raise e
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
