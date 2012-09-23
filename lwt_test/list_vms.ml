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
	lwt rpc = make !uri in
	lwt session_id = Session.login_with_password rpc !username !password "1.0" in
	try_lwt
		lwt vms = VM.get_all_records rpc session_id in
		List.iter
			(fun (vm, vm_rec) ->
				Printf.printf "VM %s\n" vm_rec.API.vM_name_label
			) vms;
		return ()
	finally
		Session.logout rpc session_id

let _ =
	Arg.parse [
		"-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
		"-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
		"-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Simple example which lists VMs found on a pool";

	Lwt_main.run (main ())
