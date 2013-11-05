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

let main filename =
	Lwt_unix.LargeFile.stat filename >>= fun stats ->
	let virtual_size = stats.Lwt_unix.LargeFile.st_size in
	let rpc = make !uri in
	lwt session_id = Session.login_with_password rpc !username !password "1.0" in
	try_lwt
		lwt pools = Pool.get_all rpc session_id in
		let pool = List.hd pools in
		lwt sr = Pool.get_default_SR rpc session_id pool in
		lwt vdi = VDI.create ~rpc ~session_id ~name_label:"upload_disk" ~name_description:""
		  ~sR:sr ~virtual_size ~_type:`user ~sharable:false ~read_only:false
		  ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
		lwt uuid = VDI.get_uuid rpc session_id vdi in
		Printf.printf "%s\n" uuid;
		return ()
	finally
		Session.logout rpc session_id

let _ =
	let filename = ref "" in
	Arg.parse [
		"-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
		"-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
		"-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
	] (fun x -> match !filename with
	   | "" -> filename := x
	   | _ -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Simple example which uploads a disk image";

	Lwt_main.run (main !filename)
