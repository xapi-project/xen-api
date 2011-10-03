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
(**
 * @group Storage
 *)

let socket = ref "/var/xapi/storage"

let rpc call =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~transport:(Unix !socket) 
		~http:(xmlrpc ~version:"1.0" "/") call

open Storage_interface

let task = "sm-cli"

let usage_and_exit () =
	Printf.fprintf stderr "Usage:\n";
	Printf.fprintf stderr "  %s sr-list\n" Sys.argv.(0);
	exit 1

let _ =
	if Array.length Sys.argv < 2 then usage_and_exit ();
	match Sys.argv.(1) with
		| "sr-list" ->
			let srs = Client.SR.list rpc ~task in
			List.iter
				(fun sr ->
					Printf.printf "%s\n" sr
				) srs
		| x ->
			Printf.fprintf stderr "Unknown command: %s\n" x;
			usage_and_exit ()
