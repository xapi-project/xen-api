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

(* Establish connection to the server via a file descriptor. *)
let make_rpc path  =
	let open Xmlrpc_client in
	let module Rpc = struct
		let transport = ref (Unix path)
		let rpc call =
			XMLRPC_protocol.rpc ~transport:!transport ~http:(xmlrpc ~version:"1.0" "/") call
	end in
	(module Rpc : Rrdd_interface.RPC)
module Rpc =
	(val (make_rpc (Filename.concat Fhs.vardir Rrdd_interface.name)) : Rrdd_interface.RPC)
module Client = Rrdd_interface.Client(Rpc)

(* Make an API call. *)
let _ =
	let open Printf in
	printf "CLIENT: Begin.\n";
	printf "CLIENT: Calling SERVER's hello with \"Foo\"...\n";
	let _ = Client.get_uncooperative_domains () in
	printf "CLIENT: SERVER replied.\n";
	printf "CLIENT: End.\n"

