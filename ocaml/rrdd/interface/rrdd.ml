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

module D = Debug.Debugger(struct let name = "rrdd" end)
open D

(* Establish connection to the server via a file descriptor. *)
let make_rpc path  =
	let open Xmlrpc_client in
	let module Rpc = struct
		let transport = Unix path

		let rpc call =
			let rec aux ~retrying =
				let response' =
					try
						let response =
							XMLRPC_protocol.rpc ~srcstr:"xapi" ~dststr:Rrdd_interface.name ~transport
								~http:(xmlrpc ~version:"1.0" "/") call in
						if retrying then
							debug "Successfully communicated with service at %s after retrying!" path;
						Some response
					with Unix.Unix_error (code, _, _) as e ->
						if code = Unix.ECONNREFUSED || code = Unix.ENOENT then begin
							if not retrying then
								error "Could not reach the service at %s. Retrying every second..." path;
							Thread.delay 1.;
							None
						end else
							raise e
				in
				match response' with
				| Some response -> response
				| None -> aux ~retrying:true
			in aux ~retrying:false
	end in
	(module Rpc : Rrdd_interface.RPC)

module Rpc =
	(val (make_rpc (Filename.concat Fhs.vardir Rrdd_interface.name)) : Rrdd_interface.RPC)

(* Make the RRDD interface available directly to anyone using this module.*)
include Rrdd_interface.Client(Rpc)
