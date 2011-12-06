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
(** Small executable that sends a reopen-logs XML/RPC message to the licensing daemon *)
 
let socket = Filename.concat Fhs.vardir "v6"

(* RPC function for communication with the v6 daemon *)
let v6rpc xml = 
	let open Xmlrpc_client in
	XML_protocol.rpc ~transport:(Unix socket) ~http:(xmlrpc ~version:"1.0" "/") xml

let _ = 
	try
		let request = XMLRPC.To.methodCall "reopen-logs" [] in
		ignore (v6rpc request)
	with
	| _ -> ()

