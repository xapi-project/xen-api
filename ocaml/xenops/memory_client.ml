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

open Fun
open Memory_interface
open Xmlrpc_client

module D = Debug.Debugger(struct let name = "memory_client" end)
open D
module E = Debug.Debugger(struct let name = "mscgen" end)

let json_uri = "file:" ^ json_path
let json_url = json_uri |> Http.Url.of_string

(* Use a binary 16-byte length to frame RPC messages *)
let binary_rpc string_of_call response_of_string ?(srcstr="unset") ?(dststr="unset") url (call: Rpc.call) : Rpc.response =
	(* Logging call.Rpc.name is safe, but we must not log the args or the whole
	 * string_of_call since these can sometimes contain sensitive data such as passwords. *)
	E.debug "%s=>%s [label=\"%s\"];" srcstr dststr call.Rpc.name;
	let transport = transport_of_url url in
	with_transport transport
		(fun fd ->
			let msg_buf = string_of_call call in
			let len = Printf.sprintf "%016d" (String.length msg_buf) in
			Unixext.really_write_string fd len;
			Unixext.really_write_string fd msg_buf;
			let len_buf = Unixext.really_read_string fd 16 in
			let len = int_of_string len_buf in
			let msg_buf = Unixext.really_read_string fd len in
			let (response: Rpc.response) = response_of_string msg_buf in
			response
		)

let json_binary_rpc = binary_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

module Client = Memory_interface.Client(struct let rpc = json_binary_rpc ~srcstr:"xenops" ~dststr:"squeezed" json_url end)

