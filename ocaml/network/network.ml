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

open Xmlrpc_client

module D=Debug.Debugger(struct let name="network" end)
open D

let make_rpc path  =
	let module Rpc = struct
		let transport = ref (Unix path)
		let rpc call =
			XMLRPC_protocol.rpc ~transport:!transport ~http:(xmlrpc ~version:"1.0" "/") call
	end in
	(module Rpc : Network_interface.RPC)

module Rpc = (val (make_rpc (Filename.concat Fhs.vardir "xcp-networkd")) : Network_interface.RPC)
module type CLIENT = module type of Network_interface.Client(Rpc)

let get_client () =
	(module Network_interface.Client(Rpc): CLIENT)

