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

module type RPC_META =
	sig
		val client_name : string
		val server_name : string
		val server_path : string
		val should_retry : bool
	end

module Make : functor (Meta : RPC_META) ->
	sig
		val transport : Xmlrpc_client.transport
		val rpc : Rpc.call -> Rpc.response
	end
