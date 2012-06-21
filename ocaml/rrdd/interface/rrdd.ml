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

module Rpc : Rrdd_interface.RPC = Rpc_retry.Make(
	struct
		let client_name = "xapi"
		let server_name = Rrdd_interface.name
		let server_path = Filename.concat Fhs.vardir Rrdd_interface.name
		let should_retry = false
	end
)
module Client = Rrdd_interface.Client(Rpc)

(* Make the interface available directly to anyone using this module.*)
include Client
