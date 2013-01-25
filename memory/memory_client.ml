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

open Memory_interface

let json_url () = "file:" ^ json_path

let json_binary_rpc = Xcp_client.binary_rpc Jsonrpc.string_of_call Jsonrpc.response_of_string

module Client = Memory_interface.Client(struct let rpc = json_binary_rpc ~srcstr:"xenops" ~dststr:"squeezed" json_url end)

