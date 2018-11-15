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
open Xcp_client

let json_url () = "file:" ^ json_path
let xml_url () = "file:" ^ xml_path

let rpc call =
		if !use_switch
		then json_switch_rpc queue_name call
		else xml_http_rpc ~srcstr:"xenops" ~dststr:"squeezed" xml_url call

module Client = Memory_interface.API(Idl.Exn.GenClient(struct let rpc=rpc end))

