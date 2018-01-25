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

open Gpumon_interface
open Xcp_client

let xml_url () = "file:" ^ xml_path

let rpc call =
  if !use_switch
  then json_switch_rpc queue_name call
  else xml_http_rpc
      ~srcstr:(get_user_agent ())
      ~dststr:"gpumon"
      xml_url
      call
module Client = RPC_API(Idl.GenClientExnRpc(struct let rpc=rpc end))
include Client
