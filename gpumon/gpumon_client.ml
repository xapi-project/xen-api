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

let xml_url () = "file:" ^ Gpumon_interface.xml_path

let rpc call =
  if !Xcp_client.use_switch
  then Xcp_client.json_switch_rpc Gpumon_interface.queue_name call
  else Xcp_client.xml_http_rpc
      ~srcstr:(Xcp_client.get_user_agent ())
      ~dststr:"gpumon"
      xml_url
      call
module Client = Gpumon_interface.RPC_API(Idl.Exn.GenClient(struct let rpc=rpc end))
