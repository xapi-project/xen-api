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

open Varstore_privileged_interface

(* daemon only listens on message-switch *)
let rpc call = Xcp_client.json_switch_rpc ~timeout:20 queue_name call

module Client = RPC_API (Idl.Exn.GenClient (struct
  let rpc = rpc
end))
