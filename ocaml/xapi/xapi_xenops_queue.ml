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

module D=Debug.Make(struct let name="xenops" end)
open D

open Xenops_interface

module type XENOPS = module type of Xenops_client.Client

let queue_override = ref []

let make_client queue_name =
  let module Client = Xenops_interface.XenopsAPI(Idl.Exn.GenClient(struct
      let rpc x =
        if !Xcp_client.use_switch
        then begin
          if List.mem_assoc queue_name !queue_override
          then List.assoc queue_name !queue_override x
          else Xcp_client.json_switch_rpc queue_name x
        end else Xcp_client.http_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string ~srcstr:"xapi" ~dststr:"xenops" Xenops_interface.default_uri x
    end)) in
  (module Client: XENOPS)

let all_known_xenopsds () = !Xapi_globs.xenopsd_queues
let default_xenopsd () = !Xapi_globs.default_xenopsd

let queue_of_other_config oc =
  if List.mem_assoc "xenops" oc then begin
    let queue_name = List.assoc "xenops" oc in
    if List.mem queue_name (all_known_xenopsds ())
    then queue_name
    else begin
      error "Unknown xenops queue: %s, using default %s" queue_name (default_xenopsd ());
      default_xenopsd ()
    end
  end else default_xenopsd ()

let queue_of_vm ~__context ~self = queue_of_other_config (Db.VM.get_other_config ~__context ~self)
