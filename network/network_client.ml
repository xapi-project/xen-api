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

let retry_econnrefused f =
  let rec loop () =
    let result =
      try
        Some (f ())
      with Unix.Unix_error((Unix.ECONNREFUSED | Unix.ENOENT), _, _) ->
        Thread.delay 1.;
        None in
    match result with
    | Some x -> x
    | None -> loop () in
  loop ()

let rpc call =
  retry_econnrefused
    (fun () ->
       if !Xcp_client.use_switch
       then Xcp_client.json_switch_rpc !Network_interface.queue_name call
       else Xcp_client.xml_http_rpc
           ~srcstr:(Xcp_client.get_user_agent ())
           ~dststr:"network"
           Network_interface.uri
           call
    )

module Client = Network_interface.Interface_API(Idl.GenClientExnRpc(struct let rpc=rpc end))
