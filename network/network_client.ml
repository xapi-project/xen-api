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

open Network_interface
open Xcp_client

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

module Client = Network_interface.Client(struct
    let rpc call =
      retry_econnrefused
        (fun () ->
           if !use_switch
           then json_switch_rpc !queue_name call
           else xml_http_rpc
               ~srcstr:(Xcp_client.get_user_agent ())
               ~dststr:"network"
               Network_interface.uri
               call
        )
  end)
