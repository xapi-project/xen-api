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

module D = Debug.Make(struct let name = "rpc_retry" end)
open D

open Xmlrpc_client

module type RPC_META =
sig
  val client_name : string
  val server_name : string
  val server_path : string
  val should_retry : bool
end

module Make = functor (Meta : RPC_META) ->
struct
  let transport = Unix Meta.server_path

  let simple_rpc =
    XMLRPC_protocol.rpc ~srcstr:Meta.client_name ~dststr:Meta.server_name
      ~transport ~http:(xmlrpc ~version:"1.0" "/")

  let rpc call =
    let rec aux ~retrying =
      let response' =
        try
          let response = simple_rpc call in
          if retrying then
            debug "Successfully communicated with service at %s after retrying!"
              Meta.server_path;
          Some response
        with Unix.Unix_error (code, _, _) as e ->
          if code = Unix.ECONNREFUSED || code = Unix.ENOENT then begin
            if not retrying then
              error "Could not reach the service at %s. Retrying every second..."
                Meta.server_path;
            Thread.delay 1.;
            None
          end else
            raise e
      in
      match response' with
      | Some response -> response
      | None -> aux ~retrying:true
    in
    if Meta.should_retry then aux ~retrying:false else simple_rpc call
end
