(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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

module D = Debug.Make (struct let name = "http_proxy" end)

open D

let one request fromfd s =
  let open Xapi_stdext_unix in
  (* We can only proxy certain types of request properly *)
  match request.Http.Request.m with
  | Http.Get | Http.Post | Http.Put ->
      (* Set Connection:close if it's not already set *)
      request.Http.Request.close <- true ;
      (* Transmit request headers to master *)
      Unixext.really_write_string s (Http.Request.to_wire_string request) ;
      let limit =
        match request.Http.Request.m with
        | Http.Get ->
            Some 0L
        | _ ->
            request.Http.Request.content_length
      in
      let (_ : int64) = Unixext.copy_file ?limit fromfd s in
      (* Receive response headers from master *)
      let response =
        Option.value ~default:Http.Response.internal_error
          (Http_client.response_of_fd s)
      in
      (* Transmit response headers to client *)
      Unixext.really_write_string fromfd (Http.Response.to_wire_string response) ;
      if response.Http.Response.code = "200" then
        (* If there is a request payload then transmit *)
        let (_ : int64) =
          Unixext.copy_file ?limit:response.Http.Response.content_length s
            fromfd
        in
        ()
  | m ->
      error "Proxy doesn't support: %s" (Http.string_of_method_t m) ;
      Http_svr.response_forbidden ~req:request fromfd
