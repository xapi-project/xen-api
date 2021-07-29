(*
 * Copyright (C) 2020 Citrix Systems Inc.
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

type t = string [@@deriving rpc]

let of_string s = s

let json_rpc_of_t s = Jsonrpc.of_string ~strict:true s

let _pool_secret = "pool_secret"

let with_cookie t request =
  let cookie = [(_pool_secret, t)] in
  {request with Http.Request.cookie}

let write_to_file = Xapi_stdext_unix.Unixext.write_string_to_file

let write_to_channel c s = output_string c s

let equal = String.equal

let of_request req =
  match List.assoc_opt _pool_secret req.Http.Request.cookie with
  | Some _ as r ->
      r
  | None ->
      List.assoc_opt _pool_secret req.Http.Request.query
