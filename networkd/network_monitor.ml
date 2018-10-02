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
include Network_stats

let write_stats stats =
  let payload = stats |> Rpcmarshal.marshal typ_of_stats_t |> Jsonrpc.to_string in
  let checksum = payload |> Digest.string |> Digest.to_hex in
  let length = String.length payload in
  let data = Printf.sprintf "%s%s%08x%s" magic checksum length payload in
  Xapi_stdext_unix.Unixext.write_string_to_file stats_file (data)
