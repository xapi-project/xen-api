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

(* JSON-RPC Client *)

module D = Debug.Make (struct let name = "jsonrpc_client" end)

open D
open Xapi_stdext_unix

exception Timeout

exception Read_error

let json_rpc_max_len = ref 65536 (* Arbitrary maximum length of RPC response *)

let json_rpc_read_timeout = ref (Mtime.Span.(60 * s) |> Unixext.Timeout.of_span)

(* timeout value in ns when reading RPC response *)

let json_rpc_write_timeout = ref (Mtime.Span.(60 * s) |> Unixext.Timeout.of_span)

(* timeout value in ns when writing RPC request *)

(* Read the entire contents of the fd, of unknown length *)
let timeout_read fd max_wait =
  Unixext.time_limited_single_read fd !json_rpc_max_len
    (Unixext.Timer.start ~timeout:max_wait)

(* Write as many bytes to a file descriptor as possible from data before a given
   clock time. *)
(* Raises Timeout exception if the number of bytes written is less than the
   specified length. *)
(* Writes into the file descriptor at the current cursor position. *)
let timeout_write filedesc total_length data response_time =
  Xapi_stdext_unix.Unixext.Timer.start ~timeout:response_time
  |> Xapi_stdext_unix.Unixext.time_limited_write filedesc total_length data

let with_rpc ?(version = Jsonrpc.V2) ~path ~call () =
  let uri = Uri.of_string (Printf.sprintf "file://%s" path) in
  Open_uri.with_open_uri uri (fun s ->
      Unix.set_nonblock s ;
      let req = Bytes.of_string (Jsonrpc.string_of_call ~version call) in
      timeout_write s (Bytes.length req) req !json_rpc_write_timeout ;
      let res = timeout_read s !json_rpc_read_timeout in
      debug "Response: %s" res ;
      Jsonrpc.response_of_string ~strict:false res
  )
