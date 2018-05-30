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

exception Timeout
exception Read_error

val json_rpc_max_len : int ref
val json_rpc_read_timeout : int64 ref
val json_rpc_write_timeout : int64 ref

val timeout_read : Unix.file_descr -> int64 -> string
(** Do an JSON-RPC call to a server that is listening on a Unix domain 
 *  socket at the given path. *)
val with_rpc : ?version:Jsonrpc.version -> path:string -> call:Rpc.call -> unit -> Rpc.response

