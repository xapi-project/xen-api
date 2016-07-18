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

(** Do an JSON-RPC call to a server that is listening on a Unix domain 
 *  socket at the given path. *)
val with_rpc : ?version:Jsonrpc.version -> path:string -> call:Rpc.call -> unit -> Rpc.response

(** Read an entire JSON object from an input channel. *)
val input_json_object : in_channel -> string
