(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

val to_string : Rpc.t -> string
val of_string : ?callback:Rpc.callback -> string -> Rpc.t

val to_a : empty:(unit -> 'a) -> append:('a -> string -> unit) -> Rpc.t -> 'a
val of_a : ?callback:Rpc.callback -> next_char:('a  -> char) -> 'a -> Rpc.t

val string_of_call: Rpc.call -> string
val call_of_string: ?callback:Rpc.callback -> string -> Rpc.call

val string_of_response: Rpc.response -> string
val a_of_response : empty:(unit -> 'a) -> append:('a -> string -> unit) -> Rpc.response -> 'a

val response_of_string: ?callback:Rpc.callback -> string -> Rpc.response
val response_of_in_channel: ?callback:Rpc.callback -> in_channel -> Rpc.response
