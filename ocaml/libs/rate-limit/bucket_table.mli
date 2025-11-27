(*
 * Copyright (C) 2025 Cloud Software Group
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

type t = (string, Token_bucket.t) Hashtbl.t

val create : unit -> t

val add_bucket :
  t -> user_agent:string -> burst_size:float -> fill_rate:float -> unit

val delete_bucket : t -> user_agent:string -> unit

val try_consume : t -> string -> float -> bool

val consume_and_block : t -> string -> float -> unit
