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
type t = {
	sz: int;
	data: string;
	mutable prod: int;
	mutable cons: int;
	mutable pwrap: bool;
}

exception Data_limit
exception Full

val make : int -> t

val to_consume : t -> int
val to_fill : t -> int

val is_full : t -> bool
val is_empty : t -> bool

val consume : t -> int -> string
val consume_all : t -> string
val skip : t -> int -> unit

val feed_data : t -> string -> unit
val read_search : t -> (string -> int -> int -> int)
                    -> (string -> int -> int -> unit) -> int
                    -> int
val search : t -> char -> int
