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
type 'a t = { size : int; mutable current : int; data : 'a array; }
val make : int -> 'a -> 'a t
val length : 'a t -> int
val push : 'a t -> 'a -> unit
val peek : 'a t -> int -> 'a
val top : 'a t -> 'a
val iter_nb : 'a t -> ('a -> 'b) -> int -> unit
val raw_iter : 'a t -> ('a -> unit) -> unit
val iter : 'a t -> ('a -> 'b) -> unit
val get_nb : 'a t -> int -> 'a array
val get : 'a t -> 'a array
