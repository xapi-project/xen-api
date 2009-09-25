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
  size : int;
  mutable current : int;
  data : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
}
val make : int -> float -> t
val length : t -> int
val push : t -> float -> unit
val peek : t -> int -> float
val top : t -> float
val iter_nb : t -> (float -> 'a) -> int -> unit
val raw_iter : t -> (float -> 'a) -> unit
val iter : t -> (float -> 'a) -> unit
val get_nb : t -> int -> float array
val get : t -> float array
