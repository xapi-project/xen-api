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
(** Ring structures *)
 
type t = {
  size : int;
  mutable current : int;
  data : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

(** create a ring structure with [size] record; records initialised to [init] *)
val make : int -> float -> t

(** create a duplicate ring structure *)
val copy : t -> t

(** length of the ring *)
val length : t -> int

(** push into the ring one element *)
val push : t -> float -> unit

(** get the i{^th} old element from the ring *)
val peek : t -> int -> float

(** get the top element of the ring *)
val top : t -> float

(** iterate over nb element of the ring, starting from the top *)
val iter_nb : t -> (float -> unit) -> int -> unit

val raw_iter : t -> (float -> unit) -> unit

(** iterate over all elements of the ring, starting from the top *)
val iter : t -> (float -> unit) -> unit

(** get array of latest [nb] value *)
val get_nb : t -> int -> float array

val get : t -> float array
