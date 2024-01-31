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
(** Ring structures for RRAs
    The values in the structures are bound to a range *)

module BoundedFloat = Rrd_utils.BoundedFloat

type t

val make : int -> float -> float -> float -> t
(** create a ring structure with [size] record; records initialised to [init]
    @param size number of elements the ring holds (constant)
    @param init value all the elements are initialized to
    *)

val copy : t -> t
(** create a duplicate ring structure *)

val length : t -> int
(** length (size) of the ring, it is constant *)

val push : t -> float -> unit
(** push into the ring one element *)

val peek : t -> int -> float
(** get the i{^th} old element from the ring *)

val top : t -> float
(** get the top element of the ring *)

val iter_nb : t -> (float -> unit) -> int -> unit
(** iterate over nb element of the ring, starting from the top *)

val raw_iter : t -> (float -> unit) -> unit

val iter : t -> (float -> unit) -> unit
(** iterate over all elements of the ring, starting from the top *)

val get_nb : t -> int -> float array
(** get array of latest [nb] value *)

val get : t -> float array
(** get an array with all the values in the ring *)
