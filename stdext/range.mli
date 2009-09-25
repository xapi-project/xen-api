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
type t

(** Make a range. *)
val make : int -> int -> t

(** Extract the start and end of the given range. *)
val get : t -> int * int

(** Test the given int for membership in the given range. *)
val mem : int -> t -> bool

(** Fold over a range, starting at the smallest int. *)
val fold_left : ('a -> int -> 'a) -> 'a -> t -> 'a

(** Fold over a range, starting at the largest int. *)
val fold_right : (int -> 'a -> 'a) -> t -> 'a -> 'a

(** Convert a range to a list of ascending integers *)
val to_list : t -> int list

