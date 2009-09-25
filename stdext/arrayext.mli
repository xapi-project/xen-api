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
module Array :
  sig
    external length : 'a array -> int = "%array_length"
    external get : 'a array -> int -> 'a = "%array_safe_get"
    external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
    external make : int -> 'a -> 'a array = "caml_make_vect"
    external create : int -> 'a -> 'a array = "caml_make_vect"
    val init : int -> (int -> 'a) -> 'a array
    val make_matrix : int -> int -> 'a -> 'a array array
    val create_matrix : int -> int -> 'a -> 'a array array
    val append : 'a array -> 'a array -> 'a array
    val concat : 'a array list -> 'a array
    val sub : 'a array -> int -> int -> 'a array
    val copy : 'a array -> 'a array
    val fill : 'a array -> int -> int -> 'a -> unit
    val blit : 'a array -> int -> 'a array -> int -> int -> unit
    val to_list : 'a array -> 'a list
    val of_list : 'a list -> 'a array
    val iter : ('a -> unit) -> 'a array -> unit
    val map : ('a -> 'b) -> 'a array -> 'b array
    val iteri : (int -> 'a -> unit) -> 'a array -> unit
    val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a array -> 'b -> 'b
    val sort : ('a -> 'a -> int) -> 'a array -> unit
    val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
    val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
    external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
    external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

      (** Map a function over a pair of arrays simultaneously. *)
    val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array


      (** Fold a function over a pair of arrays simultaneously. *)
    val fold_left2 :
      ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a

      (** Fold a function over a pair of arrays simultaneously. *)
    val fold_right2 :
      ('a -> 'b -> 'c -> 'c) -> 'a array -> 'b array -> 'c -> 'c

      (** Compute the inner product of two arrays. *)
    val inner :
      (('a -> 'b -> 'c -> 'd) -> 'e -> 'f -> 'g -> 'h) ->
      'e -> ('b -> 'c -> 'i) -> 'f -> 'g -> ('a -> 'i -> 'd) -> 'h
  end
