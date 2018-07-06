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
module List :
sig
  module Monad : sig include Xapi_stdext_monadic.Monad.M1.MONAD with type 'a m = 'a list end
  val setify : 'a list -> 'a list
  val subset : 'a list -> 'a list -> bool
  val set_equiv : 'a list -> 'a list -> bool
  val length : 'a list -> int
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val rev : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val flatten : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val map : ('a -> 'b) -> 'a list -> 'b list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val fold_right2 :
    ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a list -> bool
  val exists : ('a -> bool) -> 'a list -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val mem : 'a -> 'a list -> bool
  val memq : 'a -> 'a list -> bool
  val find : ('a -> bool) -> 'a list -> 'a
  val filter : ('a -> bool) -> 'a list -> 'a list
  val find_all : ('a -> bool) -> 'a list -> 'a list
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
  val assoc : 'a -> ('a * 'b) list -> 'b
  val assq : 'a -> ('a * 'b) list -> 'b
  val mem_assoc : 'a -> ('a * 'b) list -> bool
  val mem_assq : 'a -> ('a * 'b) list -> bool
  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  val split : ('a * 'b) list -> 'a list * 'b list
  val combine : 'a list -> 'b list -> ('a * 'b) list
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list

  (** Perform a lookup on an association list of (value, key) pairs. *)
  val inv_assoc : 'a -> ('b * 'a) list -> 'b

  (** A tail-recursive map. *)
  val map_tr : ('a -> 'b) -> 'a list -> 'b list

  (** Count the number of list elements matching the given predicate. *)
  val count : ('a -> bool) -> 'a list -> int

  (** Find the indices of all elements matching the given predicate. *)
  val position : ('a -> bool) -> 'a list -> int list

  (** Map the given function over a list, supplying the integer
      	    index as well as the element value. *)
  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

  val iteri : (int -> 'a -> unit) -> 'a list -> unit

  val iteri_right : (int -> 'a -> unit) -> 'a list -> unit

  (** Map the given function over a list in reverse order. *)
  val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

  (** Tail-recursive [mapi]. *)
  val mapi_tr : (int -> 'a -> 'b) -> 'a list -> 'b list

  (** Split a list at the given index to give a pair of lists. *)
  val chop : int -> 'a list -> 'a list * 'a list

  (** Split a list at the given index to give a pair of lists, the first in
      		  reverse order. *)
  val rev_chop : int -> 'a list -> 'a list * 'a list

  (** Tail-recursive [chop]. *)
  val chop_tr : int -> 'a list -> 'a list * 'a list

  (** Split a list into lists with the given number of elements. *)
  val dice : int -> 'a list -> 'a list list

  (** Extract the sub-list between the given indices. *)
  val sub : int -> int -> 'a list -> 'a list

  (** Remove the element at the given index. *)
  val remove : int -> 'a list -> 'a list

  (** Extract the element at the given index, returning the element and the
      		list without that element. *)
  val extract : int -> 'a list -> 'a * 'a list

  (** Insert the given element at the given index. *)
  val insert : int -> 'a -> 'a list -> 'a list

  (** Replace the element at the given index with the given value. *)
  val replace : int -> 'a -> 'a list -> 'a list

  (** Apply the given function to the element at the given index. *)
  val morph : int -> ('a -> 'a) -> 'a list -> 'a list

  (** Insert the element [e] between every pair of adjacent elements in the
      	    given list. *)
  val between : 'a -> 'a list -> 'a list

  (** Tail-recursive [between]. *)
  val between_tr : 'a -> 'a list -> 'a list

  (** Generate a random permutation of the given list. *)
  val randomize : 'a list -> 'a list

  (** Distribute the given element over the given list, returning a list of
      	    lists with the new element in each position. *)
  val distribute : 'a -> 'a list -> 'a list list

  (** Generate all permutations of the given list. *)
  val permute : 'a list -> 'a list list

  (** Run-length encode the given list using the given equality function. *)
  val rle_eq : ('a -> 'a -> bool) -> 'a list -> ('a * int) list

  (** Run-length encode the given list using built-in equality. *)
  val rle : 'a list -> ('a * int) list

  (** Decode a run-length encoded list. *)
  val unrle : (int * 'a) list -> 'a list

  (** Compute the inner product of two lists. *)
  val inner :
    (('a -> 'b -> 'c -> 'd) -> 'e -> 'f -> 'g -> 'h) ->
    'e -> ('b -> 'c -> 'i) -> 'f -> 'g -> ('a -> 'i -> 'd) -> 'h

  (** Applies a function f that generates optional values, to each
      	    of the items in a list A [a1; ...; am], generating a new list of
      	    non-optional values B [b1; ...; bn], with m >= n. For each value
      	    a in list A, list B contains a corresponding value b if and only
      	    if the application of (f a) results in Some b.  *)
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  (** Returns true if and only if the given list is in sorted order
      	    according to the given comparison function.  *)
  val is_sorted : ('a -> 'a -> int) -> 'a list -> bool

  (** Returns the intersection of two lists. *)
  val intersect : 'a list -> 'a list -> 'a list

  (** Returns the set difference of two lists *)
  val set_difference : 'a list -> 'a list -> 'a list

  (** Act as List.assoc, but return the given default value if the
      	    key is not in the list. *)
  val assoc_default : 'a -> ('a * 'b) list -> 'b -> 'b

  (** [map_assoc_with_key op al] transforms every value in [al] based on the
      	    key and the value using [op]. *)
  val map_assoc_with_key : ('k -> 'v1 -> 'v2) -> ('k * 'v1) list -> ('k * 'v2) list

  (* Like Lisp cons*)
  val cons : 'a -> 'a list -> 'a list

  (** [take n list] returns the first [n] elements of [list] (or less if list
      	    is shorter).*)
  val take : int -> 'a list -> 'a list

  val tails : 'a list -> ('a list) list
  val safe_hd : 'a list -> 'a option

  (** Replace the value belonging to a key in an association list. Adds the key/value pair
      	 *  if it does not yet exist in the list. If the same key occurs multiple time in the original
      	 *  list, all occurances are removed and replaced by a single new key/value pair.
      	 *  This function is useful is the assoc list is used as a lightweight map/hashtable/dictonary. *)
  val replace_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

  (** Includes everything from [update] and all key/value pairs from [existing] for
      	 *  which the key does not exist in [update]. In other words, it is like [replace_assoc]
      	 *  but then given a whole assoc list of updates rather than a single key/value pair. *)
  val update_assoc : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list

  val make_assoc : ('a -> 'b) -> 'a list -> ('a * 'b) list

  (** Unbox all values from the option list. *)
  val unbox_list : 'a option list -> 'a list

  (** [restrict_with_default default keys al] makes a new association map
      	    from [keys] to previous values for [keys] in [al]. If a key is not found
      	    in [al], the [default] is used. *)
  val restrict_with_default : 'v -> 'k list -> ('k * 'v) list -> ('k * 'v) list

  (** range lower upper = [lower; lower + 1; ...; upper - 1]
      	    Returns the empty list if lower >= upper. *)
  val range : int -> int -> int list
end
