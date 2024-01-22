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
module List : sig
  (** {1 Comparison} *)

  val is_sorted : ('a -> 'a -> int) -> 'a list -> bool
  (** [is_sorted cmp l] returns whether [l] is sorted according to [cmp]. *)

  (** {1 Iterators} *)

  val take : int -> 'a list -> 'a list
  (** [take n list] returns the first [n] elements of [list] (or less if list
      is shorter).*)

  val drop : int -> 'a list -> 'a list
  (** [drop n list] returns the list without the first [n] elements of [list]
      (or [] if list is shorter). *)

  val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  (** [rev_map f l] gives the same result as {!Stdlib.List.rev}[ (]
      {!Stdlib.List.mapi}[ f l)], but is tail-recursive and more efficient. *)

  val map_tr : ('a -> 'b) -> 'a list -> 'b list
  (** [map_tr f l] is {!Stdlib.List.rev}[ (]{!Stdlib.List.rev_map}[ f l)]. *)

  val mapi_tr : (int -> 'a -> 'b) -> 'a list -> 'b list
  (** [mapi_tr f l] is {!Stdlib.List.rev}[ (]{!rev_mapi}[ f l)]. *)

  val unbox_list : 'a option list -> 'a list
  (** Unbox all values from the option list. *)

  val count : ('a -> bool) -> 'a list -> int
  (** Count the number of list elements matching the given predicate. *)

  val position : ('a -> bool) -> 'a list -> int list
  (** Find the indices of all elements matching the given predicate. *)

  val iteri_right : (int -> 'a -> unit) -> 'a list -> unit
  (** [iteri_right f l] is {!Stdlib.List.iteri}[ f (]{!Stdlib.List.rev}[ l)] *)

  (** {1 List searching} *)

  val find_minimum : ('a -> 'a -> int) -> 'a list -> 'a option
  (** [find_minimum cmp l] returns the lowest element in [l] according to
      the sort order of [cmp], or [None] if the list is empty. When two ore
      more elements match the lowest value, the left-most is returned. *)

  (** {1 Using indices to manipulate lists} *)

  val chop : int -> 'a list -> 'a list * 'a list
  (** [chop k l] splits [l] at index [k] to return a pair of lists. Raises
      invalid_arg when [i] is negative or greater than the length of [l]. *)

  val rev_chop : int -> 'a list -> 'a list * 'a list
  (** [rev_chop k l] splits [l] at index [k] to return a pair of lists, the
      first in reverse order. Raises invalid_arg when [i] is negative or
      greater than the length of [l]. *)

  val chop_tr : int -> 'a list -> 'a list * 'a list
  (** Tail-recursive {!chop}. *)

  val dice : int -> 'a list -> 'a list list
  (** [dice k l] splits [l] into lists with [k] elements each. Raises
      {!Invalid_arg} if [List.length l] is not divisible by [k]. *)

  val sub : int -> int -> 'a list -> 'a list
  (** [sub from to l] returns the sub-list of [l] that starts at index [from]
      and ends at [to] or an empty list if [to] is equal or less than [from].
      Negative indices are treated as 0 and indeces higher than [List.length l
      - 1] are treated as [List.length l - 1]. *)

  val remove : int -> 'a list -> 'a list
  (** Remove the element at the given index. *)

  val insert : int -> 'a -> 'a list -> 'a list
  (** Insert the given element at the given index. *)

  val replace : int -> 'a -> 'a list -> 'a list
  (** Replace the element at the given index with the given value. *)

  val morph : int -> ('a -> 'a) -> 'a list -> 'a list
  (** Apply the given function to the element at the given index. *)

  (** {1 Association Lists} *)

  val make_assoc : ('a -> 'b) -> 'a list -> ('a * 'b) list

  val assoc_default : 'a -> ('a * 'b) list -> 'b -> 'b
  (** Act as List.assoc, but return the given default value if the
      key is not in the list. *)

  val replace_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
  (** Replace the value belonging to a key in an association list. Adds the key/value pair
      if it does not yet exist in the list. If the same key occurs multiple time in the original
      list, all occurances are removed and replaced by a single new key/value pair.
      This function is useful is the assoc list is used as a lightweight map/hashtable/dictonary. *)

  val update_assoc : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list
  (** Includes everything from [update] and all key/value pairs from [existing] for
      which the key does not exist in [update]. In other words, it is like [replace_assoc]
      but then given a whole assoc list of updates rather than a single key/value pair. *)

  val map_assoc_with_key :
    ('k -> 'v1 -> 'v2) -> ('k * 'v1) list -> ('k * 'v2) list
  (** [map_assoc_with_key op al] transforms every value in [al] based on the
      key and the value using [op]. *)

  val inv_assoc : 'a -> ('b * 'a) list -> 'b
  (** Perform a lookup on an association list of (value, key) pairs. *)

  val restrict_with_default : 'v -> 'k list -> ('k * 'v) list -> ('k * 'v) list
  (** [restrict_with_default default keys al] makes a new association map
      from [keys] to previous values for [keys] in [al]. If a key is not found
      in [al], the [default] is used. *)

  (** {1 Run-length encoded lists}
      There are no known users of these functions. *)

  (** {1 Generative functions}
      These are usually useful for coding challenges like Advent of Code.*)

  val range : int -> int -> int list
  (** range lower upper = [lower; lower + 1; ...; upper - 1]
      Returns the empty list if lower >= upper.
      Consider building an {!Stdlib.Seq}, it's more flexible *)

  val between : 'a -> 'a list -> 'a list
  (** [between e l] Intersperses [e] between elements of [l]. *)

  val between_tr : 'a -> 'a list -> 'a list
  (** Tail-recursive {!between}. *)

  val inner :
       (('a -> 'b -> 'c -> 'd) -> 'e -> 'f -> 'g -> 'h)
    -> 'e
    -> ('b -> 'c -> 'i)
    -> 'f
    -> 'g
    -> ('a -> 'i -> 'd)
    -> 'h
  (** Compute the inner product of two lists. *)

  val tails : 'a list -> 'a list list

  (** {1 Lists as sets, avoid}
      Please use Set.Make instead, these functions have quadratic costs! *)

  val setify : 'a list -> 'a list
  (** [setify a] removes all duplicates from [a] while maintaining order.
      Please use [List.sort_uniq] instead to deduplicate lists if possible *)

  val subset : 'a list -> 'a list -> bool
  (** [subset a b] returns whether all elements in [b] can be found in [a]*)

  val set_equiv : 'a list -> 'a list -> bool

  val set_difference : 'a list -> 'a list -> 'a list
  (** Returns the set difference of two lists *)

  val intersect : 'a list -> 'a list -> 'a list
  (** Returns the intersection of two lists. *)
end
