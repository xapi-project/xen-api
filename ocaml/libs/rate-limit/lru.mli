(*
 * This module implements a cache with support for a least-recently-used
 * (LRU) replacement policy. Main features:
 *
 * Implemented with standard OCaml data types, no dependency on outside
 * libararies for the main functionality.
 *
 * Implemented with mutable state. This keeps the implementation compact
 * and efficient but requires thinking about state more.
 *
 * The architecture is: elements are kept in a hash table to look them
 * up based on a key. Additionally they are kept in a doubly linked
 * list. The head of the list is the least recently used element that
 * can be dropped to make room in the cache. When an element is found in
 * the cache it is moved to the tail of the linked list.
 *)

(** A key/value store with a size cap and support to remove the
      least-used element to make room for new entries. *)
type ('k, 'v) t

val create : int -> ('a, 'b) t
(** [create] and empty [LRU] for a given (positive) size. *)

val size : ('a, 'b) t -> int
(** current number of entries *)

val cap : ('a, 'b) t -> int
(** max number of entries (from when LRU was created) *)

val lookup : ('a, 'b) t -> 'a -> 'b option
(** [lookup] an entry. Returns [None] if it is not found. *)

val remove : ('a, 'b) t -> 'a -> unit
(** [remove] an entry based on its key *)

val add : ('a, 'b) t -> 'a -> 'b -> bool
(** [add] a new entry; if the entry already exists the entry is not
    added. The reason is that we want to avoid removing entries without
    clients being able to act on them. Returns [true] if the capacity of
    the cache is exceeded and should be [trim]'ed. *)

val drop_while : ('a, 'b) t -> evict:('a * 'b -> bool -> bool) -> unit
(** [drop_while] drops elements starting in least-recently-used
    while predicate [evict] is true. The predicate receives the key/value
    and a boolean that indicates if the cache is over capacity. If
    [evict] returns true it can perform any finalisation on the value
    before it will be removed by [drop_while]. [drop_while] can be used
    to clean the cache or to remove elements on any criteria.

    The [evict] function must not call any of the functions of this API
    but decide purely on the value it is passed *)

val trim : ('a, 'b) t -> unit
(** [trim] the cache by removing least-used elements until the size does
   not exceed the capacity. No finalisation of elements that are
   removed. Use [drop_while] for custom finalisation. *)

val to_list : ('a, 'b) t -> ('a * 'b) list
(** retrieve all elements as a list. The head of the list is in LRU
    order: the least-used entry is at the head of the list. *)

module LL : sig
  (** Doubly linked list ['a t] holding elements of type ['a].I t is
      only exposed here to facilitate testing. It should not be used by
      outside code *)

  (** doubly linked list; this is a cyclic data structure; don't use [=]
      on it as it may not terminate. *)
  type 'a t

  (** a node in the list. A node can be removed from its list. Don't use
      [=] on [node] values as it may not terminate. *)
  type 'a node

  val create : unit -> 'a t
  (** create an empty list *)

  val node : 'a -> 'a node
  (** create a node to carry a value *)

  val value : 'a node -> 'a
  (** obtain the value from a node *)

  val append : 'a t -> 'a node -> unit
  (** append a node at the end *)

  val drop : 'a t -> 'a node -> unit
  (** [drop t n] a node [n] from list [t]. It is an unchecked error to
      pass a node [n] to [drop] that is not an element of [t] to begin
      with.*)

  val first : 'a t -> 'a node option
  (** first/head node of the list *)

  val last : 'a t -> 'a node option
  (** last/tail node of the list *)

  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** fold from head *)

  val foldr : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** fold from tail *)

  val to_list : 'a t -> 'a list
  (** retrieve all elements from the list *)

  val from_list : 'a list -> 'a t
  (** construct a [t] value from list *)
end

(*
 * Copyright (C) 2023 Cloud Software Group
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
