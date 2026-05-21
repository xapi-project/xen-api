(*
 * Copyright (C) 2026 Cloud Software Group
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

(** Doubly linked list ['a t] holding elements of type ['a]. *)

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

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Lexicographic comparison. Terminates on cyclic lists. *)

val equal : 'a t -> 'a t -> bool
(** Structural equality on the sequence of values. Terminates on cyclic lists. *)
