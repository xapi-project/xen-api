(*
 * Copyright (c) Cloud Software Group, Inc
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

(** bounded container for elements of type ['a] *)
type 'a t

val make : int -> 'a t
(** [make capacity] creates a bounded container that can hold at most [capacity] items.
    When [capacity] is exceeded the oldest item will be dropped.
*)

val capacity : _ t -> int
(** [capacity t] is the maximum number of items that [t] can store *)

val dropped : _ t -> int
(** [dropped t] is the number of items dropped because [capacity] was exceeded. *)

val add : 'a t -> 'a -> unit
(** [add t x] adds [x] to [t], dropping the oldest item to make room if
    [capacity] is exceeded. *)

val to_seq : 'a t -> 'a Seq.t
(** [to_seq t] iterates on the container. The container must not be modified
    during iteration. *)

val clear : _ t -> unit
(** [clear ()] removes all items, but doesn't alter [dropped]. *)
