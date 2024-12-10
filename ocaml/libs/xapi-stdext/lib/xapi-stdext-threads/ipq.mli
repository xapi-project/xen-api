(*
 * Copyright (C) 2024 Cloud Software Group
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

type 'a event = {ev: 'a; time: Mtime.span}

type 'a t

exception EmptyHeap

val create : int -> 'a -> 'a t
(** [create n default] creates an empty Imperative priority queue.
   The queue initially is initialized to store [n] elements.
   The queue will expand beyond [n] automatically if needed.
   [default] value will the used to fill unused data. *)

val is_empty : 'a t -> bool
(** Check if the queue is empty *)

val add : 'a t -> 'a event -> unit
(** Add an event to the queue *)

val remove : 'a t -> int -> unit
(** Remove an event from the queue passing the index.
   @raise EmptyHeap if the queue is empty.
   @raise Invalid_argument if the index is invalid. *)

val find_p : 'a t -> ('a -> bool) -> int
(** Find the index of an event which matches a given condition
   or -1 if not found *)

val find : 'a t -> 'a -> int
(** Find the index of an event which matches a given event
   or -1 if not found *)

val maximum : 'a t -> 'a event
(** Return a copy of the event with the next time.
   @raise EmptyHeap if the queue is empty. *)

val pop_maximum : 'a t -> 'a event
(** Return and remove the event with the next time.
   @raise EmptyHeap if the queue is empty. *)

val iter : ('a event -> unit) -> 'a t -> unit
(** Iterate given function on the list of events in the queue *)

val check : 'a t -> unit
(** Check internal queue state, used for debugging *)
