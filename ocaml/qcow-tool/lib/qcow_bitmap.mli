(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** The type of the set elements *)
type elt = int64

(** An interval: a range (x, y) of set values where all the elements from
    x to y inclusive are in the set *)
type interval

module Interval : sig
  val make : elt -> elt -> interval
  (** [make first last] construct an interval describing all the elements from
      [first] to [last] inclusive. *)

  val x : interval -> elt
  (** the starting element of the interval *)

  val y : interval -> elt
  (** the ending element of the interval *)
end

(** The type of sets *)
type t

val make_empty : initial_size:int -> maximum_size:int -> t
(** [make_empty n] creates a set of [initial_size] which can be resized up to
    [maximum size], initially empty *)

val make_full : initial_size:int -> maximum_size:int -> t
(** [make_full n] creates a set of [initial_size] which can be resized up to
    [maximum size], initially full *)

val copy : t -> t
(** [copy t] returns a duplicate of [t] *)

val fold : (interval -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f t acc] folds [f] across all the intervals in [t] *)

val fold_s : (interval -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t
(** [fold_s f t acc] folds [f] across all the intervals in [t] *)

val add : interval -> t -> unit
(** [add interval t] adds the [interval] to [t] in-place *)

val remove : interval -> t -> unit
(** [remove interval t] removes the [interval] from [t] in-place *)

val min_elt : t -> elt
(** [min_elt t] returns the smallest element, or raises [Not_found] if the set
    is empty. *)

val to_string : t -> string

module Test : sig
  val all : (string * (unit -> unit)) list
end
