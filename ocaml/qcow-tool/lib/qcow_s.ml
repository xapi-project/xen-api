(*
 * Copyright (C) 2015 David Scott <dave@recoil.org>
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

(** Common signatures used by the library *)

open Result

module type LOG = sig
  (** Common logging functions *)

  val debug : ('a, unit, string, unit) format4 -> 'a
  val info  : ('a, unit, string, unit) format4 -> 'a
  val error : ('a, unit, string, unit) format4 -> 'a
end

module type SERIALISABLE = sig
  (** Values which can be read and written *)

  type t
  (** Instances of this type can be read and written *)

  val sizeof: t -> int
  (** The size of a buffer needed to hold [t] *)

  val read: Cstruct.t -> (t * Cstruct.t, [ `Msg of string]) result
  (** Read a [t] from the given buffer and return it, along with the
      unused remainder of the buffer. If the buffer cannot
      be parsed then return an error.*)

  val write: t -> Cstruct.t -> (Cstruct.t, [ `Msg of string]) result
  (** Write a [t] into the given buffer. If the buffer is too small,
      then return an error. Return the unused remainder of the buffer.*)
end

module type PRINTABLE = sig
  (** Values which can be pretty-printed *)

  type t
  (** Instances of this type can be pretty-printed *)

  val to_string: t -> string
  (** Produce a pretty human-readable string from a value *)
end

module type RESIZABLE_BLOCK = sig
  include Mirage_block.S

  val resize: t -> int64 -> (unit, write_error) result Lwt.t
  (** Resize the file to the given number of sectors. *)

  val flush : t -> (unit, write_error) result Lwt.t
  (** [flush t] flushes any buffers, if the file has been opened in buffered
      mode *)
end

module type INTERVAL_SET = sig
  type elt
  (** The type of the set elements *)

  type interval
  (** An interval: a range (x, y) of set values where all the elements from
      x to y inclusive are in the set *)

  module Interval: sig
    val make: elt -> elt -> interval
    (** [make first last] construct an interval describing all the elements from
        [first] to [last] inclusive. *)

    val x: interval -> elt
    (** the starting element of the interval *)

    val y: interval -> elt
    (** the ending element of the interval *)
  end

  type t [@@deriving sexp]
  (** The type of sets *)

  val empty: t
  (** The empty set *)

  val is_empty: t -> bool
  (** Test whether a set is empty or not *)

  val cardinal: t -> elt
  (** [cardinal t] is the number of elements in the set [t] *)

  val mem: elt -> t -> bool
  (** [mem elt t] tests whether [elt] is in set [t] *)

  val fold: (interval -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f t acc] folds [f] across all the intervals in [t] *)

  val fold_s: (interval -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t
  (** [fold_s f t acc] folds [f] across all the intervals in [t] *)

  val fold_individual: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_individual f t acc] folds [f] across all the individual elements of [t] *)

  val add: interval -> t -> t
  (** [add interval t] returns the set consisting of [t] plus [interval] *)

  val remove: interval -> t -> t
  (** [remove interval t] returns the set consisting of [t] minus [interval] *)

  val min_elt: t -> interval
  (** [min_elt t] returns the smallest (in terms of the ordering) interval in
      [t], or raises [Not_found] if the set is empty. *)

  val max_elt: t -> interval
  (** [max_elt t] returns the largest (in terms of the ordering) interval in
      [t], or raises [Not_found] if the set is empty. *)

  val choose: t -> interval
  (** [choose t] returns one interval, or raises Not_found if the set is empty *)

  val take: t -> elt -> (t * t) option
  (** [take n] returns [Some a, b] where [cardinal a = n] and [diff t a = b]
      or [None] if [cardinal t < n] *)

  val union: t -> t -> t
  (** set union *)

  val diff: t -> t -> t
  (** set difference *)

  val inter: t -> t -> t
  (** set intersection *)
end

module type NUM = sig
  type t
  val zero: t
  val pred: t -> t
  val succ: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val of_int64: int64 -> t
  val to_int64: t -> int64
  val of_int: int -> t
  val to_int: t -> int
  val to_string: t -> string
  val shift_left: t -> int -> t
  val shift_right_logical: t -> int -> t
  val logor: t -> t -> t
  val rem: t -> t -> t
end
