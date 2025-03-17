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

module type ELT = sig
  type t [@@deriving sexp]
  (** The type of the set elements. *)

  include Set.OrderedType with type t := t

  val zero: t
  (** The zeroth element *)

  val pred: t -> t
  (** Predecessor of an element *)

  val succ: t -> t
  (** Successor of an element *)

  val sub: t -> t -> t
  (** [sub a b] returns [a] - [b] *)

  val add: t -> t -> t
  (** [add a b] returns [a] + [b] *)
end

module Make(Elt: ELT): Qcow_s.INTERVAL_SET with type elt = Elt.t

module Test: sig
  val all: (string * (unit -> unit)) list
end
