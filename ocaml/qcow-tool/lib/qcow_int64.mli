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
(** Parsers and printers for types used in qcow2 fields *)

open Sexplib

include module type of Int64

val t_of_sexp : Sexp.t -> t

val sexp_of_t : t -> Sexp.t

val of_int64 : int64 -> t

val to_int64 : t -> int64

val round_up : int64 -> int64 -> int64
(** [round_up value to] rounds [value] to the next multiple of [to] *)

val round_down : int64 -> int64 -> int64
(** [round_down value to] rounds [value] down to the multiple of [to] *)

module IntervalSet : Qcow_s.INTERVAL_SET with type elt = t

module Map : Map.S with type key = t

include Qcow_s.SERIALISABLE with type t := t
