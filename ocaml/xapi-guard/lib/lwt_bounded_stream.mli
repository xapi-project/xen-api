(*
 * Copyright (c) 2012 Citrix Systems
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
 *)

(** Similar to Lwt_stream.bounded_push except threads never block in push() *)
type 'a t

val create : int -> 'a t * ('a option -> unit option)
(** [create capacity] creates a stream which can contain at most
    [capacity] elements *)

val get_available : 'a t -> 'a list
(** [get_available t] returns all available elements from [t] without blocking *)

val get : 'a t -> 'a option Lwt.t
(** [get t] returns an element from [t] *)

val nget : int -> 'a t -> 'a list Lwt.t
(** [nget n t] returns [n] elements from [t] *)

val size : 'a t -> int
(** [size t] return the number of enqueued elements *)
