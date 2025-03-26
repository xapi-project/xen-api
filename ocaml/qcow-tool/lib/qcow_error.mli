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

(** Common error reporting functions *)

open Result

type error =
  [`Msg of string  (** A fatal error condition; the string should be logged *)]

type 'a t = ('a, error) result

val return : 'a -> ('a, error) result

val error_msg :
  ('a, unit, string, ('b, [> `Msg of string]) result) format4 -> 'a

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

val any : (unit, 'b) result list -> (unit, 'b) result

module Lwt_error : sig
  module Infix : sig
    val ( >>= ) :
         ('a, [< `Disconnected | `Msg of 'b]) result Lwt.t
      -> ('a -> ('c, ([> `Disconnected | `Msg of 'b] as 'd)) result Lwt.t)
      -> ('c, 'd) result Lwt.t
  end

  val or_fail_with :
    ('a, [< `Disconnected | `Msg of string]) result Lwt.t -> 'a Lwt.t

  module List : sig
    val map_p :
         ('a -> ('b, 'error) result Lwt.t)
      -> 'a list
      -> ('b list, 'error) result Lwt.t
    (** [map_p f xs] computes [f x] where [x \in xs] concurrently and returns
        a list of successful results or the first error encountered. All threads
        will have terminated by the time the function returns. *)
  end
end

module Lwt_write_error : sig
  module Infix : sig
    val ( >>= ) :
         ('a, [< `Disconnected | `Is_read_only | `Msg of 'b]) result Lwt.t
      -> (   'a
          -> ('c, ([> `Disconnected | `Is_read_only | `Msg of 'b] as 'd)) result
             Lwt.t
         )
      -> ('c, 'd) result Lwt.t
  end

  val or_fail_with :
       ('a, [< `Disconnected | `Is_read_only | `Msg of string]) result Lwt.t
    -> 'a Lwt.t
end

exception Duplicate_reference of (int64 * int) * (int64 * int) * int64
