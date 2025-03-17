(*
 * Copyright (C) 2016 David Scott <dave.scott@unikernel.com>
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
open Result

module Lwt_error: sig
  module Infix : sig
    val ( >>= ) :
      ('a, [> `Disconnected | `Unimplemented ]) result Lwt.t ->
      ('a -> 'b Lwt.t) -> 'b Lwt.t
  end
end

module Lwt_write_error: sig
  module Infix : sig
    val ( >>= ) :
      ('a, [> `Is_read_only | `Disconnected | `Unimplemented ]) result Lwt.t ->
      ('a -> 'b Lwt.t) -> 'b Lwt.t
  end
end

module Infix: sig
  val ( >>= ) : ('a, 'b) result Lwt.t ->
    ('a -> ('c, 'b) result Lwt.t) -> ('c, 'b) result Lwt.t

end

module FromResult: sig
  val ( >>= ) :
             ('a, 'b) result ->
             ('a -> ('c, 'b) result Lwt.t) -> ('c, 'b) result Lwt.t
end
