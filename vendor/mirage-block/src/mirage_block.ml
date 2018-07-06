(*
 * Copyright (C) 2015-present David Scott <dave.scott@unikernel.com>
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

type error = Mirage_device.error

type write_error = [error | `Is_read_only]

let pp_error = Mirage_device.pp_error

let pp_write_error ppf = function
  | #Mirage_device.error as e -> Mirage_device.pp_error ppf e
  | `Is_read_only -> Fmt.pf ppf "attempted to write to a read-only disk"

type info = {
  read_write: bool;    (** True if we can write, false if read/only *)
  sector_size: int;    (** Octets per sector *)
  size_sectors: int64; (** Total sectors per device *)
}

module type S = sig
  type page_aligned_buffer
  type error = private [> Mirage_device.error]
  val pp_error: error Fmt.t
  type write_error = private [> Mirage_device.error | `Is_read_only]
  val pp_write_error: write_error Fmt.t
  include Mirage_device.S
  val get_info: t -> info io
  val read: t -> int64 -> page_aligned_buffer list -> (unit, error) result io
  val write: t -> int64 -> page_aligned_buffer list ->
    (unit, write_error) result io
end
