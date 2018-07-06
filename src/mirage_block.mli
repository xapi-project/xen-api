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

(** Block device signatures. *)

open Result

type error = Mirage_device.error
(** The type for IO operation errors. *)

val pp_error: error Fmt.t
(** [pp_error] pretty-prints errors. *)

type write_error = [
  | error
  | `Is_read_only      (** attempted to write to a read-only disk *)
]

val pp_write_error: write_error Fmt.t
(** [pp_write_error] pretty-prints errors. *)

type info = {
  read_write: bool;    (** True if we can write, false if read/only *)
  sector_size: int;    (** Octets per sector *)
  size_sectors: int64; (** Total sectors per device *)
}
(** The type for characteristics of the block device. Note some
    devices may be able to make themselves bigger over time. *)

(** Operations on sector-addressible block devices, usually used for
    persistent storage. *)
module type S = sig

  type page_aligned_buffer
  (** The type for page-aligned memory buffers. *)

  type error = private [> Mirage_device.error]
  (** The type for block errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type write_error = private [> Mirage_device.error | `Is_read_only]
  (** The type for write errors. *)

  val pp_write_error: write_error Fmt.t
  (** [pp_write_error] is the pretty-printer for write errors. *)

  include Mirage_device.S

  val get_info: t -> info io
  (** Query the characteristics of a specific block device *)

  val read: t -> int64 -> page_aligned_buffer list -> (unit, error) result io
  (** [read device sector_start buffers] reads data starting at
      [sector_start] from the block device into [buffers]. [Ok ()]
      means the buffers have been filled.  [Error _] indicates an I/O
      error has happened and some of the buffers may not be filled.
      Each of elements in the list [buffers] must be a whole number of
      sectors in length.  The list of buffers can be of any length. *)

  val write: t -> int64 -> page_aligned_buffer list ->
    (unit, write_error) result io
  (** [write device sector_start buffers] writes data from [buffers]
      onto the block device starting at [sector_start]. [Ok ()] means
      the contents of the buffers have been written. [Error _]
      indicates a partial failure in which some of the writes may not
      have happened.

      Once submitted, it is not possible to cancel a request and there
      is no timeout.

      The operation may fail with:

      {ul

      {- [`Unimplemented]: the operation has not been implemented, no
      data has been written.}
      {- [`Is_read_only]: the device is read-only, no data has been
      written.}
      {- [`Disconnected]: the device has been disconnected at
        application request, an unknown amount of data has been
        written.}
      }

      Each of [buffers] must be a whole number of sectors in
      length. The list of buffers can be of any length.

      The data will not be copied, so the supplied buffers must not be
      re-used until the IO operation completes. *)

end
