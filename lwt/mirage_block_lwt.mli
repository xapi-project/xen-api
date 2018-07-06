(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
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

(** Block device implementatins using lwt *)

open Result

module type S = Mirage_block.S
  with type 'a io = 'a Lwt.t
   and type page_aligned_buffer = Cstruct.t

module type SEEKABLE = sig
  include S

  val seek_unmapped: t -> int64 -> (int64, error) result io
  (** [seek_unmapped t start] returns the sector offset of the next
      guaranteed zero-filled region (typically guaranteed because it
      is unmapped) *)

  val seek_mapped: t -> int64 -> (int64, error) result io
  (** [seek_mapped t start] returns the sector offset of the next
      regoin of the device which may have data in it (typically this
      is the next mapped region) *)
end

module type RESIZABLE = sig
  include S

  val resize : t -> int64 -> (unit, write_error) result io
  (** [resize t new_size_sectors] attempts to resize the connected device
      to have the given number of sectors. If successful, subsequent calls
      to [get_info] will reflect the new size. *)
end

(** Utility functions over Mirage [BLOCK] devices *)

(** {1 Compare blocks} *)
module Compare (A: S) (B: S): sig

  type error = [`A of A.error | `B of B.error]
  (** The type for compare errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for compare errors. *)

  val v: A.t -> B.t -> (int, error) result Lwt.t
  (** Compare the contents of two block devices. *)

end

(** {1 Fold over the bytes of a block device} *)
module Fold (A: S): sig

  val s:
    f:('a -> int64 -> Cstruct.t -> 'a Lwt.t) -> 'a -> A.t ->
    ('a , A.error) result Lwt.t
  (** Folds [f] across blocks read sequentially from a block device *)

end

(** {1 Fast Fold over the bytes of a block device}

    This functor use seek operations to perform fast folds over block
    devices.  *)
module Fast_fold (A: SEEKABLE): sig

  val mapped_s:
    f:('a -> int64 -> Cstruct.t -> 'a Lwt.t) -> 'a -> A.t ->
    ('a, A.error) result Lwt.t
  (** Folds [f] across data blocks read sequentially from a block
      device.  In contrast to [fold_s], [Fold.mapped_s] will use
      knowledge about the underlying disk structure and will skip
      blocks which it knows contain only zeroes. Note it may still
      read blocks containing zeroes. The function [f] receives an
      accumulator, the sector number and a data buffer. *)

  val unmapped_s:
    f:('a -> int64 -> int64 -> 'a Lwt.t) -> 'a -> A.t ->
    ('a, A.error) result Lwt.t
  (** Folds [f acc ofs len] across offsets of unmapped data blocks
      read sequentially from the block device. [Fold.unmapped_s] will
      use knowledge about the underlying disk structure and will only
      fold across those blocks which are guaranteed to be zero
      i.e. those which are unmapped somehow. *)

end

(** {1 Copy bytes between blocks} *)
module Copy (A: S) (B: S): sig

  type error = private [> `Is_read_only | `Different_sizes]
  (** The type for copy errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for copy errors. *)

  val v: src:A.t -> dst:B.t -> (unit, error) result Lwt.t
  (** Copy all data from a source BLOCK device to a destination BLOCK
      device.

      Fails with `Different_sizes if the source and destination are not exactly
      the same size.

      Fails with `Is_read_only if the destination device is read-only.
  *)

end

(** {1 Sparse copy bytes between blocks}

    This functor use seeks operation to perform fast sparse copy
    between blocks. *)
module Sparse_copy (A: SEEKABLE) (B: S): sig

  type error = private [> `Is_read_only | `Different_sizes]
  (** The type for copy errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  val v: src:A.t -> dst:B.t -> (unit, error) result Lwt.t
  (** Copy all mapped data from a source SEEKABLE device to a
      destination BLOCK device.

      This function will preserve sparseness information in the source disk. The
      destination block device must be pre-zeroed, otherwise previous data will
      "leak through".

      Fails with `Different_sizes if the source and destination are not exactly
      the same size.

      Fails with `Is_read_only if the destination device is read-only.
  *)

end

(** {1 Fill blocks} *)
module Fill (A: S): sig

  val random: A.t -> (unit, A.write_error) result Lwt.t
  (** Fill a block device with pseudorandom data *)

end

(** {1 Safe block devices}

    Construct a safe wrapper around [B] where necessary buffer
    preconditions are checked on [read] and [write], and useful error
    messages generated. Some concrete implementations generate
    confusing errors (e.g. Unix might say "EINVAL") which are harder
    to debug. *)
module Make_safe (B: S): sig

  type error = private [> Mirage_block.error | `Unsafe of string]
  (** The type for errors. *)

  type write_error = private [> Mirage_block.write_error | `Unsafe of string]
  (** The type for write errors. *)

  include S with type t = B.t
             and type error := error
             and type write_error := write_error

  val unsafe_read: t -> int64 -> page_aligned_buffer list ->
    (unit, B.error) result Lwt.t
  (** [unsafe_read] is like [read] except it bypasses the necessary
      buffer precondition checks. Only use this if you want maximum
      performance and if you can prove the preconditions are
      respected. *)

  val unsafe_write: t -> int64 -> page_aligned_buffer list ->
    (unit, B.write_error) result Lwt.t
  (** [unsafe_write] is like [write] except it bypasses the necessary
      buffer precondition checks. Only use this if you want maximum
      performance and if you can prove the buffer preconditions are
      respected. *)

end

(** Very simple in-memory implementation of the block-device
    signature, using blocks of constant size (16M). Use
    {{:https://github.com/mirage/mirage-block-ramdisk}ramdisk} for a
    more serious implementation. *)
module Mem: sig
  include S
  val connect : string -> t Lwt.t
end
