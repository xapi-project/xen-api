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
module Error = Qcow_error
module Header = Qcow_header
module Physical = Qcow_physical
module Int64 = Qcow_types.Int64

module Make (B : Qcow_s.RESIZABLE_BLOCK) (Time : Mirage_time.S) : sig
  include Mirage_block.S

  module Config : sig
    (** Runtime configuration of a device *)
    type t = {
        id: string  (** unique name for prometheus metrics *)
      ; discard: bool  (** true if `discard` will be enabled at runtime *)
      ; keep_erased: int64 option  (** size of erased free pool in sectors *)
      ; compact_after_unmaps: int64 option
            (** automatically compact after n sectors are unmapped *)
      ; check_on_connect: bool  (** perform an integrity check on connect *)
      ; runtime_asserts: bool  (** check cluster invariants at runtime *)
      ; read_only: bool  (** guarantee to not modify the file *)
    }

    val create :
         ?id:string
      -> ?discard:bool
      -> ?keep_erased:int64
      -> ?compact_after_unmaps:int64
      -> ?check_on_connect:bool
      -> ?runtime_asserts:bool
      -> ?read_only:bool
      -> unit
      -> t
    (** Customise the runtime behaviour, see [connect] or [create] *)

    val to_string : t -> string
    (** Marshal a config into a string suitable for a command-line argument *)

    val of_string : string -> (t, [`Msg of string]) result
    (** Parse the result of a previous [to_string] invocation *)
  end

  module Stats : sig
    (** Runtime statistics on a device *)
    type t = {
        mutable nr_erased: int64  (** number of sectors erased during discard *)
      ; mutable nr_unmapped: int64
            (** number of sectors unmapped during discard *)
    }
  end

  val create :
       B.t
    -> size:int64
    -> ?lazy_refcounts:bool
    -> ?cluster_bits:int
    -> ?config:Config.t
    -> unit
    -> (t, write_error) result Lwt.t
  (** [create block ~size ?lazy_refcounts ?cluster_bits ?config ()] initialises
      a qcow-formatted image on [block] with virtual size [size] in bytes.

      By default the file will use lazy refcounts, but this can be overriden by supplying
      [~lazy_refcounts:false]. By default the file will use 64KiB clusters (= 16 bits)
      but this can be overridden by supplying [?cluster_bits]. Note the cluster size
      must be greater than the sector size on the underlying block device.

      The [?config] argument does not affect the on-disk format but rather the
      behaviour as seen from this client. *)

  val connect : ?config:Config.t -> B.t -> t Lwt.t
  (** [connect ?config block] connects to an existing qcow-formatted image on
      [block]. *)

  val resize :
       t
    -> new_size:int64
    -> ?ignore_data_loss:bool
    -> unit
    -> (unit, write_error) result Lwt.t
  (** [resize block new_size_bytes ?ignore_data_loss] changes the size of the
      qcow-formatted image to [new_size_bytes], rounded up to the next allocation
      unit. This function will fail with an error if the new size would be
      smaller than the old size as this would cause data loss, unless the argument
      [?ignore_data_loss] is set to true. *)

  (** Summary of the compaction run *)
  type compact_result = {
      copied: int64  (** number of sectors copied *)
    ; refs_updated: int64  (** number of cluster references updated *)
    ; old_size: int64  (** previous size in sectors *)
    ; new_size: int64  (** new size in sectors *)
  }

  val compact :
       t
    -> ?progress_cb:(percent:int -> unit)
    -> unit
    -> (compact_result, write_error) result Lwt.t
  (** [compact t ()] scans the disk for unused space and attempts to fill it
      and shrink the file. This is useful if the underlying block device doesn't
      support discard and we must emulate it. *)

  val discard :
    t -> sector:int64 -> n:int64 -> unit -> (unit, write_error) result Lwt.t
  (** [discard sector n] signals that the [n] sectors starting at [sector]
      are no longer needed and the contents may be discarded. Note the contents
      may not actually be deleted: this is not a "secure erase". *)

  val seek_unmapped : t -> int64 -> (int64, error) result Lwt.t
  (** [seek_unmapped t start] returns the offset of the next "hole": a region
      of the device which is guaranteed to be full of zeroes (typically
      guaranteed because it is unmapped) *)

  val seek_mapped : t -> int64 -> (int64, error) result Lwt.t
  (** [seek_mapped t start] returns the offset of the next region of the
      device which may have data in it (typically this is the next mapped
      region) *)

  val rebuild_refcount_table : t -> (unit, write_error) result Lwt.t
  (** [rebuild_refcount_table t] rebuilds the refcount table from scratch.
      Normally we won't update the refcount table live, for performance. *)

  type check_result = {
      free: int64  (** unused sectors *)
    ; used: int64  (** used sectors *)
  }

  val check :
       B.t
    -> ( check_result
       , [ Mirage_block.error
         | `Reference_outside_file of int64 * int64
         | `Duplicate_reference of (int64 * int) * (int64 * int) * int64
         | `Msg of string ]
       )
       result
       Lwt.t
  (** [check t] performs sanity checks of the file, looking for errors.
      The error [`Reference_outside_file (src, dst)] means that at offset [src]
      there is a reference to offset [dst] which is outside the file.
      The error [`Duplicate_reference (ref1, ref2, target) means that references
      at both [ref1] and [ref2] both point to the same [target] offset. *)

  val flush : t -> (unit, write_error) result Lwt.t
  (** [flush t] flushes any outstanding buffered writes *)

  val header : t -> Header.t
  (** Return a snapshot of the current header *)

  val to_config : t -> Config.t
  (** [to_config t] returns the configuration of a device *)

  val get_stats : t -> Stats.t
  (** [get_stats t] returns the runtime statistics of a device *)

  module Debug : sig
    val check_no_overlaps : t -> (unit, write_error) result Lwt.t

    val assert_no_leaked_blocks : t -> unit

    val assert_cluster_map_in_sync : t -> unit Lwt.t

    module Setting : sig
      val compact_mid_write : bool ref
      (** true means to trigger a compact part-way through a write to check that
          the write completes properly after the compact *)
    end

    val metadata_blocks : t -> Int64.IntervalSet.t
    (** Return the set of physical disk offsets containing metadata *)
  end
end
