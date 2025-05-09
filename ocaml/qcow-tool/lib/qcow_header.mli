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

module Version : sig
  type t = [`One | `Two | `Three] [@@deriving sexp]

  include Qcow_s.SERIALISABLE with type t := t

  val compare : t -> t -> int
end

module CryptMethod : sig
  type t = [`Aes | `None] [@@deriving sexp]

  include Qcow_s.SERIALISABLE with type t := t

  val compare : t -> t -> int
end

module Feature : sig
  type ty = [`Incompatible | `Compatible | `Autoclear]

  type feature = [`Corrupt | `Dirty | `Lazy_refcounts | `Unknown of string]

  type t = {ty: ty; bit: int; feature: feature}

  val understood : t list
  (** The features understood by this implementation *)

  include Qcow_s.SERIALISABLE with type t := t
end

(** Offset within the image *)
type offset = int64

type extension =
  [ `Unknown of int32 * string
  | `Backing_file of string
  | `Feature_name_table of Feature.t list ]
[@@deriving sexp]

(** Version 3 and above have additional header fields *)
type additional = {
    dirty: bool
  ; corrupt: bool
  ; lazy_refcounts: bool
  ; autoclear_features: int64
  ; refcount_order: int32
}
[@@deriving sexp]

(** The qcow2 header *)
type t = {
    version: Version.t
  ; backing_file_offset: offset  (** offset of the backing file path *)
  ; backing_file_size: int32  (** length of the backing file path *)
  ; cluster_bits: int32  (** a cluster is 2 ** cluster_bits in size *)
  ; size: int64  (** virtual size of the image *)
  ; crypt_method: CryptMethod.t
  ; l1_size: int32  (** number of 8-byte entries in the L1 table *)
  ; l1_table_offset: Qcow_physical.t  (** offset of the L1 table *)
  ; refcount_table_offset: Qcow_physical.t  (** offset of the refcount table *)
  ; refcount_table_clusters: int32
        (** size of the refcount table in clusters *)
  ; nb_snapshots: int32  (** the number of internal snapshots *)
  ; snapshots_offset: offset  (** offset of the snapshot header *)
  ; additional: additional option  (** for version 3 or higher *)
  ; extensions: extension list  (** for version 3 or higher *)
}
[@@deriving sexp]

val refcounts_per_cluster : t -> int64
(** The number of 16-bit reference counts per cluster *)

val max_refcount_table_size : t -> int64
(** Compute the maximum size of the refcount table *)

val l2_tables_required : cluster_bits:int -> int64 -> int64
(** Compute the number of L2 tables required for this size of image *)

include Qcow_s.SERIALISABLE with type t := t

include Qcow_s.PRINTABLE with type t := t

include Set.OrderedType with type t := t
