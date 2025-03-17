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

open Qcow_types

type t [@@deriving sexp]
(** A physical address within the backing disk *)

val is_compressed: t -> bool
(** True if the address has been marked as being compressed *)

val is_mutable: t -> bool
(** True if the offset is safe to mutate directly (i.e. is not referenced
    by a snapshot *)

val unmapped: t
(** An unmapped physical address *)

val shift: t -> int -> t
(** [shift t bytes] adds [bytes] to t, maintaining other properties *)

val make: ?is_mutable:bool -> ?is_compressed:bool -> int -> t
(** Create an address at the given byte offset. This defaults to [is_mutable = true]
    which meand there are no snapshots implying that directly writing to this
    		offset is ok; and [is_compressed = false]. *)

val add: t -> int -> t
(** Add a byte offset to a physical address *)

val to_sector: sector_size:int -> t -> int64 * int
(** Return the sector on disk, plus a remainder within the sector *)

val sector: sector_size:int -> t -> int64
(** Return the sector on disk containing the address *)

val to_bytes: t -> int
(** Return the byte offset on disk *)

val cluster: cluster_bits:int -> t -> Cluster.t
(** Return the cluster containing the address *)

val within_cluster: cluster_bits:int -> t -> int
(** Return the index within the cluster of the address *)

val read: Cstruct.t -> t
(** Read a [t] from the given buffer *)

val write: t -> Cstruct.t -> unit
(** Write [t] to the buffer *)

include Qcow_s.PRINTABLE with type t := t
