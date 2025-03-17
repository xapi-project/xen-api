(*
 * Copyright (C) 2017 Docker Inc
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

module Make(B: Qcow_s.RESIZABLE_BLOCK)(Time: Mirage_time.S): sig
  type t
  (** A cluster recycling engine *)

  val create: base:B.t -> sector_size:int -> cluster_bits:int
    -> cache:Qcow_cache.t -> locks:Qcow_locks.t
    -> metadata:Qcow_metadata.t -> runtime_asserts:bool -> t
  (** Initialise a cluster recycler over the given block device *)

  val set_cluster_map: t -> Qcow_cluster_map.t -> unit
  (** Set the associated cluster map (which will be updated on every cluster
      write) *)

  val start_background_thread: t -> keep_erased:int64 ->
    ?compact_after_unmaps:int64 -> unit -> unit
  (** Start a background thread which will perform block recycling *)

  val allocate: t -> Cluster.t -> Cluster.IntervalSet.t option
  (** [allocate t n] returns [n] clusters which are ready for re-use. If there
      are not enough clusters free then this returns None. *)

  val erase: t -> Cluster.IntervalSet.t -> (unit, B.write_error) result Lwt.t
  (** Write zeroes over the specified set of clusters *)

  val copy: t -> Cluster.t -> Cluster.t -> (unit, B.write_error) result Lwt.t
  (** [copy src dst] copies the cluster [src] to [dst] *)

  val move_all: ?progress_cb:(percent:int -> unit) -> t -> Qcow_cluster_map.Move.t list -> (unit, Qcow_metadata.write_error) result Lwt.t
  (** [move_all t mv] perform the initial data copy of the move operations [mv] *)

  val update_references: t -> (int64, Qcow_metadata.write_error) result Lwt.t
  (** [update_references t] rewrites references to any recently copied and
      flushed block, returning the number of writes completed. *)

  val flush: t -> (unit, B.write_error) result Lwt.t
  (** Issue a flush to the block device, update internal recycler state. *)
end
