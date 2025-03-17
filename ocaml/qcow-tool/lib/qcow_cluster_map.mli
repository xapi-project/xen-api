(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
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

type t
(** A cluster map which describes cluster usage in the file. The cluster map
    tracks which clusters are free, and which are used, and where the references
    are. *)

type move_state =
  | Copying
  (** a background copy is in progress. If this cluster is modified then
      the copy should be aborted. *)
  | Copied
  (** contents of this cluster have been copied once to another cluster.
      If this cluster is modified then the copy should be aborted. *)
  | Flushed
  (** contents of this cluster have been copied and flushed to disk: it
      is now safe to rewrite the pointer. If this cluster is modified then
      the copy should be aborted. *)
  | Referenced
  (** the reference has been rewritten; it is now safe to write to this
      cluster again. On the next flush, the copy is complete and the original
      block can be recycled. *)
(** Describes the state of a block move *)

type reference = Cluster.t * int (* cluster * index within cluster *)

module Move: sig
  type t = { src: Cluster.t; dst: Cluster.t }
  (** An instruction to move the contents from cluster [src] to cluster [dst] *)

  val to_string: t -> string
end

type move = {
  move: Move.t;
  state: move_state;
}
(** describes the state of an in-progress block move *)

val string_of_move: move -> string

type cluster_state =
  | Junk
  | Erased
  | Available
  | Copies
  | Roots
(** The state of a cluster *)

val set_cluster_state: t -> Cluster.IntervalSet.t -> cluster_state -> cluster_state -> unit
(** Update the state of a cluster *)

module type MutableSet = sig
  val get: t -> Cluster.IntervalSet.t
  (** [get t] query the current contents of the set *)

  val remove: t -> Cluster.IntervalSet.t -> unit
  (** [remove t less] removes [less] from the set *)

  val mem: t -> Cluster.t -> bool
  (** [mem t cluster] is true if [cluster] is in [t] *)
end

val zero: t
(** A cluster map for a zero-length disk *)

val make: free:Qcow_bitmap.t -> refs:reference Cluster.Map.t -> cache:Qcow_cache.t
  -> first_movable_cluster:Cluster.t -> runtime_asserts:bool
  -> id:string option -> cluster_size:int -> t
(** Given a set of free clusters, and the first cluster which can be moved
    (i.e. that isn't fixed header), construct an empty cluster map. *)

val total_used: t -> int64
(** Return the number of tracked used clusters *)

val total_free: t -> int64
(** Return the number of tracked free clusters *)

val resize: t -> Cluster.t -> unit
(** [resize t new_size_clusters] is called when the file is to be resized. *)

val add: t -> reference -> Cluster.t -> unit
(** [add t ref cluster] marks [cluster] as in-use and notes the reference from
    [reference]. *)

val remove: t -> Cluster.t -> unit
(** [remove t cluster] marks [cluster] as free and invalidates any reference
    to it (e.g. in response to a discard) *)

module Junk: MutableSet
(** Clusters which contain arbitrary data *)

module Erased: MutableSet
(** Clusters which have been erased but haven't been flushed yet so can't be
    safely reallocated. *)

module Available: MutableSet
(** Clusters which are available for reallocation *)

module Copies: MutableSet
(** Clusters which contain copies, as part of a compact *)

module Roots: MutableSet
(** Clusters which have been allocated but not yet placed somewhere reachable
    from the GC *)

val wait: t -> unit Lwt.t
(** [wait t] wait for some amount of recycling work to become available, e.g.
    - junk could be created
    - available could be used
    - a move might require a reference update *)

val start_moves: t -> Move.t list
(** [start_moves t] calculates the block moves required to compact [t] and
    marks the clusters as moving *)

val moves: t -> move Cluster.Map.t
(** [moves t] returns the state of the current active moves *)

val set_move_state: t -> Move.t -> move_state -> unit
(** Update the state of the given move operation *)

val is_moving: t -> Cluster.t -> bool
(** [is_moving t cluster] returns true if [cluster] is still moving *)

val cancel_move: t -> Cluster.t -> unit
(** [cancel_move cluster] cancels any in-progress move of cluster [cluster].
    This should be called with the cluster write lock held whenever there has
    been a change in the contents of [cluster] *)

val complete_move: t -> Move.t -> unit
(** [complete_move t move] marks the move as complete. *)

val find: t -> Cluster.t -> reference
(** [find t cluster] returns the reference to [cluster], or raises [Not_found] *)

val get_last_block: t -> Cluster.t
(** [get_last_block t] is the last allocated block in [t]. Note if there are no
    data blocks this will point to the last header block even though it is
    immovable. *)

val is_immovable: t -> Cluster.t -> bool
(** [is_immovable t cluster] is true if [cluster] is fixed and cannot be moved
    i.e. it is before the first_movable_cluster i.e. it is part of the fixed
    (L1) header structure. *)

val update_references: t -> Cluster.t Cluster.Map.t -> unit
(** [update_references t subst] updates the reference table following the given set
    of substitutions. Any reference to a source block must be updated to the
    destination block otherwise it will be left pointing to junk. Normally this
    is guaranteed by the Metadata.Physical.set function, but when compacting we
    split the operation into phases and copy the block first at the byte level,
    leaving the map out-of-sync *)

val to_summary_string: t -> string
(** [to_summary_string t] returns a terse printable summary of [t] *)

module Debug: sig
  val assert_no_leaked_blocks: t -> unit
  (** Check no blocks have gone missing *)

  val assert_equal: t -> t -> unit
  (** Check that 2 maps have equivalent contents *)

  val metadata_blocks: t -> Cluster.IntervalSet.t
  (** Return the set of blocks containing metadata *)
end
