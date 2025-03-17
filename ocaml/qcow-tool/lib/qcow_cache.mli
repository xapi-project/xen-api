(*
 * Copyright (C) 2017 David Scott <dave@recoil.org>
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
(** A cache of clusters *)

val create:
  read_cluster:(Cluster.t -> (Cstruct.t, Mirage_block.error) result Lwt.t) ->
  write_cluster:(Cluster.t -> Cstruct.t -> (unit, Mirage_block.write_error) result Lwt.t) ->
  unit -> t
(** Create a cache of clusters, given the read/write functions *)

val read: t -> Cluster.t -> (Cstruct.t, Mirage_block.error) result Lwt.t
(** [read t cluster] returns the data in [cluster] *)

val write: t -> Cluster.t -> Cstruct.t -> (unit, Mirage_block.write_error) result Lwt.t
(** [write t cluster data] writes [data] to [cluster] *)

val remove: t -> Cluster.t -> unit
(** [remove t cluster] drops any cache associated with [cluster] *)

val resize: t -> Cluster.t -> unit
(** [resize t new_size_clusters] drops any cache entries which are beyond the new
    file size. *)

module Debug: sig
  val assert_not_cached: t -> Cluster.t -> unit

  val all_cached_clusters: t -> Cluster.IntervalSet.t

  val check_disk: t -> (unit, Mirage_block.error) result Lwt.t
end
