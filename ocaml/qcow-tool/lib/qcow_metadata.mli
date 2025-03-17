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

type t
(** Qcow metadata: clusters containing references and clusters containing
    reference counts. *)

type error = [ Mirage_block.error | `Msg of string ]
type write_error = [ Mirage_block.write_error | `Msg of string ]

val make:
  cache:Qcow_cache.t
  -> cluster_bits:int
  -> locks:Qcow_locks.t
  -> unit -> t
(** Construct a qcow metadata structure given a set of cluster read/write/flush
    operations *)

val set_cluster_map: t -> Qcow_cluster_map.t -> unit
(** Set the associated cluster map (which will be updated on every cluster
    write) *)

type contents

module Refcounts: sig
  type t
  (** A cluster full of 16bit refcounts *)

  val of_contents: contents -> t
  (** Interpret the given cluster as a refcount cluster *)

  val get: t -> int -> int
  (** [get t n] return the [n]th refcount within [t] *)

  val set: t -> int -> int -> unit
  (** [set t n v] set the [n]th refcount within [t] to [v] *)
end

module Physical: sig
  type t
  (** A cluster full of 64 bit cluster pointers *)

  val of_contents: contents -> t
  (** Interpret the given cluster as a cluster of 64 bit pointers *)

  val get: t -> int -> Qcow_physical.t
  (** [get t n] return the [n]th physical address within [t] *)

  val set: t -> int -> Qcow_physical.t -> unit
  (** [set t n v] set the [n]th physical address within [t] to [v] *)

  val len: t -> int
  (** [len t] returns the number of physical addresses within [t] *)
end

val erase: contents -> unit
(** Set the cluster contents to zeroes *)

val read_and_lock: ?client:Qcow_locks.Client.t -> t -> Cluster.t -> (contents * Qcow_locks.lock, error) result Lwt.t

val read: ?client:Qcow_locks.Client.t -> t -> Cluster.t -> (contents -> ('a, error) result Lwt.t) -> ('a, error) result Lwt.t
(** Read the contents of the given cluster and provide them to the given function *)

val update: ?client:Qcow_locks.Client.t -> t -> Cluster.t -> (contents -> ('a, write_error) result Lwt.t) -> ('a, write_error) result Lwt.t
(** Read the contents of the given cluster, transform them through the given
    function and write the results back to disk *)
