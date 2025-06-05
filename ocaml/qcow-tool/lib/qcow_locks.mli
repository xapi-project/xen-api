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

(** A set of per-cluster read and write locks *)
type t

val make : unit -> t
(** Create a set of locks *)

(** A value which represents holding a lock *)
type lock

val unlock : lock -> unit
(** [unlock lock] releases the lock. Note releasing the same lock more than
    once will trigger a runtime failure. *)

module Client : sig
  (** An entity which holds a set of locks *)
  type t

  val make : (unit -> string) -> t
  (** [make describe_fn] creates an entity where [describe_fn ()] returns
      a human-readable description of the client for use in debugging. *)
end

module Read : sig
  (** Non-exclusive read locks *)

  val with_lock :
    ?client:Client.t -> t -> Cluster.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_lock t f] executes [f ()] with the lock held for reading *)

  val with_locks :
       ?client:Client.t
    -> t
    -> first:Cluster.t
    -> last:Cluster.t
    -> (unit -> 'a Lwt.t)
    -> 'a Lwt.t
  (** [with_locks t ~first ~last f] executes [f ()] with all clusters in the
      interval [first .. last] inclusive locked for reading. *)

  val lock : ?client:Client.t -> t -> Cluster.t -> lock Lwt.t
  (** [lock t cluster] acquire a non-exclusive read lock on [cluster]. The
      resulting lock must be released by calling [unlock] *)
end

module Write : sig
  (** Exclusive write locks *)

  val with_lock :
    ?client:Client.t -> t -> Cluster.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_lock t f] executes [f ()] with the lock held for writing *)

  val with_locks :
       ?client:Client.t
    -> t
    -> first:Cluster.t
    -> last:Cluster.t
    -> (unit -> 'a Lwt.t)
    -> 'a Lwt.t
  (** [with_locks t ~first ~last f] executes [f ()] with all clusters in the
      interval [first .. last] inclusive locked for writing. *)

  val try_lock : ?client:Client.t -> t -> Cluster.t -> lock option
  (** [try_lock ?client t cluster] returns a write lock on [cluster] if it can
      be done without blocking, or returns None. *)
end

val with_metadata_lock : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** [with_metadata_lock t f] executes [f ()] with the global metadata lock held.
    This prevents metadata blocks from moving while they're being used. *)

module Debug : sig
  val assert_no_locks_held : Client.t -> unit
  (** Check that all locks have been explicitly released. *)

  val dump_state : t -> unit
  (** Write the cluster lock state to the logs for analysis *)
end
