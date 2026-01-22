(*
 * Copyright (C) 2025 Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** Key type for bucket table lookups. Empty strings act as wildcards. *)
module Key : sig
  type t = {user_agent: string; host_ip: string}

  val equal : t -> t -> bool

  val matches : pattern:t -> target:t -> bool
  (** [matches ~pattern ~target] returns true if [pattern] matches [target].
      Empty strings in [pattern] act as wildcards matching any value. *)

  val compare : t -> t -> int
  (** Total order: fewer wildcards first, then lexicographic by fields. *)
end

(** List of entries mapping keys to their token buckets for rate limiting.
    Lookups use wildcard matching with priority: exact > host_ip only > user_agent only. *)
type t

val create : unit -> t
(** [create ()] creates a new empty bucket table. *)

val add_bucket :
  t -> client_id:Key.t -> burst_size:float -> fill_rate:float -> bool
(** [add_bucket table ~client_id ~burst_size ~fill_rate] adds a token bucket
    for the given client_id. Returns [false] if a bucket already exists, if
    the bucket configuration is invalid (e.g. negative/zero fill rate), or if
    client_id has both fields empty (all-wildcard keys are rejected). *)

val mem : t -> client_id:Key.t -> bool
(** [mem table ~client_id] returns whether [client_id] matches any entry
    in the bucket table using wildcard matching. *)

val peek : t -> client_id:Key.t -> float option
(** [peek table ~client_id] returns the current token count for the client_id,
    or [None] if no bucket exists. *)

val delete_bucket : t -> client_id:Key.t -> unit
(** [delete_bucket table ~client_id] removes the bucket for the client_id. *)

val try_consume : t -> client_id:Key.t -> float -> bool
(** [try_consume table ~client_id amount] attempts to consume tokens.
    Returns [true] on success, [false] if insufficient tokens. *)

val submit : t -> client_id:Key.t -> callback:(unit -> unit) -> float -> unit
(** [submit table ~client_id ~callback amount] submits a callback to be executed
    under rate limiting. If tokens are immediately available and no callbacks are
    queued, the callback runs synchronously. Otherwise, it is enqueued and will
    be executed by a worker thread when tokens become available. Returns immediately. *)

val submit_sync : t -> client_id:Key.t -> callback:(unit -> 'a) -> float -> 'a
(** [submit_sync table ~client_id ~callback amount] submits a callback to be
    executed under rate limiting and blocks until it completes, returning the
    callback's result. *)
