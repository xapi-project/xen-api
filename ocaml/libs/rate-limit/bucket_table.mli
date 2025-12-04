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

(** Hash table mapping client identifiers to their token buckets for rate limiting. *)
type t

val create : unit -> t
(** [create ()] creates a new empty bucket table. *)

val add_bucket :
  t -> user_agent:string -> burst_size:float -> fill_rate:float -> bool
(** [add_bucket table ~user_agent ~burst_size ~fill_rate] adds a token bucket
    for the given user agent. Returns [false] if a bucket already exists, or if
    the bucket configuration is invalid, e.g. negative fill rate. *)

val peek : t -> user_agent:string -> float option
(** [peek table ~user_agent] returns the current token count for the user agent,
    or [None] if no bucket exists. *)

val delete_bucket : t -> user_agent:string -> unit
(** [delete_bucket table ~user_agent] removes the bucket for the user agent. *)

val try_consume : t -> user_agent:string -> float -> bool
(** [try_consume table ~user_agent amount] attempts to consume tokens.
    Returns [true] on success, [false] if insufficient tokens. *)

val submit : t -> user_agent:string -> callback:(unit -> unit) -> float -> unit
(** [submit table ~user_agent ~callback amount] submits a callback to be executed
    under rate limiting. If tokens are immediately available and no callbacks are
    queued, the callback runs synchronously. Otherwise, it is enqueued and will
    be executed by a worker thread when tokens become available. Returns immediately. *)

val submit_sync : t -> user_agent:string -> callback:(unit -> 'a) -> float -> 'a
(** [submit_sync table ~user_agent ~callback amount] submits a callback to be
    executed under rate limiting and blocks until it completes, returning the
    callback's result. *)
