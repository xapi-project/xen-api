(*
 * Copyright (C) 2026 Cloud Software Group
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

(** Rate limiter built on top of a token bucket. Provides async and sync
    submission of callbacks that are rate-limited. Uses a worker thread
    to process queued callbacks when tokens become available. *)

type t

val create : burst_size:float -> fill_rate:float -> t
(** [create ~burst_size ~fill_rate] creates a new rate limiter with the given
    token bucket parameters.
    @raises Invalid_argument if the parameters are invalid: a fill rate
    that is not a positive finite number, or a NaN burst size.
    @param burst_size Maximum number of tokens in the bucket
    @param fill_rate Number of tokens added per second *)

val delete : t -> unit
(** [delete t] signals the worker thread to terminate. The worker thread
    processes any remaining queued callbacks, then exits. Blocks the caller
    until the worker thread has finished. Subsequent calls to [submit_async]
    or [submit_sync] will raise [Deleted]. *)

(** Raised by [submit_async] and [submit_sync] on a rate limiter that
    [delete] has been called on. The callback has been neither run nor
    queued. A caller that looked the limiter up in a table can look it up
    again, as the limiter may have been replaced. *)
exception Deleted

(** Observes the delay of a request that cannot run at once; neither
    function is called for a request that runs at once. [on_start] runs on
    the submitting thread when the request is delayed. [on_end] runs when
    the delay is over, just before the callback: on the worker thread for
    [submit_async], on the submitting thread for [submit_sync]. An exception
    from either is logged and ignored. For [submit_async], [on_start] runs
    while the queue lock of the limiter is held, so it must return quickly
    and must not call [delete], [submit_async] or [submit_sync] on the same
    limiter. *)
type delay_observer = {on_start: unit -> unit; on_end: unit -> unit}

val submit_async :
     t
  -> ?observer:delay_observer
  -> callback:(unit -> unit)
  -> caller_details:string
  -> float
  -> unit
(** [submit_async t ~callback amount] submits a callback under rate limiting.
    If tokens are immediately available and no callbacks are queued, the
    callback runs synchronously on the calling thread. Otherwise it is
    enqueued and will be executed by a worker thread when tokens become
    available. Returns immediately. *)

val submit_sync :
     t
  -> ?observer:delay_observer
  -> callback:(unit -> 'a)
  -> caller_details:string
  -> float
  -> 'a
(** [submit_sync t ~callback amount] submits a callback under rate limiting
    and blocks until it completes, returning the callback's result. If tokens
    are immediately available and no callbacks are queued, the callback runs
    directly. Otherwise, the caller blocks until the worker thread signals
    that tokens are available. *)
