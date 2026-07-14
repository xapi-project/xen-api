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
    @raises Invalid_argument if the parameters are invalid
    (e.g. non-positive fill rate).
    @param burst_size Maximum number of tokens in the bucket
    @param fill_rate Number of tokens added per second *)

val delete : t -> unit
(** [delete t] signals the worker thread to terminate. The worker thread
    processes any remaining queued callbacks, then exits. Blocks the caller
    until the worker thread has finished. Subsequent calls to [submit_async]
    or [submit_sync] will raise [Invalid_argument]. *)

(** Observer hooks for rate-limit delays. Both callbacks must be non-blocking
    (they run on the queue lock's critical path). Any exception is caught and
    logged; it never reaches the caller or the worker thread. *)
type delay_observer = {
    on_start: unit -> unit
        (** Fired when a submission is about to wait for tokens - never fires
            when tokens are available immediately. *)
  ; on_end: unit -> unit
        (** Fired the moment the wait ends, immediately before the user's
            [callback] runs. Together with [on_start] this brackets the
            actual delay. *)
}

val submit_async :
  t -> ?observer:delay_observer -> callback:(unit -> unit) -> float -> unit
(** [submit_async t ?observer ~callback amount] submits a callback under rate
    limiting. If tokens are immediately available and no callbacks are queued,
    the callback runs synchronously on the calling thread and [observer] is not
    invoked. Otherwise it is enqueued and will be executed by a worker thread
    when tokens become available. Returns immediately.

    When [observer] is supplied and the call is queued: [observer.on_start]
    fires on the caller's thread right after enqueue; [observer.on_end] fires
    on the worker thread once tokens have been granted, immediately before
    [callback]. *)

val submit_sync :
  t -> ?observer:delay_observer -> callback:(unit -> 'a) -> float -> 'a
(** [submit_sync t ?observer ~callback amount] submits a callback under rate
    limiting and blocks until it completes, returning the callback's result. If
    tokens are immediately available and no callbacks are queued, the callback
    runs directly and [observer] is not invoked. Otherwise, the caller blocks
    until the worker thread signals that tokens are available.

    When [observer] is supplied and the call is queued: both [observer.on_start]
    and [observer.on_end] fire on the caller's thread, bracketing the wait on
    the worker thread's signal. [observer.on_end] fires before [callback]. *)
