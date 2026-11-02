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

(** The waits of one lock, each bounded by a monotonic deadline, with a
    back-off from 10 to 50 ms. A wait is refused as [`Stuck] when a thread of
    a holder stays in one wait in the kernel through a window of 2 s, and as
    [`Busy], without waiting, while a holder of another process that
    outlasted a whole wait less than 20 s ago still holds. *)

(** Shared by the threads that take the lock *)
type t

val create : unit -> t

(** A holder found stuck: its process, as in a blocker, and its threads that
    stayed in one wait in the kernel through the whole window *)
type stuck = {process: int * string; threads: int list}

val waiter :
     ?on_stuck:(stuck list -> unit)
  -> t
  -> Mtime.Span.t
  -> Xapi_stdext_unix.Lock_rw.waiter
(** Its waits end, in [`Busy], once that span has passed since it was made. A
    wait refused as [`Stuck] first passes the stuck holders to [on_stuck],
    which must not raise, for a caller that reports them. *)
