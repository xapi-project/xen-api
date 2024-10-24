(*
 * Copyright (c) Cloud Software Group
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

val global_flush_mutex : Mutex.t

val with_lock : (unit -> 'a) -> 'a
(** [with_lock f] executes [f] in a context where the calling thread
    holds the database lock. It is safe to nest such calls as the
    underlying lock is reentrant (a recursive mutex). *)

type report = {count: int; avg_time: float; min_time: float; max_time: float}

val report : unit -> report
