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
    back-off from 10 to 50 ms. *)

type t

val create : unit -> t

val waiter : t -> Mtime.Span.t -> Xapi_stdext_unix.Lock_rw.waiter
(** Its waits end, in [`Busy], once that span has passed since it was made. *)
