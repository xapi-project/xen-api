(*
 * Copyright (C) Cloud Software Group
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

(** Measure the speed of an operation in a very simple and robust way.
    More detailed measurements can be dune using [Bechamel].
*)

(** 95% confidence interval, and median value *)
type t = {low: float; median: float; high: float}

val measure : ?n:int -> ?inner:int -> (unit -> unit) -> t
(** [measure ?n ?inner f] measures [n] times the duration of [inner] iterations of [f ()].

    Returns the median of the inner measurements, and a 95% confidence interval.
    The median is used, because it makes no assumptions about the distribution of the samples,
    i.e. it doesn't require a normal (Gaussian) distribution.

    The inner measurements use a simple average, because we only know the duration of [inner] iterations,
    not the duration of each individual call to [f ()].
    The purpose of the [inner] iterations is to reduce measurement overhead.

    @param n iteration count for the outer loop, must be more than [70].
    @param n iteration count for the inner loop
    @param f function to measure

    @raises Invalid_argument if [n<70]
 *)

val measure_min : ?n:int -> ('a -> unit) -> 'a -> float
(** [measure_min ?n:int f arg] is the minimum amount of time that [f arg] takes.

    This should be used when we try to measure the maximum speed of some operation (e.g. cached memory accesses),
    while ignoring latencies/hickups introduced by other processes on the system.

    It shouldn't be used for measuring the overhead of an operation, because the hickups may be part of that overhead.
 *)
