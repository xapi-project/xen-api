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

(* avoid allocating an extra option every time *)
let invalid_holder = -1

let last_lock_holder = Atomic.make invalid_holder

let me () = Thread.self () |> Thread.id

let lock_acquired () =
  (* these need to be very low overhead, so just keep track of the last lock holder,
     i.e. track only one high-priority lock at a time
  *)
  Atomic.set last_lock_holder (me ())

let lock_released () = Atomic.set last_lock_holder invalid_holder

let[@inline always] am_i_holding_locks () =
  let last = Atomic.get last_lock_holder in
  last <> invalid_holder && last = me ()

let yield_interval = Atomic.make Mtime.Span.zero

(* TODO: use bechamel.monotonic-clock instead, which has lower overhead,
   but not in the right place in xs-opam yet
*)
let last_yield = Atomic.make (Mtime_clock.counter ())

let failures = Atomic.make 0

let periodic_hook (_ : Gc.Memprof.allocation) =
  let () =
    try
      if not (am_i_holding_locks ()) then
        let elapsed = Mtime_clock.count (Atomic.get last_yield) in
        if Mtime.Span.compare elapsed (Atomic.get yield_interval) > 0 then (
          let now = Mtime_clock.counter () in
          Atomic.set last_yield now ; Thread.yield ()
        )
    with _ ->
      (* It is not safe to raise exceptions here, it'd require changing all code to be safe to asynchronous interrupts/exceptions,
         see https://guillaume.munch.name/software/ocaml/memprof-limits/index.html#isolation
         Because this is just a performance optimization, we fall back to safe behaviour: do nothing, and just keep track that we failed
      *)
      Atomic.incr failures
  in
  None

let periodic =
  Gc.Memprof.
    {null_tracker with alloc_minor= periodic_hook; alloc_major= periodic_hook}

let set ?(sampling_rate = 1e-4) interval =
  Atomic.set yield_interval
    (Mtime.Span.of_float_ns @@ (interval *. 1e9) |> Option.get) ;
  Gc.Memprof.start ~sampling_rate ~callstack_size:0 periodic

let clear () =
  Gc.Memprof.stop () ;
  Atomic.set yield_interval Mtime.Span.zero
