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

module D = Debug.Make (struct let name = "timeslice_recommended" end)

let yield_stop = Atomic.make false

let yield_worker () =
  while not (Atomic.get yield_stop) do
    Thread.yield ()
  done

let yield_overhead () =
  (* Thread.yield only has an effect if another thread exists,
     so create one that yields back immediately *)
  D.debug "Measuring Thread.yield overhead" ;
  Atomic.set yield_stop false ;
  let t = Thread.create yield_worker () in
  let measured = Simple_measure.measure Thread.yield in
  D.debug "Thread.yield overhead: %.6fs <= %.6fs <= %.6fs" measured.low
    measured.median measured.high ;
  D.debug "Waiting for worker thread to stop" ;
  Atomic.set yield_stop true ;
  Thread.join t ;
  measured.median

let measure ?(max_overhead_percentage = 1.0) () =
  let overhead = yield_overhead () in
  let interval = overhead /. (max_overhead_percentage /. 100.) in
  D.debug "Recommended timeslice interval = %.4fs" interval ;
  (* Avoid too high or too low intervals:
     do not go below 1ms (our HZ is 250, and max is 1000, the kernel would round up anyway)
     do not go above 50ms (the current default in OCaml 4.14)
  *)
  let interval = interval |> Float.max 0.001 |> Float.min 0.050 in
  D.debug "Final recommeded timeslice interval = %.4fs" interval ;
  interval
