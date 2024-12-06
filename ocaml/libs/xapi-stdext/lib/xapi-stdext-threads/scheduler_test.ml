(*
 * Copyright (C) 2024 Cloud Software Group
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

module Scheduler = Xapi_stdext_threads_scheduler.Scheduler

let started = Atomic.make false

let start_schedule () =
  if not (Atomic.exchange started true) then
    Thread.create Scheduler.loop () |> ignore

let send event data = Event.(send event data |> sync)

let receive event = Event.(receive event |> sync)

let elapsed_ms cnt =
  let elapsed_ns = Mtime_clock.count cnt |> Mtime.Span.to_uint64_ns in
  Int64.(div elapsed_ns 1000000L |> to_int)

let is_less = Alcotest.(testable (pp int)) Stdlib.( > )

let test_single () =
  let finished = Event.new_channel () in
  Scheduler.add_to_queue "one" Scheduler.OneShot 0.001 (fun () ->
      send finished true
  ) ;
  start_schedule () ;
  Alcotest.(check bool) "result" true (receive finished)

let test_remove_self () =
  let which = Event.new_channel () in
  Scheduler.add_to_queue "self" (Scheduler.Periodic 0.001) 0.001 (fun () ->
      (* this should remove the periodic scheduling *)
      Scheduler.remove_from_queue "self" ;
      (* add an operation to stop the test *)
      Scheduler.add_to_queue "stop" Scheduler.OneShot 0.1 (fun () ->
          send which "stop"
      ) ;
      send which "self"
  ) ;
  start_schedule () ;
  let cnt = Mtime_clock.counter () in
  Alcotest.(check string) "same event name" "self" (receive which) ;
  Alcotest.(check string) "same event name" "stop" (receive which) ;
  let elapsed_ms = elapsed_ms cnt in
  Alcotest.check is_less "small time" 300 elapsed_ms

let test_empty () =
  let finished = Event.new_channel () in
  Scheduler.add_to_queue "one" Scheduler.OneShot 0.001 (fun () ->
      send finished true
  ) ;
  start_schedule () ;
  Alcotest.(check bool) "finished" true (receive finished) ;
  (* wait loop to go to wait with no work to do *)
  Thread.delay 0.1 ;
  Scheduler.add_to_queue "two" Scheduler.OneShot 0.001 (fun () ->
      send finished true
  ) ;
  let cnt = Mtime_clock.counter () in
  Alcotest.(check bool) "finished" true (receive finished) ;
  let elapsed_ms = elapsed_ms cnt in
  Alcotest.check is_less "small time" 100 elapsed_ms

let tests =
  [
    ("test_single", `Quick, test_single)
  ; ("test_remove_self", `Quick, test_remove_self)
  ; ("test_empty", `Quick, test_empty)
  ]

let () = Alcotest.run "Scheduler" [("generic", tests)]
