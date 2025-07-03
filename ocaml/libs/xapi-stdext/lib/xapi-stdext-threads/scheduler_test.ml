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

let calibrated_ratio () =
  let expected = Mtime.Span.(100 * ms |> to_float_ns) in
  let elapsed = Mtime_clock.counter () in
  (* Add a 10% leeway to the ratio calculated *)
  Thread.delay 0.11 ;
  let actual = Mtime_clock.count elapsed |> Mtime.Span.to_float_ns in
  let ratio = actual /. expected in
  Alcotest.(check bool) (Printf.sprintf "ratio is %f" ratio) true true ;
  ratio

let started = Atomic.make false

let start_schedule () =
  if not (Atomic.exchange started true) then
    Thread.create Scheduler.loop () |> ignore

let send event data () = Event.(send event data |> sync)

let receive event = Event.(receive event |> sync)

let is_less ratio a b =
  let a =
    Mtime.Span.to_float_ns a
    |> Float.mul ratio
    |> Int64.of_float
    |> Mtime.Span.of_uint64_ns
  in
  Mtime.Span.is_shorter ~than:a b

let mtime_span () =
  let cmp = is_less (calibrated_ratio ()) in
  Alcotest.(testable Mtime.Span.pp) cmp

let test_single () =
  let finished = Event.new_channel () in
  Scheduler.add_to_queue "one" Scheduler.OneShot 0.001 (send finished true) ;
  start_schedule () ;
  Alcotest.(check bool) "result" true (receive finished)

let test_remove_self mtime_span () =
  let which = Event.new_channel () in
  Scheduler.add_to_queue "self" (Scheduler.Periodic 0.001) 0.001 (fun () ->
      (* this should remove the periodic scheduling *)
      Scheduler.remove_from_queue "self" ;
      (* add an operation to stop the test *)
      Scheduler.add_to_queue "stop" Scheduler.OneShot 0.1 (send which "stop") ;
      send which "self" ()
  ) ;
  start_schedule () ;

  let from_wait_to_receive = Mtime_clock.counter () in
  Alcotest.(check string) "same event name" "self" (receive which) ;
  Alcotest.(check string) "same event name" "stop" (receive which) ;

  let elapsed = Mtime_clock.count from_wait_to_receive in
  let expected = Mtime.Span.(300 * ms) in
  Alcotest.check mtime_span "small time" expected elapsed

let test_empty mtime_span () =
  let finished = Event.new_channel () in
  Scheduler.add_to_queue "one" Scheduler.OneShot 0.001 (send finished true) ;
  start_schedule () ;
  Alcotest.(check bool) "finished" true (receive finished) ;
  (* wait loop to go to wait with no work to do *)
  Thread.delay 0.1 ;
  Scheduler.add_to_queue "two" Scheduler.OneShot 0.001 (send finished true) ;

  let from_wait_to_receive = Mtime_clock.counter () in
  Alcotest.(check bool) "finished" true (receive finished) ;

  let elapsed = Mtime_clock.count from_wait_to_receive in
  let expected = Mtime.Span.(100 * ms) in
  Alcotest.check mtime_span "small time" expected elapsed

let test_wakeup mtime_span () =
  let which = Event.new_channel () in
  (* schedule a long event *)
  Scheduler.add_to_queue "long" Scheduler.OneShot 2.0 (send which "long") ;
  start_schedule () ;
  (* wait loop to go to wait with no work to do *)
  Thread.delay 0.1 ;

  (* schedule a quick event, should wake up the loop *)
  Scheduler.add_to_queue "quick" Scheduler.OneShot 0.1 (send which "quick") ;

  let from_wait_to_receive_quick = Mtime_clock.counter () in
  Alcotest.(check string) "same event name" "quick" (receive which) ;

  Scheduler.remove_from_queue "long" ;
  let elapsed = Mtime_clock.count from_wait_to_receive_quick in
  let expected = Mtime.Span.(100 * ms) in
  Alcotest.check mtime_span "small time" expected elapsed

let tests =
  let mtime_span = mtime_span () in
  [
    ("test_single", `Quick, test_single)
  ; ("test_remove_self", `Quick, test_remove_self mtime_span)
  ; ("test_empty", `Quick, test_empty mtime_span)
  ; ("test_wakeup", `Quick, test_wakeup mtime_span)
  ]

let () = Alcotest.run "Scheduler" [("generic", tests)]
