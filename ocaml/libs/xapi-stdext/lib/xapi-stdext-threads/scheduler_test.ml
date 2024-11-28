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

let test_single () =
  let finished = Event.new_channel () in
  Scheduler.add_to_queue "one" Scheduler.OneShot 0.001 (fun () ->
      send finished true
  ) ;
  start_schedule () ;
  Alcotest.(check bool) "result" true (receive finished)

let tests = [("test_single", `Quick, test_single)]

let () = Alcotest.run "Scheduler" [("generic", tests)]
