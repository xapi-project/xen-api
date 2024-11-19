(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

open Test_common
open Event_types
module Delay = Xapi_stdext_threads.Threadext.Delay

let event_setup_common = Test_event_common.event_setup_common

let test_event_from_ev () =
  (* Test that creating an object generates an event for that object *)
  let __context, _ = event_setup_common () in
  let evs =
    Xapi_event.from ~__context ~classes:["vm"] ~token:"" ~timeout:30.0
    |> parse_event_from
  in
  let tok = evs.token in
  let vm = make_vm ~__context () in
  let evs_rpc =
    Xapi_event.from ~__context ~classes:["vm"] ~token:tok ~timeout:4.0
  in
  let evs = evs_rpc |> parse_event_from in
  Printf.printf "evs: %s\n%!" (Jsonrpc.to_string evs_rpc) ;
  let ev = List.filter (fun ev -> ev.ty = "vm") evs.events in
  Alcotest.(check int) "list length" 1 (List.length ev) ;
  let ev = List.hd ev in
  Alcotest.(check string) "ev.reference" (Ref.string_of vm) ev.reference

let test_event_from_ev_rel () =
  (* Test that creating a connector object generates all relevant events *)
  let __context, _ = event_setup_common () in
  let vm = make_vm ~__context () in
  let evs =
    Xapi_event.from ~__context ~classes:["vm"; "vbd"] ~token:"" ~timeout:30.0
    |> parse_event_from
  in
  let tok = evs.token in
  let vbd = make_vbd ~__context ~vM:vm () in
  let evs_rpc =
    Xapi_event.from ~__context ~classes:["vm"; "vbd"] ~token:tok ~timeout:4.0
  in
  let evs2 = evs_rpc |> parse_event_from in
  let tok2 = evs2.token in
  let vm_ev = List.filter (fun ev -> ev.ty = "vm") evs2.events in
  let vbd_ev = List.filter (fun ev -> ev.ty = "vbd") evs2.events in
  Alcotest.(check int) "list length" 1 (List.length vm_ev) ;
  Alcotest.(check int) "list length" 1 (List.length vbd_ev) ;
  let ev = List.hd vm_ev in
  Alcotest.(check string) "ev.reference" (Ref.string_of vm) ev.reference ;
  Db.VBD.destroy ~__context ~self:vbd ;
  let evs_rpc =
    Xapi_event.from ~__context ~classes:["vm"; "vbd"] ~token:tok2 ~timeout:4.0
  in
  let evs3 = evs_rpc |> parse_event_from in
  let vm_ev = List.filter (fun ev -> ev.ty = "vm") evs3.events in
  let vbd_ev = List.filter (fun ev -> ev.ty = "vbd") evs3.events in
  Alcotest.(check int) "list length" 1 (List.length vm_ev) ;
  Alcotest.(check int) "list length" 1 (List.length vbd_ev)

let test_event_from_timeout () =
  let __context, _ = event_setup_common () in
  let evs =
    Xapi_event.from ~__context ~classes:["vm"] ~token:"" ~timeout:30.0
    |> parse_event_from
  in
  let tok = evs.token in
  let start_time = Unix.gettimeofday () in
  let _ =
    Xapi_event.from ~__context ~classes:["vm"] ~token:tok ~timeout:1.0
    |> parse_event_from
  in
  let end_time = Unix.gettimeofday () in
  let elapsed = end_time -. start_time in
  Printf.printf "test_event_from_timeout: elapsed=%f\n" elapsed ;
  Alcotest.(check bool) "timeout correct" true (elapsed < 2.0 && elapsed > 1.0)

let event_next_unblock () =
  let __context, session_id = event_setup_common () in
  let () = Xapi_event.register ~__context ~classes:[] in
  (* no events *)
  let wait_hdl = Delay.make () in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        ( try ignore (Xapi_event.next ~__context)
          with e ->
            Printf.printf
              "background thread caught: %s (an exception is expected)"
              (Printexc.to_string e)
        ) ;
        Delay.signal wait_hdl
      )
      ()
  in
  (* Background thread is started but it cannot simultaneously block and signal us to
     logout so a little pause in here is probably the best we can do *)
  Thread.delay 0.5 ;
  (* Logout which should cause the background thread to unblock *)
  Xapi_session.destroy_db_session ~__context ~self:session_id ;
  (* Again we can't tell the difference between a slow and a totally blocked thread
     so set the max wait time 10 times more, but it will ublock as early as possible *)
  let unblocked = not (Delay.wait wait_hdl (0.5 *. 10.)) in
  Alcotest.(check bool) "Unblocked" true unblocked

let event_next_test () =
  let __context, _ = event_setup_common () in
  let () = Xapi_event.register ~__context ~classes:["pool"] in
  let wait_hdl = Delay.make () in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  let key = "event_next_test" in
  ( try Db.Pool.remove_from_other_config ~__context ~self:pool ~key
    with _ -> ()
  ) ;
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        let finished = ref false in
        while not !finished do
          ignore (Xapi_event.next ~__context) ;
          let oc = Db.Pool.get_other_config ~__context ~self:pool in
          if List.mem_assoc key oc && List.assoc key oc = "1" then (
            Printf.printf "got expected event" ;
            finished := true ;
            Delay.signal wait_hdl
          )
        done
      )
      ()
  in
  Thread.delay 1. ;
  Db.Pool.add_to_other_config ~__context ~self:pool ~key ~value:"1" ;
  let unblocked = not (Delay.wait wait_hdl (1.0 *. 10.)) in
  Alcotest.(check bool) "checking other_config" true unblocked

let wait_for_pool_key __context key =
  let token = ref "" in
  let finished = ref false in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  while not !finished do
    let events =
      Xapi_event.from ~__context ~classes:["pool"] ~token:!token ~timeout:10.
      |> parse_event_from
    in
    token := events.token ;
    let oc = Db.Pool.get_other_config ~__context ~self:pool in
    if List.mem_assoc key oc && List.assoc key oc = "1" then finished := true
  done

let event_from_test () =
  let __context, _ = event_setup_common () in
  let wait_hdl = Delay.make () in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  let key = "event_from_test" in
  ( try Db.Pool.remove_from_other_config ~__context ~self:pool ~key
    with _ -> ()
  ) ;
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        wait_for_pool_key __context key ;
        Delay.signal wait_hdl
      )
      ()
  in
  Thread.delay 0.5 ;
  Db.Pool.add_to_other_config ~__context ~self:pool ~key ~value:"1" ;
  let unblocked = not (Delay.wait wait_hdl (0.5 *. 10.)) in
  Alcotest.(check bool) "event_from_test" true unblocked

let event_from_parallel_test () =
  let __context, _ = event_setup_common () in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  let key = "event_next_test" in
  ( try Db.Pool.remove_from_other_config ~__context ~self:pool ~key
    with _ -> ()
  ) ;
  let ok = ref true in
  let (i_should_succeed : Thread.t) =
    Thread.create
      (fun () ->
        try
          let _ =
            Xapi_event.from ~__context ~classes:[] ~token:"" ~timeout:2.0
          in
          ()
          (* good *)
        with e ->
          Printf.printf "Caught unexpected error: %s\n"
            (ExnHelper.string_of_exn e) ;
          ok := false
      )
      ()
  in
  let (interfering_thread : Thread.t) =
    Thread.create (fun () -> wait_for_pool_key __context key) ()
  in
  Thread.delay 0.5 ;
  (* wait for both threads to block in Event.from *)
  Db.Pool.add_to_other_config ~__context ~self:pool ~key ~value:"1" ;
  Thread.join interfering_thread ;
  Thread.join i_should_succeed ;
  (* Check that Event.from didn't get cancelled by mistake *)
  Alcotest.(check bool) "event_from_parallel_test" true !ok

let object_level_event_test _session_id =
  let __context, _ = event_setup_common () in
  let finished = ref false in
  let failure = ref false in
  let wait_hdl = Delay.make () in
  let vm_a = make_vm ~__context ~name_label:"vm_a" () in
  let vm_b = make_vm ~__context ~name_label:"vm_b" () in
  Printf.printf "watching %s\n%!" (Ref.string_of vm_a) ;
  Printf.printf "ignoring %s\n%!" (Ref.string_of vm_b) ;
  let key = "object_level_event_next" in
  (try Db.VM.remove_from_other_config ~__context ~self:vm_a ~key with _ -> ()) ;
  (try Db.VM.remove_from_other_config ~__context ~self:vm_b ~key with _ -> ()) ;
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        let token = ref "" in
        while not !finished do
          Printf.printf "Calling event.from...\n%!" ;
          let events =
            Xapi_event.from ~__context
              ~classes:[Printf.sprintf "vm/%s" (Ref.string_of vm_a)]
              ~token:!token ~timeout:10.
            |> parse_event_from
          in
          Printf.printf "Got %d events\n%!" (List.length events.events) ;
          List.iter
            (fun event ->
              if event.reference <> Ref.string_of vm_a then (
                Printf.printf
                  "FAILURE: event on %s which we aren't watching\n%!"
                  event.reference ;
                failure := true ;
                finished := true
              )
            )
            events.events ;
          token := events.token ;
          let oc = Db.VM.get_other_config ~__context ~self:vm_a in
          if List.mem_assoc key oc && List.assoc key oc = "1" then (
            Printf.printf "got expected event (new token = %s)\n%!" !token ;
            finished := true
          ) else
            Printf.printf "Db doesn't have expected change in...\n%!"
        done ;
        Delay.signal wait_hdl
      )
      ()
  in
  Thread.delay 0.5 ;
  Printf.printf "Adding to vm_b\n%!" ;
  Db.VM.add_to_other_config ~__context ~self:vm_b ~key ~value:"1" ;
  Thread.delay 0.5 ;
  Printf.printf "Removing from vm_b\n%!" ;
  Db.VM.remove_from_other_config ~__context ~self:vm_b ~key ;
  Printf.printf "Adding to vm_a. This ought to wake up the event thread\n%!" ;
  Db.VM.add_to_other_config ~__context ~self:vm_a ~key ~value:"1" ;
  let blocked = Delay.wait wait_hdl (1.0 *. 10.) in
  if blocked then (
    Printf.printf "FAILURE: Didn't get expected change in event thread\n%!" ;
    failure := true
  ) ;
  if !failure then
    Alcotest.fail "failed to see object-level event change"

let test_short_oneshot () =
  (* don't call event_setup_common here, it'll register a dummy event and hide the bug *)
  let started = ref false in
  let m = Mutex.create () in
  let cond = Condition.create () in
  let scheduler () =
    Mutex.lock m ;
    started := true ;
    Condition.broadcast cond ;
    Mutex.unlock m ;
    Xapi_periodic_scheduler.loop ()
  in
  ignore (Thread.create scheduler ()) ;
  (* ensure scheduler sees an empty queue , by waiting for it to start *)
  Mutex.lock m ;
  while not !started do
    Condition.wait cond m
  done ;
  Mutex.unlock m ;
  (* run the scheduler, let it realize its queue is empty,
     a Thread.yield is not enough due to the use of debug which would yield back almost immediately.
  *)
  Thread.delay 1. ;
  let fired = Atomic.make false in
  let fire () = Atomic.set fired true in
  let task = "test_oneshot" in
  Xapi_periodic_scheduler.add_to_queue task Xapi_periodic_scheduler.OneShot 1.
    fire ;
  Thread.delay 2. ;
  assert (Atomic.get fired)

let test =
  [
    ("test_event_from_timeout", `Slow, test_event_from_timeout)
  ; ("test_event_from_ev", `Quick, test_event_from_ev)
  ; ("test_event_from_ev_rel", `Quick, test_event_from_ev_rel)
  ; ("test_event_next_unblock", `Slow, event_next_unblock)
  ; ("test_event_next", `Slow, event_next_test)
  ; ("test_event_from", `Quick, event_from_test)
  ; ("test_event_from_parallel", `Slow, event_from_parallel_test)
  ; ("test_event_object_level_event", `Slow, object_level_event_test)
  ; ("test_short_oneshot", `Slow, test_short_oneshot)
  ]
