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

open OUnit
open Test_common
open Event_types
open Stdext
open Threadext

let event_setup_common = Test_event_common.event_setup_common

let test_event_from_ev () =
  (* Test that creating an object generates an event for that object *)
  let __context, session_id = event_setup_common () in
  let evs = Xapi_event.from __context ["vm"] "" 30.0 |> parse_event_from in
  let tok = evs.token in
  let vm = make_vm __context () in
  let evs_rpc = Xapi_event.from __context ["vm"] tok 4.0 in
  let evs = evs_rpc |> parse_event_from in
  Printf.printf "evs: %s\n%!" (Jsonrpc.to_string evs_rpc);
  let ev = List.filter (fun ev -> ev.ty="vm") evs.events in
  assert_equal (List.length ev) 1;
  let ev = List.hd ev in
  assert_equal ev.reference (Ref.string_of vm)

let test_event_from_ev_rel () =
  (* Test that creating a connector object generates all relevant events *)
  let __context, session_id = event_setup_common () in
  let vm = make_vm __context () in
  let evs = Xapi_event.from __context ["vm";"vbd"] "" 30.0 |> parse_event_from in
  let tok = evs.token in
  let vbd = make_vbd ~__context ~vM:vm () in
  let evs_rpc = Xapi_event.from __context ["vm"; "vbd"] tok 4.0 in
  let evs2 = evs_rpc |> parse_event_from in
  let tok2 = evs2.token in
  let vm_ev = List.filter (fun ev -> ev.ty="vm") evs2.events in
  let vbd_ev = List.filter (fun ev -> ev.ty="vbd") evs2.events in
  assert_equal (List.length vm_ev) 1;
  assert_equal (List.length vbd_ev) 1;
  let ev = List.hd vm_ev in
  assert_equal ev.reference (Ref.string_of vm);
  Db.VBD.destroy ~__context ~self:vbd;
  let evs_rpc = Xapi_event.from __context ["vm"; "vbd"] tok2 4.0 in
  let evs3 = evs_rpc |> parse_event_from in
  let vm_ev = List.filter (fun ev -> ev.ty="vm") evs3.events in
  let vbd_ev = List.filter (fun ev -> ev.ty="vbd") evs3.events in
  assert_equal (List.length vm_ev) 1;
  assert_equal (List.length vbd_ev) 1

let test_event_from_timeout () =
  let __context, session_id = event_setup_common () in
  let evs = Xapi_event.from __context ["vm"] "" 30.0 |> parse_event_from in
  let tok = evs.token in
  let start_time = Unix.gettimeofday () in
  let _ = Xapi_event.from __context ["vm"] tok 1.0 |> parse_event_from in
  let end_time = Unix.gettimeofday () in
  let elapsed = end_time -. start_time in
  Printf.printf "test_event_from_timeout: elapsed=%f\n" elapsed;
  assert_bool "timeout correct" (elapsed < 2.0 && elapsed > 1.0)

let event_next_unblock () =
  let __context, session_id = event_setup_common () in
  let () = Xapi_event.register ~__context ~classes:[] in (* no events *)
  let m = Mutex.create () in
  let unblocked = ref false in
  let (_: Thread.t) = Thread.create
      (fun () ->
         begin
           try ignore(Xapi_event.next ~__context)
           with e ->
             Printf.printf "background thread caught: %s (an exception is expected)" (Printexc.to_string e)
         end;
         Mutex.execute m (fun () -> unblocked := true)
      ) () in
  (* Background thread is started but it cannot simultaneously block and signal us to
     logout so a little pause in here is probably the best we can do *)
  Thread.delay 0.5;
  (* Logout which should cause the background thread to unblock *)
  Xapi_session.destroy_db_session __context session_id;
  (* Again we can't tell the difference between a slow and a totally blocked thread
     so a little pause in here is also required *)
  Thread.delay 0.5;
  assert_bool "Unblocked" (Mutex.execute m (fun () -> !unblocked))

let event_next_test () =
  let __context, session_id = event_setup_common () in
  let () = Xapi_event.register ~__context ~classes:[ "pool" ] in
  let m = Mutex.create () in
  let finished = ref false in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  let key = "event_next_test" in
  begin try Db.Pool.remove_from_other_config __context pool key with _ -> () end;
  let (_: Thread.t) = Thread.create
      (fun () ->
         while not (Mutex.execute m (fun () -> !finished)) do
           ignore (Xapi_event.next __context);
           let oc = Db.Pool.get_other_config __context pool in
           if List.mem_assoc key oc && (List.assoc key oc) = "1"
           then Mutex.execute m (fun () ->
               Printf.printf "got expected event";
               finished := true;
             )
         done
      ) () in
  Thread.delay 1.;
  Db.Pool.add_to_other_config __context pool key "1";
  Thread.delay 1.;
  assert_bool "checking other_config" (Mutex.execute m (fun () -> !finished))

let wait_for_pool_key __context key =
  let token = ref "" in
  let finished = ref false in
  let pool = Db.Pool.get_all ~__context |> List.hd in
  while not !finished do
    let events = Xapi_event.from __context [ "pool" ] (!token) 10. |> parse_event_from in
    token := events.token;
    let oc = Db.Pool.get_other_config __context pool in
    if List.mem_assoc key oc && (List.assoc key oc) = "1" then finished := true;
  done

let event_from_test () =
  let __context, session_id = event_setup_common () in
  let m = Mutex.create () in
  let finished = ref false in
  let pool = Db.Pool.get_all __context |> List.hd in
  let key = "event_from_test" in
  begin try Db.Pool.remove_from_other_config __context pool key with _ -> () end;
  let (_: Thread.t) = Thread.create
      (fun () ->
         wait_for_pool_key __context key;
         Mutex.execute m (fun () -> finished := true)
      ) () in
  Thread.delay 0.5;
  Db.Pool.add_to_other_config __context pool key "1";
  Thread.delay 0.5;
  assert_bool "event_from_test" (Mutex.execute m (fun () -> !finished))

let event_from_parallel_test () =
  let __context, session_id = event_setup_common () in
  let pool = Db.Pool.get_all __context |> List.hd in
  let key = "event_next_test" in
  begin try Db.Pool.remove_from_other_config __context pool key with _ -> () end;
  let ok = ref true in
  let (i_should_succeed: Thread.t) = Thread.create
      (fun () ->
         try
           let _ = Xapi_event.from __context [] "" 2.0 in
           () (* good *)
         with e ->
           Printf.printf "Caught unexpected error: %s\n" (ExnHelper.string_of_exn e);
           ok := false;
      ) () in
  let (interfering_thread: Thread.t) = Thread.create
      (fun () ->
         wait_for_pool_key __context key
      ) () in
  Thread.delay 0.5; (* wait for both threads to block in Event.from *)
  Db.Pool.add_to_other_config __context pool key "1";
  Thread.join interfering_thread;
  Thread.join i_should_succeed;
  (* Check that Event.from didn't get cancelled by mistake *)
  assert_bool "event_from_parallel_test" !ok

let object_level_event_test session_id =
  let __context, session_id = event_setup_common () in
  let m = Mutex.create () in
  let finished = ref false in
  let failure = ref false in
  let vm_a = make_vm ~__context ~name_label:"vm_a" () in
  let vm_b = make_vm ~__context ~name_label:"vm_b" () in

  Printf.printf "watching %s\n%!" (Ref.string_of vm_a);
  Printf.printf "ignoring %s\n%!" (Ref.string_of vm_b);

  let key = "object_level_event_next" in

  begin try Db.VM.remove_from_other_config __context vm_a key with _ -> () end;
  begin try Db.VM.remove_from_other_config __context vm_b key with _ -> () end;

  let (_: Thread.t) = Thread.create
      (fun () ->
         let token = ref "" in
         while not (Mutex.execute m (fun () -> !finished)) do
           Printf.printf "Calling event.from...\n%!";
           let events = Xapi_event.from __context [ Printf.sprintf "vm/%s" (Ref.string_of vm_a) ] (!token) 10. |> parse_event_from in
           Printf.printf "Got %d events\n%!" (List.length events.events);
           List.iter
             (fun event ->
                if event.reference <> Ref.string_of vm_a then begin
                  Printf.printf "FAILURE: event on %s which we aren't watching\n%!" event.reference;
                  Mutex.execute m
                    (fun () ->
                       failure := true;
                       finished := true;
                    )
                end
             ) events.events;
           token := events.token;
           let oc = Db.VM.get_other_config __context vm_a in
           if List.mem_assoc key oc && (List.assoc key oc) = "1"
           then Mutex.execute m (fun () ->
               Printf.printf "got expected event (new token = %s)\n%!" !token;
               finished := true;
             )
           else Printf.printf "Db doesn't have expected change in...\n%!";
         done
      ) () in
  Thread.delay 0.5;
  Printf.printf "Adding to vm_b\n%!";
  Db.VM.add_to_other_config __context vm_b key "1";
  Thread.delay 0.5;
  Printf.printf "Removing from vm_b\n%!";
  Db.VM.remove_from_other_config __context vm_b key;
  Printf.printf "Adding to vm_a. This ought to wake up the event thread\n%!";
  Db.VM.add_to_other_config __context vm_a key "1";
  Thread.delay 1.0;
  Mutex.execute m
    (fun () ->
       if not !finished then begin
         Printf.printf "FAILURE: Didn't get expected change in event thread\n%!";
         failure := true;
       end);
  Mutex.execute m
    (fun () ->
       if (!failure) then begin
         assert_bool "failed to see object-level event change" false
       end
    )

let test =
  "test_event" >:::
  [
    "test_event_from_timeout" >:: test_event_from_timeout;
    "test_event_from_ev" >:: test_event_from_ev;
    "test_event_from_ev_rel" >:: test_event_from_ev_rel;
    "test_event_next_unblock" >:: event_next_unblock;
    "test_event_next" >:: event_next_test;
    "test_event_from" >:: event_from_test;
    "test_event_from_parallel" >:: event_from_parallel_test;
    "test_event_object_level_event" >:: object_level_event_test;
  ]
