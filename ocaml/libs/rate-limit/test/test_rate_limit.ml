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

module Rate_limit = Rate_limit_lib.Rate_limit

let test_create_invalid () =
  Alcotest.match_raises "Creating with zero fill rate should raise"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Rate_limit.create ~burst_size:10.0 ~fill_rate:0.0)) ;
  Alcotest.match_raises "Creating with negative fill rate should raise"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Rate_limit.create ~burst_size:10.0 ~fill_rate:(-1.0)))

let test_submit () =
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  (* Drain the bucket *)
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 10.0 ;
  let executed = ref false in
  let start_counter = Mtime_clock.counter () in
  Rate_limit.submit_async rl ~callback:(fun () -> executed := true) 5.0 ;
  let elapsed_span = Mtime_clock.count start_counter in
  let elapsed_seconds = Mtime.Span.to_float_ns elapsed_span *. 1e-9 in
  (* submit should return immediately (non-blocking) *)
  Alcotest.(check bool) "submit returns immediately" true (elapsed_seconds < 0.1) ;
  (* Wait for callback to be executed by worker *)
  Thread.delay 0.6 ;
  Alcotest.(check bool) "callback eventually executed" true !executed ;
  Rate_limit.delete rl

let test_submit_fairness () =
  (* Test that callbacks are executed in FIFO order regardless of token cost *)
  let rl = Rate_limit.create ~burst_size:5.0 ~fill_rate:5.0 in
  (* Drain the bucket *)
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 5.0 ;
  let execution_order = ref [] in
  let order_mutex = Mutex.create () in
  let record_execution id =
    Mutex.lock order_mutex ;
    execution_order := id :: !execution_order ;
    Mutex.unlock order_mutex
  in
  (* Submit callbacks with varying costs - order should be preserved *)
  Rate_limit.submit_async rl ~callback:(fun () -> record_execution 1) 1.0 ;
  Rate_limit.submit_async rl ~callback:(fun () -> record_execution 2) 3.0 ;
  Rate_limit.submit_async rl ~callback:(fun () -> record_execution 3) 1.0 ;
  Rate_limit.submit_async rl ~callback:(fun () -> record_execution 4) 2.0 ;
  (* Wait for all callbacks to complete (total cost = 7 tokens, rate = 5/s) *)
  Thread.delay 2.0 ;
  let order = List.rev !execution_order in
  Alcotest.(check (list int))
    "callbacks execute in FIFO order" [1; 2; 3; 4] order ;
  Rate_limit.delete rl

let test_submit_sync () =
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  (* Test 1: Returns callback result immediately when tokens available *)
  let result = Rate_limit.submit_sync rl ~callback:(fun () -> 42) 5.0 in
  Alcotest.(check int) "returns callback result" 42 result ;
  (* Test 2: Blocks and waits for tokens, then returns result *)
  (* Drain the bucket *)
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 5.0 ;
  let start_counter = Mtime_clock.counter () in
  let result2 = Rate_limit.submit_sync rl ~callback:(fun () -> "hello") 5.0 in
  let elapsed_span = Mtime_clock.count start_counter in
  let elapsed_seconds = Mtime.Span.to_float_ns elapsed_span *. 1e-9 in
  Alcotest.(check string) "returns string result" "hello" result2 ;
  Alcotest.(check bool)
    "blocked waiting for tokens" true (elapsed_seconds >= 0.4) ;
  Rate_limit.delete rl

let test_submit_sync_with_queued_items () =
  (* Test that submit_sync respects FIFO ordering when queue has items *)
  let rl = Rate_limit.create ~burst_size:5.0 ~fill_rate:10.0 in
  (* Drain the bucket *)
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 5.0 ;
  let execution_order = ref [] in
  let order_mutex = Mutex.create () in
  let record_execution id =
    Mutex.lock order_mutex ;
    execution_order := id :: !execution_order ;
    Mutex.unlock order_mutex
  in
  (* Submit async items first *)
  Rate_limit.submit_async rl ~callback:(fun () -> record_execution 1) 1.0 ;
  Rate_limit.submit_async rl ~callback:(fun () -> record_execution 2) 1.0 ;
  (* Now submit_sync should queue behind the async items *)
  let result =
    Rate_limit.submit_sync rl
      ~callback:(fun () -> record_execution 3 ; "sync_result")
      1.0
  in
  Alcotest.(check string)
    "submit_sync returns correct result" "sync_result" result ;
  let order = List.rev !execution_order in
  Alcotest.(check (list int))
    "submit_sync executes after queued items" [1; 2; 3] order ;
  Rate_limit.delete rl

let test_submit_sync_concurrent () =
  (* Test multiple concurrent submit_sync calls *)
  let rl = Rate_limit.create ~burst_size:1.0 ~fill_rate:10.0 in
  (* Drain the bucket to force queueing *)
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 1.0 ;
  let results = Array.make 5 0 in
  let threads =
    Array.init 5 (fun i ->
        Thread.create
          (fun () ->
            let r = Rate_limit.submit_sync rl ~callback:(fun () -> i + 1) 1.0 in
            results.(i) <- r
          )
          ()
    )
  in
  Array.iter Thread.join threads ;
  (* Each thread should get its own result back *)
  for i = 0 to 4 do
    Alcotest.(check int)
      (Printf.sprintf "thread %d gets correct result" i)
      (i + 1) results.(i)
  done ;
  Rate_limit.delete rl

let test_no_skip_ahead_during_worker_delay () =
  (* A caller arriving while the worker is delaying the only queued item
     must not skip ahead of it. The queued item has a large cost so the
     worker's delay is long enough for the bucket to refill enough that a
     newly-arriving cheap caller could opportunistically consume without
     the fix. *)
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:100.0 in
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 10.0 ;
  let execution_order = ref [] in
  let order_mutex = Mutex.create () in
  let record_execution id =
    Mutex.lock order_mutex ;
    execution_order := id :: !execution_order ;
    Mutex.unlock order_mutex
  in
  Rate_limit.submit_async rl ~callback:(fun () -> record_execution 1) 10.0 ;
  Thread.delay 0.02 ;
  Rate_limit.submit_async rl ~callback:(fun () -> record_execution 2) 1.0 ;
  Thread.delay 0.2 ;
  let order = List.rev !execution_order in
  Alcotest.(check (list int))
    "late arrival does not overtake queued item" [1; 2] order ;
  Rate_limit.delete rl

let test_submit_sync_interleaved () =
  (* Test interleaving submit and submit_sync *)
  let rl = Rate_limit.create ~burst_size:2.0 ~fill_rate:10.0 in
  (* Drain the bucket *)
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 2.0 ;
  let async_executed = ref false in
  (* Submit async first *)
  Rate_limit.submit_async rl ~callback:(fun () -> async_executed := true) 1.0 ;
  (* Submit sync should wait for async to complete first *)
  let sync_result =
    Rate_limit.submit_sync rl ~callback:(fun () -> !async_executed) 1.0
  in
  Alcotest.(check bool)
    "sync callback sees async already executed" true sync_result ;
  Rate_limit.delete rl

(* A recording observer used by the tests below. [event] captures ordering of
   observer callbacks relative to the user callback via monotonically-increasing
   timestamps, which is what we want to assert. *)
let make_recording_observer () =
  let mtx = Mutex.create () in
  let started = ref 0 in
  let ended = ref 0 in
  let start_time = ref None in
  let end_time = ref None in
  let stamp r =
    Mutex.lock mtx ;
    r := Some (Mtime_clock.now ()) ;
    Mutex.unlock mtx
  in
  let bump c = Mutex.lock mtx ; incr c ; Mutex.unlock mtx in
  let observer =
    Rate_limit.
      {
        on_start= (fun () -> bump started ; stamp start_time)
      ; on_end= (fun () -> bump ended ; stamp end_time)
      }
  in
  (observer, fun () -> (!started, !ended, !start_time, !end_time))

let test_observer_not_fired_when_immediate () =
  let rl = Rate_limit.create ~burst_size:10.0 ~fill_rate:10.0 in
  let observer, snapshot = make_recording_observer () in
  let result =
    Rate_limit.submit_sync rl ~observer ~callback:(fun () -> 42) 1.0
  in
  Alcotest.(check int) "callback ran" 42 result ;
  let started, ended, _, _ = snapshot () in
  Alcotest.(check int) "on_start not fired" 0 started ;
  Alcotest.(check int) "on_end not fired" 0 ended ;
  let observer2, snapshot2 = make_recording_observer () in
  Rate_limit.submit_async rl ~observer:observer2 ~callback:(fun () -> ()) 1.0 ;
  let started2, ended2, _, _ = snapshot2 () in
  Alcotest.(check int) "async on_start not fired" 0 started2 ;
  Alcotest.(check int) "async on_end not fired" 0 ended2 ;
  Rate_limit.delete rl

let test_observer_fired_when_delayed_sync () =
  let rl = Rate_limit.create ~burst_size:1.0 ~fill_rate:5.0 in
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 1.0 ;
  let observer, snapshot = make_recording_observer () in
  let callback_seen_end = ref false in
  let ended_before_cb = ref false in
  let _ =
    Rate_limit.submit_sync rl ~observer
      ~callback:(fun () ->
        let _, ended, _, _ = snapshot () in
        ended_before_cb := ended = 1 ;
        callback_seen_end := true
      )
      1.0
  in
  let started, ended, start_time, end_time = snapshot () in
  Alcotest.(check int) "on_start fired once" 1 started ;
  Alcotest.(check int) "on_end fired once" 1 ended ;
  Alcotest.(check bool) "callback ran" true !callback_seen_end ;
  Alcotest.(check bool)
    "on_end observed before callback runs" true !ended_before_cb ;
  ( match (start_time, end_time) with
  | Some s, Some e ->
      Alcotest.(check bool)
        "on_start precedes on_end" true
        (Mtime.is_earlier s ~than:e || Mtime.equal s e)
  | _ ->
      Alcotest.fail "timestamps missing"
  ) ;
  Rate_limit.delete rl

let test_observer_fired_when_delayed_async () =
  let rl = Rate_limit.create ~burst_size:1.0 ~fill_rate:5.0 in
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 1.0 ;
  let observer, snapshot = make_recording_observer () in
  let done_mtx = Mutex.create () in
  let done_cond = Condition.create () in
  let ran = ref false in
  let ended_before_cb = ref false in
  Rate_limit.submit_async rl ~observer
    ~callback:(fun () ->
      let _, ended, _, _ = snapshot () in
      ended_before_cb := ended = 1 ;
      Mutex.lock done_mtx ;
      ran := true ;
      Condition.signal done_cond ;
      Mutex.unlock done_mtx
    )
    1.0 ;
  let started_immediately, _, _, _ = snapshot () in
  Alcotest.(check int)
    "on_start fires synchronously on caller thread" 1 started_immediately ;
  Mutex.lock done_mtx ;
  while not !ran do
    Condition.wait done_cond done_mtx
  done ;
  Mutex.unlock done_mtx ;
  let started, ended, _, _ = snapshot () in
  Alcotest.(check int) "on_start fired once" 1 started ;
  Alcotest.(check int) "on_end fired once" 1 ended ;
  Alcotest.(check bool)
    "on_end observed before callback ran" true !ended_before_cb ;
  Rate_limit.delete rl

let test_observer_exception_isolated () =
  let rl = Rate_limit.create ~burst_size:1.0 ~fill_rate:20.0 in
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 1.0 ;
  let raising_observer =
    Rate_limit.
      {
        on_start= (fun () -> failwith "boom on_start")
      ; on_end= (fun () -> failwith "boom on_end")
      }
  in
  (* Async: raising observer must not stop the callback or crash the worker. *)
  let done_mtx = Mutex.create () in
  let done_cond = Condition.create () in
  let ran_first = ref false in
  Rate_limit.submit_async rl ~observer:raising_observer
    ~callback:(fun () ->
      Mutex.lock done_mtx ;
      ran_first := true ;
      Condition.signal done_cond ;
      Mutex.unlock done_mtx
    )
    1.0 ;
  Mutex.lock done_mtx ;
  while not !ran_first do
    Condition.wait done_cond done_mtx
  done ;
  Mutex.unlock done_mtx ;
  Alcotest.(check bool)
    "async callback ran despite observer raising" true !ran_first ;
  (* Now confirm the worker is still healthy and processes further items. *)
  let ran_second = ref false in
  Rate_limit.submit_async rl
    ~callback:(fun () ->
      Mutex.lock done_mtx ;
      ran_second := true ;
      Condition.signal done_cond ;
      Mutex.unlock done_mtx
    )
    1.0 ;
  Mutex.lock done_mtx ;
  while not !ran_second do
    Condition.wait done_cond done_mtx
  done ;
  Mutex.unlock done_mtx ;
  Alcotest.(check bool) "worker still processes later items" true !ran_second ;
  (* Sync: raising observer must not reach the caller as an exception. *)
  Rate_limit.submit_async rl ~callback:(fun () -> ()) 1.0 ;
  let sync_ran = ref false in
  let sync_result =
    Rate_limit.submit_sync rl ~observer:raising_observer
      ~callback:(fun () ->
        sync_ran := true ;
        7
      )
      1.0
  in
  Alcotest.(check int) "sync callback returned normally" 7 sync_result ;
  Alcotest.(check bool) "sync callback ran" true !sync_ran ;
  Rate_limit.delete rl

let test =
  [
    ("Create invalid", `Quick, test_create_invalid)
  ; ("Submit", `Slow, test_submit)
  ; ("Submit fairness", `Slow, test_submit_fairness)
  ; ("Submit sync", `Slow, test_submit_sync)
  ; ("Submit sync with queue", `Slow, test_submit_sync_with_queued_items)
  ; ("Submit sync concurrent", `Slow, test_submit_sync_concurrent)
  ; ("Submit sync interleaved", `Slow, test_submit_sync_interleaved)
  ; ( "No skip-ahead during worker delay"
    , `Slow
    , test_no_skip_ahead_during_worker_delay
    )
  ; ( "Observer not fired when immediate"
    , `Quick
    , test_observer_not_fired_when_immediate
    )
  ; ( "Observer fired when delayed sync"
    , `Slow
    , test_observer_fired_when_delayed_sync
    )
  ; ( "Observer fired when delayed async"
    , `Slow
    , test_observer_fired_when_delayed_async
    )
  ; ("Observer exception isolated", `Slow, test_observer_exception_isolated)
  ]

let () = Alcotest.run "Rate limit library" [("Rate limit tests", test)]
