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

let test =
  [
    ("Create invalid", `Quick, test_create_invalid)
  ; ("Submit", `Slow, test_submit)
  ; ("Submit fairness", `Slow, test_submit_fairness)
  ; ("Submit sync", `Slow, test_submit_sync)
  ; ("Submit sync with queue", `Slow, test_submit_sync_with_queued_items)
  ; ("Submit sync concurrent", `Slow, test_submit_sync_concurrent)
  ; ("Submit sync interleaved", `Slow, test_submit_sync_interleaved)
  ]

let () = Alcotest.run "Rate limit library" [("Rate limit tests", test)]
