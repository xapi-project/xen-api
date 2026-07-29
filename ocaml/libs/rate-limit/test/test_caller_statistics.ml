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

module Caller_statistics = Rate_limit_lib.Caller_statistics

let test_initial_state () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-1" in
  Alcotest.(check string)
    "uuid is stored" "uuid-1"
    (Caller_statistics.get_uuid cs) ;
  Alcotest.(check int)
    "initial call count is 0" 0
    (Caller_statistics.get_call_count cs) ;
  Alcotest.(check (float 0.0))
    "initial token count is 0.0" 0.0
    (Caller_statistics.get_token_count cs)

let test_single_register () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-2" in
  Caller_statistics.register_call ~token_amount:2.5 cs ;
  Alcotest.(check int)
    "count incremented" 1
    (Caller_statistics.get_call_count cs) ;
  Alcotest.(check (float 0.0))
    "tokens accumulated" 2.5
    (Caller_statistics.get_token_count cs)

(* Many threads, each registering many calls. The CAS retry loop must not lose
   any updates — the final counts must equal the sum of every thread's
   contributions. *)
let test_concurrent_register () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-4" in
  let num_threads = 16 in
  let calls_per_thread = 500 in
  let token_per_call = 0.25 in
  let threads =
    Array.init num_threads (fun _ ->
        Thread.create
          (fun () ->
            for _ = 1 to calls_per_thread do
              Caller_statistics.register_call ~token_amount:token_per_call cs
            done
          )
          ()
    )
  in
  Array.iter Thread.join threads ;
  let total_calls = num_threads * calls_per_thread in
  Alcotest.(check int)
    "no register_call updates were lost under contention" total_calls
    (Caller_statistics.get_call_count cs) ;
  Alcotest.(check (float 1e-6))
    "token sum matches total contributions"
    (float_of_int total_calls *. token_per_call)
    (Caller_statistics.get_token_count cs)

(* Readers running concurrently with writers should never observe a torn
   value: call_count and token_count come from the same atomic snapshot,
   so token_count >= call_count * token_per_call at every observation. *)
let test_concurrent_reads_see_consistent_snapshot () =
  let cs = Caller_statistics.create ~caller_uuid:"uuid-6" in
  let num_writers = 4 in
  let calls_per_writer = 1_000 in
  let token_per_call = 1.0 in
  let stop = Atomic.make false in
  let inconsistencies = Atomic.make 0 in
  let reader () =
    while not (Atomic.get stop) do
      let count = Caller_statistics.get_call_count cs in
      let tokens = Caller_statistics.get_token_count cs in
      let expected = float_of_int count *. token_per_call in
      if abs_float (tokens -. expected) > 1e-9 then Atomic.incr inconsistencies
    done
  in
  let writers =
    Array.init num_writers (fun _ ->
        Thread.create
          (fun () ->
            for _ = 1 to calls_per_writer do
              Caller_statistics.register_call ~token_amount:token_per_call cs
            done
          )
          ()
    )
  in
  let readers = Array.init 4 (fun _ -> Thread.create reader ()) in
  Array.iter Thread.join writers ;
  Atomic.set stop true ;
  Array.iter Thread.join readers ;
  Alcotest.(check int)
    "readers never observed a torn count/token snapshot" 0
    (Atomic.get inconsistencies) ;
  Alcotest.(check int)
    "all writer updates landed"
    (num_writers * calls_per_writer)
    (Caller_statistics.get_call_count cs)

let test =
  [
    ("Initial state", `Quick, test_initial_state)
  ; ("Single register updates all fields", `Quick, test_single_register)
  ; ( "Concurrent register_call loses no updates"
    , `Quick
    , test_concurrent_register
    )
  ; ( "Concurrent readers see a consistent snapshot"
    , `Quick
    , test_concurrent_reads_see_consistent_snapshot
    )
  ]

let () =
  Alcotest.run "Caller statistics library" [("Caller statistics tests", test)]
