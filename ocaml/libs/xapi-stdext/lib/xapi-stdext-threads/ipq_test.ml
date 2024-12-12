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

module Ipq = Xapi_stdext_threads_scheduler.Ipq

(* test we get "out of bound" exception calling Ipq.remove *)
let test_out_of_index () =
  let q = Ipq.create 10 0 in
  Ipq.add q {Ipq.ev= 123; Ipq.time= Mtime_clock.elapsed ()} ;
  let is_oob = function
    | Invalid_argument s when String.ends_with ~suffix:"  out of bounds" s ->
        true
    | _ ->
        false
  in
  let oob_check n =
    (Alcotest.match_raises "out of bound" is_oob @@ fun () -> Ipq.remove q n) ;
    Alcotest.(check bool) "same value" false (Ipq.is_empty q)
  in
  oob_check 10 ;
  oob_check (-1) ;
  oob_check 9 ;
  oob_check 1 ;
  (* this should succeed *)
  Ipq.remove q 0

(* check queue does not retain some data after being removed *)
let test_leak () =
  let default () = () in
  let q = Ipq.create 10 default in
  let array = Array.make 1024 'x' in
  let use_array () = array.(0) <- 'a' in
  let allocated = Atomic.make true in
  Gc.finalise (fun _ -> Atomic.set allocated false) array ;
  Ipq.add q {Ipq.ev= use_array; Ipq.time= Mtime_clock.elapsed ()} ;
  Ipq.remove q 0 ;
  Gc.full_major () ;
  Gc.full_major () ;
  Alcotest.(check bool) "allocated" false (Atomic.get allocated) ;
  Ipq.add q {Ipq.ev= default; Ipq.time= Mtime_clock.elapsed ()}

(* test Ipq.is_empty call *)
let test_empty () =
  let q = Ipq.create 10 0 in
  Alcotest.(check bool) "same value" true (Ipq.is_empty q) ;
  Ipq.add q {Ipq.ev= 123; Ipq.time= Mtime_clock.elapsed ()} ;
  Alcotest.(check bool) "same value" false (Ipq.is_empty q) ;
  Ipq.remove q 0 ;
  Alcotest.(check bool) "same value" true (Ipq.is_empty q)

module Int64Set = Set.Make (Int64)

let check = Ipq.check

(* get size of the queue *)
let size queue =
  let l = ref 0 in
  Ipq.iter (fun _ -> l := !l + 1) queue ;
  !l

(* get a set of times from the queue *)
let set queue =
  let s = ref Int64Set.empty in
  Ipq.iter
    (fun d ->
      let t = d.time in
      let t = Mtime.Span.to_uint64_ns t in
      s := Int64Set.add t !s
    )
    queue ;
  !s

let test_old () =
  let test : int Ipq.t = Ipq.create 100 0 in
  let s = ref Int64Set.empty in
  let add i =
    let ti = Random.int64 1000000L in
    let t = Mtime.Span.of_uint64_ns ti in
    let e = {Ipq.time= t; Ipq.ev= i} in
    Ipq.add test e ;
    s := Int64Set.add ti !s
  in
  for i = 0 to 49 do
    add i
  done ;
  let first_half = set test in
  for i = 50 to 99 do
    add i
  done ;
  check test ;
  (* we should have all elements *)
  Alcotest.(check int) "100 elements" 100 (size test) ;

  let all = set test in
  Alcotest.(check int) "same list" 0 (Int64Set.compare !s all) ;

  (* remove half of the elements *)
  for i = 0 to 49 do
    let xx = Ipq.find test i in
    Printf.printf "Removing element %d position %d\n%!" i xx ;
    Ipq.remove test xx ;
    check test
  done ;
  Alcotest.(check int) "50 elements" 50 (size test) ;

  (* make sure we have the right elements in the list *)
  let s = set test in
  let second_half = Int64Set.diff all first_half in
  Alcotest.(check int) "same list" 0 (Int64Set.compare s second_half) ;

  (* remove test *)
  let prev = ref 0L in
  for _ = 0 to 49 do
    let e = Ipq.pop_maximum test in
    let t = Mtime.Span.to_uint64_ns e.time in
    Alcotest.(check bool)
      (Printf.sprintf "%Ld bigger than %Ld" t !prev)
      true (t >= !prev) ;
    Printf.printf "time: %Ld, site: %d\n" t e.ev ;
    prev := t ;
    check test
  done

let tests =
  [
    ("test_out_of_index", `Quick, test_out_of_index)
  ; ("test_leak", `Quick, test_leak)
  ; ("test_empty", `Quick, test_empty)
  ; ("test_old", `Quick, test_old)
  ]

let () = Alcotest.run "Ipq" [("generic", tests)]
