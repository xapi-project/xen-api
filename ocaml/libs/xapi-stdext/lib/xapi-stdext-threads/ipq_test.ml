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
  Ipq.add q {Ipq.ev= 123; Ipq.time= Mtime_clock.now ()} ;
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
  Ipq.add q {Ipq.ev= use_array; Ipq.time= Mtime_clock.now ()} ;
  Ipq.remove q 0 ;
  Gc.full_major () ;
  Gc.full_major () ;
  Alcotest.(check bool) "allocated" false (Atomic.get allocated) ;
  Ipq.add q {Ipq.ev= default; Ipq.time= Mtime_clock.now ()}

(* test Ipq.is_empty call *)
let test_empty () =
  let q = Ipq.create 10 0 in
  Alcotest.(check bool) "same value" true (Ipq.is_empty q) ;
  Ipq.add q {Ipq.ev= 123; Ipq.time= Mtime_clock.now ()} ;
  Alcotest.(check bool) "same value" false (Ipq.is_empty q) ;
  Ipq.remove q 0 ;
  Alcotest.(check bool) "same value" true (Ipq.is_empty q)

let tests =
  [
    ("test_out_of_index", `Quick, test_out_of_index)
  ; ("test_leak", `Quick, test_leak)
  ; ("test_empty", `Quick, test_empty)
  ]

let () = Alcotest.run "Ipq" [("generic", tests)]
