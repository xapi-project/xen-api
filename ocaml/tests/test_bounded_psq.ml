(*
 * Copyright (C) Cloud Software Group
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

module BPSQ = Helpers.BoundedPsq.Make (Int) (Int)

(* Create a BPSQ of specified capacity, attempt to insert a specified
   number of (distinct) elements, and then compare the expected size
   of the queue against the actual size of the queue. *)
let test_bpsq_n_capacity ~capacity ~to_insert ~size_after =
  let psq = BPSQ.create ~capacity in
  for i = 0 to to_insert do
    BPSQ.add psq i i
  done ;
  let size = BPSQ.size psq in
  Alcotest.(check' int)
    ~msg:"Number of elements is as expected" ~expected:size_after ~actual:size

(* A negative capacity BPSQ should behave as though 0 were specified
   as the capacity. *)
let test_bpsq_lte_zero_capacity () =
  let size_after = 0 in
  let to_insert = 10 in
  for capacity = 0 downto -15 do
    test_bpsq_n_capacity ~capacity ~to_insert ~size_after
  done

(* Exercises property that cache size reaches capacity and then remains fixed. *)
let test_bpsq_fixed_capacity () =
  let capacity = 40 in
  let to_insert = capacity * 2 in
  test_bpsq_n_capacity ~capacity ~to_insert ~size_after:capacity

(* This test creates a BPSQ of capacity x and inserts elements [1..y] in increasing order, where y > x.
   It must be the case that the insertion of elements [x+1..y] displaces
   the entries [1..(y-x)] already in the queue. This is because the
   queue evicts the highest priority (minimum value) entry when an
   entry is competing for space. The behaviour is akin to wrapping around a ring buffer. *)
let test_bpsq_competing_priority () =
  let x = 40 in
  let extra = 20 in
  let y = x + extra in
  (* Create a BPSQ with capacity x < y. *)
  let psq = BPSQ.create ~capacity:x in
  (* Insert entry i => i, for each i in [1..y]. *)
  for i = 1 to y do
    BPSQ.add psq i i
  done ;
  let expectation = function
    (* The insertion of the extras should have evicted the highest priority (lowest) entries.
       All of [1..extra] should not be present. *)
    | i when i >= 1 && i <= extra ->
        false
    (* All entries [extra+1, y] should be present. *)
    | i when i > extra && i <= y ->
        true
    | _ ->
        false
  in
  (* Iterate each i in [1..y] and ensure our expectations are satisfied. *)
  for i = 1 to y do
    let present = BPSQ.contains psq i in
    let should_be_present = expectation i in
    if present <> should_be_present then
      let polarity = if should_be_present then "" else " not" in
      failwith
        (Printf.sprintf
           "Queue membership expectations violated, %d should%s be present" i
           polarity
        )
  done

let () =
  let open Alcotest in
  run "Bounded PSQ"
    [
      ( "Bounded PSQ"
      , [
          test_case "<= Zero Capacity" `Quick test_bpsq_lte_zero_capacity
        ; test_case "Fixed Capacity" `Quick test_bpsq_fixed_capacity
        ; test_case "Competing Priorities" `Quick test_bpsq_competing_priority
        ]
      )
    ]
