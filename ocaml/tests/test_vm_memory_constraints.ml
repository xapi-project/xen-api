(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Printf
module C = Vm_memory_constraints.Vm_memory_constraints

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( ** ) = Int64.mul
let ( // ) = Int64.div

(** Creates a memory constraints record (with values in bytes) from the given
    memory constraints tuple (with values in MiB). *)
let create (static_min, dynamic_min, target, dynamic_max, static_max) =
  let scale value = (Int64.of_int value) ** 1024L ** 1024L in
  C.{
    static_min  = scale static_min ;
    dynamic_min = scale dynamic_min;
    target      = scale target     ;
    dynamic_max = scale dynamic_max;
    static_max  = scale static_max ;
  }

let constraints_pinned = [
  (0,0,0,0,5); (0,1,1,1,5);
  (0,2,2,2,5); (0,3,3,3,5);
  (0,4,4,4,5); (0,5,5,5,5);
]

let constraints_unpinned = [
  (0,0,0,1,5); (0,1,1,2,5);
  (0,2,2,3,5); (0,2,3,3,5);
  (0,3,4,4,5); (0,4,5,5,5);
]

let constraints_valid = [
  (0,1,2,3,4); (1,2,3,4,5);
  (1,1,2,3,4); (1,2,3,4,4);
  (2,2,2,3,4); (1,2,3,3,3);
  (3,3,3,3,4); (1,2,2,2,2);
  (4,4,4,4,4); (1,1,1,1,1);
]

let constraints_invalid = [
  (2,1,2,3,4); (4,1,2,3,4);
  (5,1,2,3,4); (0,4,2,3,4);
  (0,5,2,3,4); (0,1,2,5,4);
]

let constraints_pinned_at_static_max = [
  (0,0,0,0,0); (0,1,1,1,1);
  (0,2,2,2,2); (1,2,2,2,2);
]

(** Tests that [fn i] evaluates to [output] for all values [i] in [inputs]. *)
let test_indicator_function fn fn_name output output_name inputs =
    (sprintf "%s_%s" fn_name output_name),
    `Quick,
    (fun () ->
       List.iter
         (fun i ->
            Alcotest.check (Alcotest_comparators.only_compare ()) "return value"
              output
              (fn ~constraints:(create i))
         )
         (inputs))

let test_are_pinned_true = test_indicator_function
    C.are_pinned "are_pinned" true "true" constraints_pinned
let test_are_pinned_false = test_indicator_function
    C.are_pinned "are_pinned" false "false" constraints_unpinned
let test_are_valid_true = test_indicator_function
    C.are_valid "are_valid" true "true" constraints_valid
let test_are_valid_false = test_indicator_function
    C.are_valid "are_valid" false "false" constraints_invalid
let test_are_valid_and_pinned_at_static_max_true = test_indicator_function
    C.are_valid_and_pinned_at_static_max "are_valid_and_pinned_at_static_max"
    true "true" constraints_pinned_at_static_max
let test_are_valid_and_pinned_at_static_max_false = test_indicator_function
    C.are_valid_and_pinned_at_static_max "are_valid_and_pinned_at_static_max"
    false "false" (constraints_invalid @ constraints_unpinned)

let test_reset_to_safe_defaults () =
  List.iter
    (fun (input, output) ->
       let reset constraints = C.reset_to_safe_defaults ~constraints in
       Alcotest.check (Alcotest_comparators.only_compare ()) "same constraints"
         (reset (create input))
         (create output))
    [
      ( 256, 512,1024,2048,4096), ( 256,4096,4096,4096,4096);
      (4096,2048,1024, 512, 256), ( 256, 256, 256, 256, 256);
      (1024,1024,1024,1024,1024), (1024,1024,1024,1024,1024);
    ]

let test =
    [
      test_are_pinned_true;
      test_are_pinned_false;
      test_are_valid_true;
      test_are_valid_false;
      test_are_valid_and_pinned_at_static_max_true;
      test_are_valid_and_pinned_at_static_max_false;
      "test_reset_to_safe_defaults", `Quick, test_reset_to_safe_defaults;
    ]

