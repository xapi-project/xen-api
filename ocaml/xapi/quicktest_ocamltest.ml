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
(* Functions for running ocamltest tests as part of quicktest. *)

open Ocamltest
open Printf
open Quicktest_common

type test_result = passed * failed
and passed = int
and failed = int

let add_result (passed, failed) (passed', failed') =
  (passed + passed', failed + failed')

let run_from_within_quicktest (test : test) =

  let rec run (test : test) (level : int) =
    match test with
    | Case (name, description, fn) ->
      run_case (name, description, fn) level
    | Suite (name, description, tests) ->
      run_suite (name, description, tests) level

  and run_case (name, description, fn) level =
    let test = make_test (sprintf "Testing '%s'" name) level in
    start test;
    try
      fn ();
      success test;
      (1, 0)
    with failure ->
      debug test (Backtrace.(to_string_hum (get failure)));
      failed test (sprintf "Failed with %s" (Printexc.to_string failure));
      (0, 1)

  and run_suite (name, description, tests) level =
    let test = make_test (sprintf "Testing '%s'" name) level in
    start test;
    print_endline "";
    let result = List.fold_left (
        fun accumulating_result test ->
          add_result accumulating_result (run test (level + 4))
      ) (0, 0) tests in
    debug test (sprintf "Finished testing '%s'" name);
    begin match result with
      | (_, 0) ->
        success test
      | (_, failure_count) ->
        let failure_description = sprintf "Detected %i failure%s"
            failure_count (if failure_count = 1 then "" else "s") in
        failed test failure_description
    end;
    result

  in let (_: int*int) = run test 0 in ()
