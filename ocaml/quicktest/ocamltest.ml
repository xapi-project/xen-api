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
(* A unit testing framework for OCaml.                        *)

open Printf

(* === Types === *)

type test =
  | Case  of name * description * case
  | Suite of name * description * suite
and name        = string
and description = string
and case        = unit -> unit
and suite       = test list

exception Failure_expected
exception Fail of string
exception Skip of string

(* === Equality assertions === *)

let assert_equal ?to_string x y =
  if not (x = y) then raise
      (Fail
         (match to_string with
          | None ->
            "found different values where equal values were expected."
          | Some to_string -> sprintf
                                "found different values where equal values were expected: %s != %s."
                                (to_string x) (to_string y)
         )
      )

(* === Other assertions === *)

let assert_true x = assert x

let assert_false x = assert (not x)

let assert_raises_match exception_match fn =
  try
    fn ();
    raise Failure_expected
  with failure ->
    if not (exception_match failure)
    then raise failure
    else ()

let assert_raises expected =
  assert_raises_match (function exn -> exn = expected)

let fail message = raise (Fail (message))

(** True if (and only if) the currently-executing  *)
(** test has generated one or more debug messages. *)
let debugging = ref false

let start_debugging () =
  if not !debugging then
    begin
      debugging := true;
      print_endline "\n"
    end

let print_endline string =
  start_debugging ();
  print_endline string;
  flush stdout

(* === Factories === *)

let make_test_case name description case =
  Case (name, description, case)

let make_function_test_case name case =
  Case (name, sprintf "Tests the %s function." name, case)

let make_test_suite name description suite =
  Suite (name, description, suite)

let make_module_test_suite name suite =
  Suite (name, sprintf "Tests the %s module." name, suite)
