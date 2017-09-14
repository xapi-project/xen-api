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

type test =
  | Case  of name * description * case
  | Suite of name * description * suite
and name        = string
and description = string
and case        = unit -> unit
and suite       = test list

(** Indicates that the current test should be skipped. *)
exception Skip of string

(** Raised when a failure was expected, but not detected. *)
exception Failure_expected

(** Asserts that the given values are logically equal. If the values *)
(** are not equal, this function uses the optional string conversion *)
(** function to generate a failure message with the non-equal values.*)
val assert_equal : ?to_string:('a -> string) -> 'a -> 'a -> unit

(** Asserts that the given value is true. *)
val assert_true : bool -> unit

(** Asserts that the given value is false. *)
val assert_false : bool -> unit

(** Asserts that the given function raises an exception *)
(** that matches the given exception matching function. *)
val assert_raises_match : (exn -> bool) -> (unit -> 'a) -> unit

(** Asserts that the given function raises the given exception. *)
val assert_raises : exn -> (unit -> 'a) -> unit

(** Indicates that the current test has failed, with the given message. *)
val fail : string -> unit

(** Prints a debugging message, followed by a newline character. *)
val print_endline : string -> unit

(** Makes a test case. *)
val make_test_case : name -> description -> case -> test

(** Makes a function test case with a default description. *)
val make_function_test_case : name -> case -> test

(** Makes a test suite. *)
val make_test_suite : name -> description -> suite -> test

(** Makes a module test suite with a default description. *)
val make_module_test_suite : name -> suite -> test
