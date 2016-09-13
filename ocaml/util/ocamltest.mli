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

(** Returns true if and only if the given function raises no exceptions. *)
val successful : (unit -> 'a) -> bool

(** Asserts that the given values are logically equal. If the values *)
(** are not equal, this function uses the optional string conversion *)
(** function to generate a failure message with the non-equal values.*)
val assert_equal : ?to_string:('a -> string) -> 'a -> 'a -> unit

(** Asserts that the given Boolean values are equal. *)
val assert_equal_bool : bool -> bool -> unit

(** Asserts that the given floating-point values are equal. *)
val assert_equal_float : float -> float -> unit

(** Asserts that the given integer values are equal. *)
val assert_equal_int : int -> int -> unit

(** Asserts that the given 32-bit integer values are equal. *)
val assert_equal_int32 : int32 -> int32 -> unit

(** Asserts that the given 64-bit integer values are equal. *)
val assert_equal_int64 : int64 -> int64 -> unit

(** Asserts that the given strings are equal. *)
val assert_equal_string : string -> string -> unit

(** Asserts that the given value is true. *)
val assert_true : bool -> unit

(** Asserts that the given value is false. *)
val assert_false : bool -> unit

(** Asserts that the given function raises an exception *)
(** that matches the given exception matching function. *)
val assert_raises_match : (exn -> bool) -> (unit -> 'a) -> unit

(** Asserts that the given function raises the given exception. *)
val assert_raises : exn -> (unit -> 'a) -> unit

(** Asserts that the given function fails with any exception. *)
val assert_raises_any : (unit -> 'a) -> unit

(** Indicates that the current test has failed, with the given message. *)
val fail : string -> unit

(** Indicates that the current test should be skipped, with the given message. *)
val skip : string -> unit

(** Prints a debugging message, followed by a newline character. *)
val print_endline : string -> unit

(** Prints a debugging message. *)
val print_string : string -> unit

(** Makes a test case. *)
val make_test_case : name -> description -> case -> test

(** Makes a function test case with a default description. *)
val make_function_test_case : name -> case -> test

(** Makes a test suite. *)
val make_test_suite : name -> description -> suite -> test

(** Makes a module test suite with a default description. *)
val make_module_test_suite : name -> suite -> test

(** Makes the given test accessible from the command-line. *)
val make_command_line_interface : test -> unit
