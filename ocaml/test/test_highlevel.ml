(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

(* This framework should be useful if you want to apply lots of different
 * inputs to a function or system, and check that the output is correct in
 * each case.
 *
 * The basic idea is to either:
 *
 * 1. Create a module of type STATELESS_TEST or
 * 2. Create a module of type STATEFUL_TEST and turn it into a STATELESS_TEST
 *    with the EncapsulateState functor
 *
 * and then pass the STATELESS_TEST into the Make functor.
 *
 * This gives you a module with the functions test_equal and
 * test_equal_multiple, which take an input-output pair or a list of
 * input-output pairs and check that the output of the system is as expected
 * for each input.
 *)

open OUnit

module Generic = struct
	module type IO = sig
		(* The type of inputs to a system being tested. *)
		type input_t
		(* The type of outputs from a system being tested. *)
		type output_t

		(* Helper functions for printing error messages on test failure. *)
		val string_of_input_t : input_t -> string
		val string_of_output_t : output_t -> string
	end

	module type STATE = sig
		(* The type of system state, which will be modified by inputs to the system. *)
		type state_t
		(* Create a base system state. *)
		val create_default_state : unit -> state_t
	end

	module type STATELESS_TEST = sig
		module Io : IO
		(* A function to transform an input into an output. *)
		val transform : Io.input_t -> Io.output_t
	end

	module type STATEFUL_TEST = sig
		module Io : IO
		module State : STATE
		(* A function to apply an input to the system state. *)
		val load_input : State.state_t -> Io.input_t -> unit
		(* A function to extract an output from the system state. *)
		val extract_output : State.state_t -> Io.output_t
	end

	(* Turn a stateful test module into a stateless test module. *)
	module EncapsulateState(T: STATEFUL_TEST) = struct
		module Io = T.Io

		let transform input =
			let state = T.State.create_default_state () in
			T.load_input state input;
			T.extract_output state
	end

	(* Create functions which will actually run a test or tests. *)
	module Make(T: STATELESS_TEST) = struct
		let test_equal ~input ~expected_output =
			let actual_output = T.transform input in
			assert_equal
				~msg:(Printf.sprintf
					"Failure: input = %s, output = %s, expected output = %s"
					(T.Io.string_of_input_t input)
					(T.Io.string_of_output_t actual_output)
					(T.Io.string_of_output_t expected_output))
				actual_output expected_output

		let test_equal_multiple ~input_output_pairs =
			List.iter
				(fun (input, expected_output) -> test_equal ~input ~expected_output)
				input_output_pairs
	end
end

module XapiDb : Generic.STATE with type state_t = Context.t = struct
	type state_t = Context.t
	let create_default_state () = Mock.make_context_with_new_db "test context"
end
