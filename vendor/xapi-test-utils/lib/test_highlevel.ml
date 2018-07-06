(*
 * Copyright (C) Citrix Systems Inc.
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
		(* A list of input/output pairs. *)
		val tests : (Io.input_t * Io.output_t) list
	end

	module type STATEFUL_TEST = sig
		module Io : IO
		module State : STATE
		(* A function to apply an input to the system state. *)
		val load_input : State.state_t -> Io.input_t -> unit
		(* A function to extract an output from the system state. How this is done
		 * may depend on the input to the test. *)
		val extract_output : State.state_t -> Io.input_t -> Io.output_t
		(* A list of input/output pairs. *)
		val tests : (Io.input_t * Io.output_t) list
	end

	(* Turn a stateful test module into a stateless test module. *)
	module EncapsulateState(T: STATEFUL_TEST) = struct
		module Io = T.Io

		let transform input =
			let state = T.State.create_default_state () in
			T.load_input state input;
			T.extract_output state input

		let tests = T.tests
	end

	(* Create functions which will actually run a test or tests. *)
	module Make(T: STATELESS_TEST) = struct
		let test_equal ~input ~expected_output =
			let title = Printf.sprintf "%s -> %s" 
				(T.Io.string_of_input_t input)
				(T.Io.string_of_output_t expected_output) in
			title >:: (fun () -> 
				let actual_output = T.transform input in
				assert_equal
					~msg:(Printf.sprintf
						"Failure: input = %s, output = %s, expected output = %s"
						(T.Io.string_of_input_t input)
						(T.Io.string_of_output_t actual_output)
						(T.Io.string_of_output_t expected_output))
					actual_output expected_output)

		let tests =
			List.map
				(fun (input, expected_output) -> test_equal ~input ~expected_output)
				T.tests
	end
end
