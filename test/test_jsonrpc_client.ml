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
open Test_highlevel
open Xapi_stdext_monadic.Either

let dir = Filename.concat "test" "jsonrpc_files"

let jsonrpc_printer : Rpc.t Test_printers.printer =
	Jsonrpc.to_string

module Input_json_object = Generic.Make (struct
	module Io = struct
		type input_t = string
		type output_t = (exn, Rpc.t) Xapi_stdext_monadic.Either.t
		let string_of_input_t = Test_printers.string
		let string_of_output_t = Test_printers.(either exn jsonrpc_printer)
	end

	let good_call =
		let fin = open_in (Filename.concat dir "good_call.json") in
		let s = input_line fin in
		close_in fin;
		Jsonrpc.of_string s

	exception Parse_error

	let transform filename =
		let fin = open_in (Filename.concat dir filename) in
		let response =
			try
				let json = Jsonrpc_client.timeout_read (Unix.descr_of_in_channel fin) 5_000_000_000L in
				let rpc = Jsonrpc.of_string ~strict:false json in
				Right rpc
			with
			| End_of_file -> Left End_of_file
			| _ -> Left Parse_error
		in
		close_in fin;
		response

	let tests = [
		(* A file containing exactly one JSON object. *)
		(* It has got curly braces inside strings to make it interesting. *)
		"good_call.json", Right good_call;

		(* A file containing a partial JSON object. *)
		"short_call.json", Left Parse_error;

		(* A file containing a JSON object, plus some more characters at the end. *)
		"good_call_plus.json", Right good_call;

		(* A file containing some invalid JSON object. *)
		"bad_call.json", (Left Parse_error);
	]
end)

let suite =
	"jsonrpc_client" >:::
		[
			"input_json_object" >::: Input_json_object.tests;
		]
