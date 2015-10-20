(*
 * Copyright (C) 2006-2015 Citrix Systems Inc.
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

open Map_check
open OUnit
open Test_common
open Test_highlevel

let string_of_requirement requirement =
	Printf.sprintf "{key = \"%s\"; default_value = \"%s\"}"
		requirement.key (Test_printers.(option string) requirement.default_value)

let true_fun = (fun _ -> true)

let false_fun = (fun _ -> false)

module AddDefaults = Generic.Make(struct
	module Io = struct
		type input_t = (requirement list) * ((string * string) list)
		type output_t = (string * string) list

		let string_of_input_t =
			Test_printers.(assoc_pair
				(list string_of_requirement)
				(assoc_list string string))
		let string_of_output_t = Test_printers.(assoc_list string string)
	end

	let transform (requirements, old_map) = add_defaults requirements old_map

	let tests = [
		(* If default value is None, no value should be added. *)
		(
			[{key = "abc"; default_value = None; is_valid_value = true_fun}],
			[]
		),
		[];
		(* If default value is Some _, the default should be added. *)
		(
			[{key = "abc"; default_value = Some "def"; is_valid_value = true_fun}],
			[]
		),
		["abc", "def"];
		(* If default value is None, an existing value should not be overwritten. *)
		(
			[{key = "abc"; default_value = None; is_valid_value = true_fun}],
			["abc", "ghi"]
		),
		["abc", "ghi"];
		(* If default value is Some _, an existing value should not be overwritten. *)
		(
			[{key = "abc"; default_value = Some "def"; is_valid_value = true_fun}],
			["abc", "ghi"]
		),
		["abc", "ghi"];
	]
end)

module ValidateKVPair = Generic.Make(struct
	module Io = struct
		type input_t = requirement list * string * string
		type output_t = (exn, unit) Either.t

		let string_of_input_t (requirements, key, value) =
			Printf.sprintf "%s, %s, %s"
				((Test_printers.list string_of_requirement) requirements) key value
		let string_of_output_t = Test_printers.(either exn unit)
	end

	let transform (requirements, key, value) =
		try Either.Right (validate_kvpair "test_field" requirements (key, value))
		with e -> Either.Left e

	let tests = [
		(* If all values are valid, the exception should not be thrown. *)
		(
			[{key = "abc"; default_value = None; is_valid_value = true_fun}],
			"abc", "def"
		),
		Either.Right ();
		(* If there is no valid value, the exception should always be thrown. *)
		(
			[{key = "abc"; default_value = None; is_valid_value = false_fun}],
			"abc", "def"
		),
		Either.Left (Api_errors.(Server_error
			(invalid_value, ["test_field"; "abc = def"])));
	]
end)

let test =
	"test_map_check" >:::
		[
			"test_add_defaults" >::: AddDefaults.tests;
			"test_validate_kvpair" >::: ValidateKVPair.tests;
		]
