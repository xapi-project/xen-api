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
		requirement.key (string_of_string_opt requirement.default_value)

let string_of_requirements requirements =
	List.map string_of_requirement requirements
	|> string_of_string_list

let true_fun = (fun _ -> true)

module AddDefaults = Generic.Make(struct
	module Io = struct
		type input_t = (requirement list) * ((string * string) list)
		type output_t = (string * string) list

		let string_of_input_t (requirements, old_map) =
			Printf.sprintf "%s, %s"
				(string_of_requirements requirements)
				(string_of_string_map old_map)
		let string_of_output_t = string_of_string_map
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

let test =
	"test_map_check" >:::
		[
			"test_add_defaults" >:: AddDefaults.test;
		]
