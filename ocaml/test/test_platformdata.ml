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

open Fun
open OUnit
open Test_highlevel

module SanityCheck = Generic.Make(struct
	module Io = struct
		type input_t = ((string * string) list * bool)
		type output_t = (string * string) list

		let string_of_input_t input =
			Printf.sprintf "(platformdata = %s, filter_out_unknowns = %b)"
				(fst input |> Test_common.string_of_string_map)
				(snd input)

		let string_of_output_t = Test_common.string_of_string_map
	end

	let transform (platformdata, filter_out_unknowns) =
		Xapi_xenops.Platform.sanity_check ~platformdata ~filter_out_unknowns

	let tests =
		let usb_defaults = [
			"usb", "true";
			"usb_tablet", "true";
		] in
		[
			(* Check that we can filter out unknown platform flags. *)
			(([
				"nonsense", "abc";
				"pae", "true";
				"whatever", "def";
				"viridian", "true";
			], true),
			usb_defaults @
			[
				"pae", "true";
				"viridian", "true";
			]);
			(* Check that usb and usb_tablet are turned on by default. *)
			(([], false),
			usb_defaults);
			(* Check that an invalid tsc_mode gets filtered out. *)
			((["tsc_mode", "17";], false),
			usb_defaults);
			(* Check that an invalid parallel port gets filtered out. *)
			((["parallel", "/dev/random"], false),
			usb_defaults);
			(* Check that we can't set usb_tablet to true if usb is false. *)
			(([
				"usb", "false";
				"usb_tablet", "true";
			], false),
			[
				"usb", "false";
				"usb_tablet", "false";
			]);
			(* Check that we can fully disable usb. *)
			(([
				"usb", "false";
				"usb_tablet", "false";
			], false),
			[
				"usb", "false";
				"usb_tablet", "false";
			]);
			(* Check that we can disable the parallel port. *)
			((["parallel", "none"], false),
			usb_defaults @
			["parallel", "none"]);
			(* Check that a set of valid fields is unchanged (apart from
			 * the ordering, which changes due to the implementation of
			 * List.update_assoc). *)
			(([
				"parallel", "/dev/parport2";
				"pae", "true";
				"usb_tablet", "false";
				"tsc_mode", "2";
				"viridian", "true";
				"usb", "true";
			], false),
			[
				"usb", "true";
				"usb_tablet", "false";
				"parallel", "/dev/parport2";
				"pae", "true";
				"tsc_mode", "2";
				"viridian", "true";
			]);
			(* Check that combination of valid and invalid fields is dealt with
			 * correctly. *)
			(([
				"pae", "true";
				"parallel", "/dev/parport0";
				"tsc_mode", "blah";
			], false),
			usb_defaults @
			[
				"pae", "true";
				"parallel", "/dev/parport0";
			]);
		]
end)

let test =
	"platformdata" >:::
		[
			"test_platform_sanity_check" >:: SanityCheck.test
		]
