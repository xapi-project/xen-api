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
open Cpuid_helpers


module StringOfFeatures = Generic.Make (struct
	module Io = struct
		type input_t = int64 array
		type output_t = string
		let string_of_input_t = Test_printers.(array int64)
		let string_of_output_t = Test_printers.string
	end

	let transform = Cpuid_helpers.string_of_features

	let tests = [
		[|0L; 2L; 123L|], "00000000-00000002-0000007b";
		[|0L|], "00000000";
		[||], "";
	]
end)

module FeaturesOfString = Generic.Make (struct
	module Io = struct
		type input_t = string
		type output_t = int64 array
		let string_of_input_t = Test_printers.string
		let string_of_output_t = Test_printers.(array int64)
	end

	let transform = Cpuid_helpers.features_of_string

	let tests = [
		"00000000-00000002-0000007b", [|0L; 2L; 123L|];
		"00000000", [|0L|];
		"", [||];
	]
end)

module RoundTripFeaturesToFeatures = Generic.Make (struct
	module Io = struct
		type input_t = int64 array
		type output_t = int64 array
		let string_of_input_t = Test_printers.(array int64)
		let string_of_output_t = Test_printers.(array int64)
	end

	let transform = fun x -> x |> Cpuid_helpers.string_of_features |> Cpuid_helpers.features_of_string

	let tests = List.map (fun x -> x, x) [
		[|0L; 1L; 123L|];
		[|1L|];
		[|0L|];
		[||];
	]
end)

module RoundTripStringToString = Generic.Make (struct
	module Io = struct
		type input_t = string
		type output_t = string
		let string_of_input_t = Test_printers.string
		let string_of_output_t = Test_printers.string
	end

	let transform = fun x -> x |> Cpuid_helpers.features_of_string |> Cpuid_helpers.string_of_features

	let tests = List.map (fun x -> x, x) [
		"00000000-00000002-0000007b";
		"00000001";
		"00000000";
		"";
	]
end)

module ParseFailure = Generic.Make (struct
	module Io = struct
		type input_t = string
		type output_t = exn
		let string_of_input_t = Test_printers.string
		let string_of_output_t = Test_printers.exn
	end

	exception NoExceptionRaised
	let transform = fun x ->
		try 
			ignore (Cpuid_helpers.features_of_string x);
			raise NoExceptionRaised
		with e -> e

	let tests = List.map (fun x -> x, InvalidFeatureString x) [
		"foo bar baz";
		"fgfg-1234";
		"0123-foo";
		"foo-0123";
		"-1234";
		"1234-";
	]
end)


module Extend = Generic.Make (struct
	module Io = struct
		type input_t = int64 array * int64 array
		type output_t = int64 array
		let string_of_input_t = Test_printers.(pair (array int64) (array int64))
		let string_of_output_t = Test_printers.(array int64)
	end

	let transform = fun (arr0, arr1) -> Cpuid_helpers.extend arr0 arr1

	let tests = [
		([| |], [| |]), [| |];
		([| |], [| 0L; 2L |]), [| 0L; 2L |];
		([| 1L |], [| |]), [| |];
		([| 1L |], [| 0L |]), [| 1L |];
		([| 1L |], [| 0L; 2L |]), [| 1L; 2L |];
		([| 1L; 0L |], [| 0L; 2L |]), [| 1L; 0L |];
		([| 1L; 0L |], [| 0L; 2L; 4L; 9L |]), [| 1L; 0L; 4L; 9L |];
	]
end)


module ZeroExtend = Generic.Make (struct
	module Io = struct
		type input_t = int64 array * int
		type output_t = int64 array
		let string_of_input_t = Test_printers.(pair (array int64) int)
		let string_of_output_t = Test_printers.(array int64)
	end

	let transform = fun (arr, len) -> Cpuid_helpers.zero_extend arr len

	let tests = [
		([| 1L |], 2), [| 1L; 0L |];
		([| 1L |], 1), [| 1L; |];
		([| |], 2), [| 0L; 0L |];
		([| |], 1), [| 0L |];
		([| |], 0), [| |];
		([| 1L; 2L |], 0), [| |];
		([| 1L; 2L |], 1), [| 1L |];
		([| 1L; 2L |], 2), [| 1L; 2L |];
	]
end)


module Intersect = Generic.Make (struct
	module Io = struct
		type input_t = int64 array * int64 array
		type output_t = int64 array
		let string_of_input_t = Test_printers.(pair (array int64) (array int64))
		let string_of_output_t = Test_printers.(array int64)
	end

	let transform = fun (a, b) -> Cpuid_helpers.intersect a b

	let tests = [
		(* Intersect should follow monoid laws - identity and commutativity *)
		([| |], [| |]),            [| |];
		([| 1L; 2L; 3L |], [| |]), [| 1L; 2L; 3L |];
		([| |], [| 1L; 2L; 3L |]), [| 1L; 2L; 3L |];

		([| 7L; 3L |], [| 5L; |]), [| 5L; 0L |];
		([| 5L; |], [| 7L; 3L |]), [| 5L; 0L |];

		([| 1L |],         [| 1L |]),      [| 1L |];
		([| 1L |],         [| 1L; 0L |]),  [| 1L; 0L |];

		([| 1L; 2L; 3L |], [| 1L; 1L; 1L |]), [| 1L; 0L; 1L |];
		([| 1L; 2L; 3L |], [| 0L; 0L; 0L |]), [| 0L; 0L; 0L |];
	]
end)


module Upgrade = Generic.Make (struct
	module Io = struct
		type input_t = string * string
		type output_t = string
		let string_of_input_t = Test_printers.(pair string string)
		let string_of_output_t = Test_printers.string
	end

	let transform = fun (vm, host) ->
		let host' = Cpuid_helpers.features_of_string host in
		let vm' = Cpuid_helpers.features_of_string vm in
		Cpuid_helpers.upgrade_features host' vm' |> Cpuid_helpers.string_of_features

	let tests = [
		("", "0000000a-0000000b-0000000c-0000000d-0000000e"),
			"0000000a-0000000b-0000000c-0000000d-0000000e";
		("00000001-00000002-00000003-00000004", "0000000a-0000000b-0000000c-0000000d-0000000e"),
			"00000001-00000002-00000003-00000004-0000000e";
		("00000001-00000002-00000003-00000004", "0000000a-0000000b-0000000c-0000000d-0000000e-0000000f"),
			"00000001-00000002-00000003-00000004-0000000e-0000000f";
		("00000001-00000002-00000003-00000004-00000005", "0000000a-0000000b-0000000c-0000000d-0000000e"),
			"00000001-00000002-00000003-00000004-00000005";
		("00000001-00000002-00000003-00000004-00000005", "0000000a-0000000b-0000000c-0000000d-0000000e-0000000f"),
			"00000001-00000002-00000003-00000004-00000005-00000000";
		("00000001-00000002-00000003-00000004-00000005", "0000000a-0000000b-0000000c-0000000d-0000000e-0000000f-000000aa"),
			"00000001-00000002-00000003-00000004-00000005-00000000-00000000";
	]
end)


module Accessors = Generic.Make (struct
	module Io = struct
		type input_t = (string * string) list
		type output_t = string * int * int * int64 array * int64 array
		let string_of_input_t = Test_printers.(assoc_list string string)
		let string_of_output_t = Test_printers.(tuple5 string int int (array int64) (array int64))
	end

	let transform = fun record -> 
		let open Map_check in
			getf vendor record, 
			getf socket_count record, 
			getf cpu_count record,
			getf features_pv record, 
			getf features_hvm record

	let tests = [
		["vendor", "Intel"; "socket_count", "1"; "cpu_count", "1";
		 "features_pv", "00000001-00000002-00000003";
		 "features_hvm", "0000000a-0000000b-0000000c"], 
		 ("Intel", 1, 1, [| 1L; 2L; 3L |], [| 0xaL; 0xbL; 0xcL |]);
		["vendor", "Amd"; "socket_count", "6"; "cpu_count", "24";
		 "features_pv", "00000001";
		 "features_hvm", ""], 
		 ("Amd", 6, 24, [| 1L |], [| |]);
	]
end)

module Setters = Generic.Make (struct
	module Io = struct
		type input_t = string * int * int * int64 array * int64 array
		type output_t = (string * string) list
		let string_of_input_t = Test_printers.(tuple5 string int int (array int64) (array int64))
		let string_of_output_t = Test_printers.(assoc_list string string)
	end

	let transform = fun (name, sockets, cpus, pv, hvm)  -> 
		let open Map_check in
			[]
			|> setf vendor name
			|> setf socket_count sockets
			|> setf cpu_count cpus
			|> setf features_pv pv
			|> setf features_hvm hvm
			|> List.sort compare

	let tests = [
		("Intel", 1, 1, [| 1L; 2L; 3L |], [| 0xaL; 0xbL; 0xcL |]),
		List.sort compare ["vendor", "Intel"; 
                 "socket_count", "1"; "cpu_count", "1";
		 "features_pv", "00000001-00000002-00000003";
		 "features_hvm", "0000000a-0000000b-0000000c"];

		("Amd", 6, 24, [| 1L |], [| |]),
		List.sort compare ["vendor", "Amd"; 
                 "socket_count", "6"; "cpu_count", "24";
		 "features_pv", "00000001";
		 "features_hvm", ""]
	]
end)


module Modifiers = Generic.Make (struct
	module Io = struct
		type input_t = (string * string) list
		type output_t = (string * string) list
		let string_of_input_t = Test_printers.(assoc_list string string)
		let string_of_output_t = Test_printers.(assoc_list string string)
	end

	let transform = fun record -> 
		let open Map_check in
			record 
			|> setf vendor (getf vendor record)
			|> setf socket_count (getf socket_count record)
			|> setf cpu_count (getf cpu_count record)
			|> setf features_pv (getf features_pv record)
			|> setf features_hvm (getf features_hvm record)
			|> List.sort compare

	let tests = [
		["cpu_count", "1";
		 "features_hvm", "0000000a-0000000b-0000000c";
		 "features_pv", "00000001-00000002-00000003";
		 "socket_count", "1"; 
		 "vendor", "Intel"],
		["cpu_count", "1";
		 "features_hvm", "0000000a-0000000b-0000000c";
		 "features_pv", "00000001-00000002-00000003";
		 "socket_count", "1"; 
		 "vendor", "Intel"];
	]
end)


let test =
	"test_cpuid_helpers" >:::
		[
			"test_string_of_features" >::: StringOfFeatures.tests;
			"test_features_of_string" >::: FeaturesOfString.tests;
			"test_roundtrip_features_to_features" >:::
				RoundTripFeaturesToFeatures.tests;
			"test_parse_failure" >:::
				ParseFailure.tests;
			"test_extend" >:::
				Extend.tests;
			"test_zero_extend" >::: 
				ZeroExtend.tests;
			"test_intersect" >:::
				Intersect.tests;
			"test_upgrade" >:::
				Upgrade.tests;
			"test_accessors" >:::
				Accessors.tests;
			"test_setters" >:::
				Setters.tests;
			"test_modifiers" >:::
				Modifiers.tests;
		]
