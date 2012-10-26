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

let ( |> ) a b = b a
let id x = x

let test_config_file () =
	let open Config in
	let () = parse_line "" [] in
	let () = parse_line "# Foo" [] in
	let () = parse_line "\n" [] in
	let () = parse_line "whatever" [] in
	let () = parse_line "foo=true" [ "", Arg.Bool (fun x -> assert_equal ~printer:string_of_bool ~msg:"foo=true" true x), "" ] in
	(* true *)
	(* string *)
	(* int *)
	(* commented out string *)
	()

let _ =
	let verbose = ref false in
	Arg.parse [
		"-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
		"Test xenopsd common library code";

	let suite = "xenopsd" >:::
		[
			"config" >:: test_config_file;
		] in
	run_test_tt ~verbose:!verbose suite
