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
open Http

let usage_and_exit () =
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr "  %s\n" Sys.argv.(0);
	Printf.fprintf stderr "       -- run all unit tests\n";
    exit 1

let test_accept_simple _ =
	let t = Accept.t_of_string "application/json" in
	assert_equal ~msg:"ty" ~printer:(Opt.default "None") t.Accept.ty (Some "application");
	assert_equal ~msg:"subty" ~printer:(Opt.default "None") t.Accept.subty (Some "json");
	assert (Accept.matches ("application", "json") t)

let test_accept_complex _ =
	let ts = Accept.ts_of_string "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" in
	let all = List.filter (Accept.matches ("text", "html")) ts in
	let m = Accept.preferred_match ("text", "html") ts in
	assert((Opt.unbox m).Accept.ty = Some "text");
	let m = Accept.preferred_match ("foo", "bar") ts in
	assert((Opt.unbox m).Accept.ty = None)

let _ =
    let verbose = ref false in
    Arg.parse [
        "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
    ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
        "Test HTTP server";

	let suite = "HTTP test" >::: 
        [
            "accept_simple" >:: test_accept_simple;
            "accept_complex" >:: test_accept_complex;
		] in
    run_test_tt ~verbose:!verbose suite
