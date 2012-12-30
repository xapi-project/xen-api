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

open Xenops_interface
open Xenops_utils
open Cancel_utils
open Xenstore
open Xenops_task

exception Did_not_cancel

let tasks = Xenops_task.empty ()

let xenstore_test xs _ =
	let task = Xenops_task.add tasks "test" (fun _ -> None) in
	let (_: Thread.t) = Thread.create (fun () -> Thread.delay 1.; Xenops_task.cancel tasks task.Xenops_task.id) () in
	try
		let (_: bool) = cancellable_watch (TestPath "/test/cancel") [] [] task ~xs ~timeout:3. () in
		raise Did_not_cancel
	with
		| Cancelled(_) ->
			(* success *)
			()

let subprocess_test _ =
	let task = Xenops_task.add tasks "test" (fun _ -> None) in
	let (_: Thread.t) = Thread.create (fun () -> Thread.delay 1.; Xenops_task.cancel tasks task.Xenops_task.id) () in
	try
		let (_, _) = cancellable_subprocess task "/bin/sleep" [ "3s" ] in
		raise Did_not_cancel
	with
		| Cancelled(_) ->
			(* success *)
			()


let _ =
	let verbose = ref false in

	Arg.parse [
		"-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Test cancellation functions";

	(* We need xenstore anyway, so use this to verify that we're running in a domain 0 environment *)
	let xs = try
		Xs.daemon_open ()
	with _ ->
		Printf.fprintf stderr "Failed to open xenstore connection. This test suite must run in domain 0.\n";
		exit 1 in

	let suite = "cancel test" >::: 
		[
			"subprocess" >:: subprocess_test;
			"xenstore" >:: xenstore_test xs;
		] in

	run_test_tt ~verbose:!verbose suite
