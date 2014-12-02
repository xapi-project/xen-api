(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
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
open Test_common

let (|>) a f = f a

let all_pairs (l : 'a list) (l' : 'b list) : ('a * 'b) list =
	List.rev_map (fun x -> List.rev_map (fun y -> (x, y)) l' ) l |> List.flatten

(* Helpers for testing Xapi_sr_operations *)

let cmp a b = match a, b with
	| Some aa, Some bb -> aa = bb
	| None, None -> true
	| _ -> false

let printer = function Some a -> a | None -> "None (No exception expected)"
let msg ~cur_ops ~op ~pbd =
	let open Record_util in
	Printf.sprintf "Attempting operation %s, on an SR %s PBD, while operations [ %s ] in progress did not behave as expected:"
		(sr_operation_to_string op)
		(match pbd with Some true -> "with an attached" | Some false -> "with a detached" | None -> "without a")
		(cur_ops |> List.map sr_operation_to_string |> String.concat "; ")

let run_assert_exn ~__context ~cur_ops ~op ~pbd expected_exn =
	let sr_ref = make_sr ~__context ~current_operations:(List.map (fun op -> ("", op)) cur_ops) () in
	let _ = match pbd with
		| Some currently_attached -> ignore (make_pbd ~__context ~sR:sr_ref ~currently_attached ())
		| None -> ()
	in
	let r = try
		Xapi_sr_operations.assert_operation_valid ~__context ~self:sr_ref ~op;
		None
	with Api_errors.Server_error (e, _) -> Some e
	in
	assert_equal ~cmp ~msg:(msg ~cur_ops ~op ~pbd) ~printer expected_exn r

let test_permissible () =
	let __context = Mock.make_context_with_new_db "Mock context" in
	let open Api_errors in
	[
		([], `destroy, Some false, None); (* Can destroy SR with unplugged PBD *)
		([], `forget, Some false, None); (* Can forget SR with unplugged PBD *)
		([], `forget, None, None); (* Can forget SR with no PBD *)
		([`scan], `pbd_create, Some true, None); (* Currently the only parallelisable operation *)
		([`scan], `pbd_destroy, Some true, Some other_operation_in_progress);
	]
	|> List.iter (fun (cur_ops, op, pbd, expected_exn) ->
		run_assert_exn ~__context ~cur_ops ~op ~pbd expected_exn)

let test_not_permitted () =
	let __context = Mock.make_context_with_new_db "Mock context" in
	let open Api_errors in
	[
		([], `destroy, Some true, Some sr_has_pbd); (* Cannot destroy SR if PBD plugged *)
		([], `destroy, None, Some sr_no_pbds); (* Cannot destroy SR with no PBD *)
		([], `forget, Some true, Some sr_has_pbd); (* Cannot forget SR if PBD plugged *)
		(* TODO: Cannot destroy SR with managed VDI *)
	]
	|> List.iter (fun (cur_ops, op, pbd, expected_exn) ->
		run_assert_exn ~__context ~cur_ops ~op ~pbd expected_exn)

let test =
	"test_sr_operations" >:::
		[
			"test_permissible" >:: test_permissible;
			"test_not_permitted" >:: test_not_permitted;
		]
