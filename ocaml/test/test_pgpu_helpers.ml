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

open Either
open OUnit
open Test_common
open Test_highlevel
open Test_vgpu_common
open Xapi_vgpu_type

module GetRemainingCapacity = Generic.Make(Generic.EncapsulateState(struct
	module Io = struct
		type input_t = (pgpu_state * vgpu_type)
		type output_t = (exn, int64) Either.t

		let string_of_input_t (pgpu, vgpu_type) =
			Printf.sprintf
				"(pgpu: %s, test vgpu_type: %s)"
				(string_of_pgpu_state pgpu)
				(string_of_vgpu_type vgpu_type)

		let string_of_output_t = function
			| Left e -> Printexc.to_string e
			| Right n -> Int64.to_string n
	end

	module State = XapiDb

	let load_input __context (pgpu, _) =
		let (_: API.ref_PGPU) = make_pgpu ~__context pgpu in ()

	let extract_output __context (_, vgpu_type) =
		let pgpu_ref = List.hd (Db.PGPU.get_all ~__context) in
		let vgpu_type_ref = find_or_create ~__context vgpu_type in
		Xapi_pgpu_helpers.get_remaining_capacity_internal
			~__context ~self:pgpu_ref ~vgpu_type:vgpu_type_ref

	let tests = [
		(default_k1, k100), Right 8L;
		(default_k1, k140q), Right 4L;
	]
end))

let test =
	"pgpu_checks" >:::
		[
			"test_get_remaining_capacity" >:: GetRemainingCapacity.test;
		]
