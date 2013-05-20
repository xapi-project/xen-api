(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

(* Helpers for testing Xapi_vdi.check_operation_error *)

let setup_test ~__context vbd_fun =
	let pbd_ref, sr_ref, vdi_ref = Ref.make (), Ref.make (), Ref.make () in
	let () = make_sr ~__context ~ref:sr_ref () in
	let () = make_pbd ~__context ~ref:pbd_ref ~sR:sr_ref () in
	let () = make_vdi ~__context ~ref:vdi_ref ~sR:sr_ref () in
	let vdi_record = Db.VDI.get_record_internal ~__context ~self:vdi_ref in
	vbd_fun vdi_ref;
	vdi_ref, vdi_record

let my_cmp a b = match a,b with
	| Some aa, Some bb	-> fst aa = fst bb
	| None, None -> a = b
	| _	-> false

let run_assert_equal_with_vdi ~__context ?(cmp = my_cmp) ?(ha_enabled=false) vbd_list op exc =
	let vdi_ref, vdi_record = setup_test ~__context vbd_list in
	assert_equal ~cmp (Xapi_vdi.check_operation_error ~__context ha_enabled vdi_record vdi_ref op) exc

(* This is to test Xapi_vdi.check_operation_error against CA-98944
   code. This DO NOT fully test the aforementionned function *)
let test_ca98944 () =
	let __context = Mock.make_context_with_new_db "Mock context" in
	(* Should raise vdi_in_use *)
	run_assert_equal_with_vdi ~__context
		(fun vdi_ref ->
			make_vbd ~vDI:vdi_ref ~__context
				~reserved:true ~currently_attached:false ~current_operations:["", `attach] ())
		`update (Some (Api_errors.vdi_in_use, []));

	(* Should raise vdi_in_use *)
	run_assert_equal_with_vdi ~__context
		(fun vdi_ref ->
			make_vbd ~vDI:vdi_ref
				~__context ~reserved:false ~currently_attached:true ~current_operations:["", `attach] ())
		`update (Some (Api_errors.vdi_in_use, []));

	(* Should raise vdi_in_use *)
	run_assert_equal_with_vdi ~__context
		(fun vdi_ref -> make_vbd ~vDI:vdi_ref
			~__context ~reserved:true ~currently_attached:true ~current_operations:["", `attach] ())
		`update (Some (Api_errors.vdi_in_use, []));

	(* Should raise other_operation_in_progress *)
	run_assert_equal_with_vdi ~__context
		(fun vdi_ref -> make_vbd ~vDI:vdi_ref
			~__context ~reserved:false ~currently_attached:false ~current_operations:["", `attach] ())
		`update (Some (Api_errors.other_operation_in_progress, []));

	(* Should pass *)
	run_assert_equal_with_vdi ~__context
		(fun vdi_ref -> make_vbd ~vDI:vdi_ref
			~__context ~reserved:false ~currently_attached:false ~current_operations:[] ())
		`forget None

(* VDI.copy should be allowed if all attached VBDs are read-only. *)
let test_ca101699 () =
	let __context = Mock.make_context_with_new_db "Mock context" in

	(* Attempting to copy a RW-attached VDI should fail with VDI_IN_USE. *)
	run_assert_equal_with_vdi ~__context
		(fun vdi_ref ->
			make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RW ())
		`copy (Some (Api_errors.vdi_in_use, []));

	(* Attempting to copy a RO-attached VDI should pass. *)
	run_assert_equal_with_vdi ~__context
		(fun vdi_ref ->
			make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO ())
		`copy None;

	(* Attempting to copy an unattached VDI should pass. *)
	run_assert_equal_with_vdi ~__context
		(fun vdi_ref -> ())
		`copy None

let test =
	"test_vdi_allowed_operations" >:::
		[
			"test_ca98944" >:: test_ca98944;
			"test_ca101699" >:: test_ca101699;
		]
