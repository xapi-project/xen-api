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
open Test_common

(* If we delete a record after making a Db.get_all_records call, but before the
 * call returns, then Db.get_all_records should return successfully (not throw
 * an Db_exn.DBCache_NotFound("missing row",...) exception, and the return
 * value should include the deleted record. *)
let test_db_get_all_records_race () =
	let __context = make_test_database () in
	let  (vm_ref: API.ref_VM) = make_vm ~__context () in

	Db_cache_impl.fist_delay_read_records_where := true;

	(* Kick off the thread which will destroy a VM. *)
	let destroyer_thread =
		Thread.create (fun self -> Db.VM.destroy ~__context ~self) vm_ref
	in

	(* Call get_all_records *)
	let _ =
		try Db.VM.get_all_records ~__context
		with Db_exn.DBCache_NotFound("missing row", _, _) ->
			assert_failure "Race condition present"
	in
	Thread.join destroyer_thread

let tear_down () =
	Db_cache_impl.fist_delay_read_records_where := false

let test =
	"test_db_lowlevel" >:::
		[
			"test_db_get_all_records_race" >:: (bracket id test_db_get_all_records_race tear_down);
		]
