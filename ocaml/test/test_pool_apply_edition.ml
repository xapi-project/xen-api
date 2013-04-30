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

open OUnit

let apply_edition_succeed ~__context ~host ~edition =
	Db.Host.set_edition ~__context ~self:host ~value:edition

let apply_edition_fail_host_offline ~__context ~host ~edition =
	raise (Api_errors.Server_error
		(Api_errors.host_offline, [Ref.string_of host]))

let setup ~__context ~host_count ~edition =
	let hosts = ref [] in
	for n = 1 to host_count do
		hosts := (Test_common.make_host ~__context ~edition ()) :: !hosts
	done;
	let (_: API.ref_pool) =
		Test_common.make_pool ~__context ~master:(List.hd !hosts) () in ()

(* Test that apply_edition_with_rollback calls apply_fn for each host,
 * assuming no exceptions are thrown. *)
let test_basic_operation () =
	let __context = Mock.make_context_with_new_db "test context" in
	setup ~__context ~host_count:8 ~edition:"free";
	let hosts = Db.Host.get_all ~__context in
	Xapi_pool_license.apply_edition_with_rollback
		~__context ~hosts ~edition:"per-socket"
		~apply_fn:apply_edition_succeed;
	List.iter
		(fun host ->
			let new_edition = Db.Host.get_edition ~__context ~self:host in
			assert_equal
				~msg:(Printf.sprintf
					"Testing that host %s has had the new license applied"
					(Ref.string_of host))
				"per-socket"
				new_edition)
		hosts

(* Check that if a host is offline, apply_edition_with_rollback rolls all hosts
 * back to the edition they had to start off with. *)
let test_rollback_logic () =
	let __context = Mock.make_context_with_new_db "test context" in
	setup ~__context ~host_count:8 ~edition:"free";
	let hosts = Db.Host.get_all ~__context in
	(* Fourth host will fail to apply_edition with HOST_OFFLINE. *)
	let offline_host = List.nth hosts 4 in
	let apply_fn ~__context ~host ~edition =
		if host = offline_host
		then apply_edition_fail_host_offline ~__context ~host ~edition
		else apply_edition_succeed ~__context ~host ~edition
	in
	assert_raises ~msg:"Testing that HOST_OFFLINE is successfully propagated"
		(Api_errors.Server_error
			(Api_errors.host_offline, [Ref.string_of offline_host]))
		(fun () ->
			Xapi_pool_license.apply_edition_with_rollback
				~__context ~hosts ~edition:"per-socket" ~apply_fn);
	List.iter
		(fun host ->
			let new_edition = Db.Host.get_edition ~__context ~self:host in
			assert_equal
				~msg:(Printf.sprintf
					"Testing that host %s has been rolled back to free edition"
					(Ref.string_of host))
				"free"
				new_edition)
		hosts

let test =
	"pool_apply_edition" >:::
		[
			"test_basic_operation" >:: test_basic_operation;
			"test_rollback_logic" >:: test_rollback_logic;
		]
