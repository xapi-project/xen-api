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

let test_always_pass () = assert_equal 1 1
let test_always_fail () = skip "This will fail" ; assert_equal 1 0

let test_mock_db () =
	let db = Mock.Database.make () in
	let __context = Mock.Context.make ~database:db "Mock context" in
	let blob_ref = Ref.make () in
	Db.Blob.create __context blob_ref
		(Uuid.to_string (Uuid.make_uuid ()))
		"BLOB" "" 5L true (Date.of_float 0.0) "" ;
	ignore (Db.Blob.get_record ~__context ~self:blob_ref) ;
	ignore (Db.VM.get_all_records ~__context) ;
	let blob_name = Db.Blob.get_name_label ~__context ~self:blob_ref in
	assert_equal blob_name "BLOB"

let test_assert_licensed_storage_motion () = skip "TODO" ;
	let db = Mock.Database.make () in
	let __context = Mock.Context.make ~database:db "Mock context" in
	let licensed = try Xapi_vm_migrate.assert_licensed_storage_motion ~__context; true
	with _ -> false in
	assert_bool "Not licensed for SXM" licensed
