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

let test_basic =
	let open Test_basic in
	"test_basic" >:::
		[
			"test_always_pass" >:: test_always_pass ;
			"test_always_fail" >:: test_always_fail ;
			"test_mock_db" >:: test_mock_db ;
			"test_assert_licensed_storage_motion" >:: test_assert_licensed_storage_motion ;
		]

let base_suite =
	"base_suite" >:::
		[
			test_basic
		]

let _ = run_test_tt_main base_suite
