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

let test_db_backup =
	"test_db_backup" >:::
		[
			"test_prepare_restore" >:: Pool_db_backup_test.test_prepare_restore;
		]

let test_db_upgrade =
	"test_db_upgrade" >:::
		[
			"upgrade_vm_memory_for_dmc" >:: Xapi_db_upgrade_test.upgrade_vm_memory_for_dmc;
			"upgrade_bios" >:: Xapi_db_upgrade_test.upgrade_bios;
			"update_snapshots" >:: Xapi_db_upgrade_test.update_snapshots;
		]

let base_suite =
	"base_suite" >:::
		[
			test_basic;
			test_db_backup;
			test_db_upgrade;
		]

let _ = run_test_tt_main base_suite
