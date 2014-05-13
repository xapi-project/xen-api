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

let base_suite =
	"base_suite" >:::
		[
			Test_basic.test;
			Test_helpers.test;
			Test_db_lowlevel.test;
			Test_pool_db_backup.test;
			Test_xapi_db_upgrade.test;
			Test_vdi_allowed_operations.test;
			Test_pool_license.test;
			Test_platformdata.test;
			Test_sm_features.test;
			Test_gpu_group.test;
			Test_pci_db.test;
			Test_pci_helpers.test;
			Test_vgpu_type.test;
			Test_pgpu.test;
			Test_pgpu_helpers.test;
			Test_vm_helpers.test;
			Test_xenopsd_metadata.test;
		]

let _ = run_test_tt_main base_suite
