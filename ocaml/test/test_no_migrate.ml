(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

module LC = Xapi_vm_lifecycle

let critical =
	[ `suspend
	; `checkpoint
	; `pool_migrate
	; `migrate_send
	]

let base_case () =
	let __context = make_test_database () in
	let vm        = make_vm ~__context () in
		( Db.VM.set_power_state ~__context ~self:vm ~value:`Running
		; assert_equal ~msg:"suspend" None
			 (LC.get_operation_error ~__context ~self:vm ~op:`suspend ~strict:false)
		; assert_equal ~msg:"checkpoint" None
			 (LC.get_operation_error ~__context ~self:vm ~op:`checkpoint ~strict:false)
		; assert_equal ~msg:"pool_migrate" None
			 (LC.get_operation_error ~__context ~self:vm ~op:`pool_migrate ~strict:false)
		; assert_equal ~msg:"migrate_send" None
			 (LC.get_operation_error ~__context ~self:vm ~op:`migrate_send ~strict:false)
		)

let with_platform platform check () =
	let __context = make_test_database () in
	let self      = make_vm ~__context () in
	let ()        =
		( Db.VM.set_power_state ~__context ~self ~value:`Running
		; Db.VM.set_platform ~__context ~self ~value:platform
		; Helpers.set_boot_record ~__context ~self
			(Db.VM.get_record ~__context ~self)
		) in
	critical (* check all critical operations *)
	|> List.map (fun op -> LC.get_operation_error ~__context ~self ~op ~strict:false)
	|> List.for_all check
	|> assert_bool "with_platform"

let test =
	let migrate = (=)  None  in (* no errors *)
	let dont    = (<>) None  in (* errors - can't migrate *)
	"test_no_migrate" >:::
	[ "test_no_migrate_00" >:: base_case
	; "test_no_migrate_01" >:: with_platform []                         migrate
	; "test_no_migrate_02" >:: with_platform ["nested-virt","true"]     dont
	; "test_no_migrate_03" >:: with_platform ["nested-virt","false"]    migrate
	; "test_no_migrate_04" >:: with_platform ["nomigrate","true"]       dont
	; "test_no_migrate_05" >:: with_platform ["nomigrate","false"]      migrate
	; "test_no_migrate_06"
		>:: with_platform ["nomigrate","true"; "nested-virt", "true"]     dont
	; "test_no_migrate_07"
		>:: with_platform ["nomigrate","true"; "nested-virt", "false"]    dont
	; "test_no_migrate_08"
		>:: with_platform ["nomigrate","false"; "nested-virt", "false"]   migrate
	; "test_no_migrate_09"
		>:: with_platform ["nomigrate","false"; "nested-virt", "true"]    dont
	; "test_no_migrate_10" >:: with_platform ["nomigrate","TRUE"]       dont
	; "test_no_migrate_12" >:: with_platform ["nomigrate","1"]          dont
	; "test_no_migrate_12" >:: with_platform ["nested-virt","1"]        dont
	; "test_no_migrate_13" >:: with_platform ["nested-virt","2"]        migrate
	; "test_no_migrate_14" >:: with_platform ["nested-virt","0"]        migrate
	]
