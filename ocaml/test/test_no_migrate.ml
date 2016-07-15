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


type mobility = Mobile | Static

let platforms =
	[ Static, ["nested-virt","true"]
	; Mobile, ["nested-virt","false"]
	; Static, ["nomigrate","true"]
	; Mobile, ["nomigrate","false"]
	; Static, ["nomigrate","true"; "nested-virt", "true"]
	; Static, ["nomigrate","true"; "nested-virt", "false"]
	; Mobile, ["nomigrate","false"; "nested-virt", "false"]
	; Static, ["nomigrate","false"; "nested-virt", "true"]
	; Static, ["nomigrate","TRUE"]
	; Static, ["nomigrate","1"]
	; Static, ["nested-virt","1"]
	; Mobile, ["nested-virt","0"]
	]

let simple () =
	let __context = make_test_database () in
	let vm        = make_vm ~__context ~hVM_boot_policy:"" () in
		( Db.VM.set_power_state ~__context ~self:vm ~value:`Running
		; LC.get_operation_error ~__context ~self:vm ~op:`suspend ~strict:true
		|> function
		| None          -> assert_bool "success" true
		| Some (x,xs)   -> assert_failure (String.concat "|" (x::xs))
		)

let base () =
	let __context = make_test_database () in
	let vm        = make_vm ~__context ~hVM_boot_policy:"" () in
		( Db.VM.set_power_state ~__context ~self:vm ~value:`Running
		; assert_equal ~msg:"suspend" None
			 (LC.get_operation_error ~__context ~self:vm ~op:`suspend ~strict:true)
		; assert_equal ~msg:"checkpoint" None
			 (LC.get_operation_error ~__context ~self:vm ~op:`checkpoint ~strict:true)
		; assert_equal ~msg:"pool_migrate" None
			 (LC.get_operation_error ~__context ~self:vm ~op:`pool_migrate ~strict:true)
		; assert_equal ~msg:"migrate_send" None
			 (LC.get_operation_error ~__context ~self:vm ~op:`migrate_send ~strict:true)
		)

(** [with_platform] checks that mobility is signaled according to the
 * values found in the platform flags of the last_booted_record *)
let with_platform () =
	let __context = make_test_database () in
	let self      = make_vm ~__context ~hVM_boot_policy:"" () in
	let test platform predicate =
		( Db.VM.set_power_state ~__context ~self ~value:`Running
		; Db.VM.set_platform ~__context ~self ~value:platform
		; Helpers.set_boot_record ~__context ~self
			(Db.VM.get_record ~__context ~self)
		; critical (* check all critical operations *)
		|> List.map (fun op -> LC.get_operation_error ~__context ~self ~op ~strict:true)
		|> List.for_all predicate
		|> assert_bool "with_platform"
		) in
	let predicate = function
	| Mobile  -> (=)  None    (* mobile, no error expected *)
	| Static  -> (<>) None in (* not mobile, expect an error message *)
	List.iter (fun (mobile, platform) -> test platform (predicate mobile)) platforms

(** [override] checks that migration is always possible when --force is
 * used *)
let override () =
	let __context = make_test_database () in
	let self      = make_vm ~__context ~hVM_boot_policy:"" () in
	let test platform =
		( Db.VM.set_power_state ~__context ~self ~value:`Running
		; Db.VM.set_platform ~__context ~self ~value:platform
		; Helpers.set_boot_record ~__context ~self
			(Db.VM.get_record ~__context ~self)
		; critical (* check all critical operations *)
		|> List.map (fun op -> LC.get_operation_error ~__context ~self ~op ~strict:false)
		|> List.for_all ((=) None) (* should not see any error *)
		|> assert_bool "override"
		) in
	List.iter (fun (_, platform) -> test platform) platforms

let test = "test_no_migrate" >:::
	[ "test_no_migrate_00" >:: simple
	; "test_no_migrate_01" >:: base
	; "test_no_migrate_02" >:: with_platform
	; "test_no_migrate_03" >:: override
	]

