(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

open Xapi_db_upgrade

(** Make a simple in-memory database containing a single host and dom0 VM record. *)
let make_test_database () = 
	let db = Db_upgrade.generic_database_upgrade (Db_cache_types.Database.make (Schema.of_datamodel ())) in
	let db_ref = Db_ref.in_memory (ref (ref db)) in
	let __context = Context.make ~database:db_ref "upgrade_vm_memory_for_dmc" in

	(* Db_xml.To.file "/tmp/new3.db" (Db_ref.get_database (Context.database_of __context)); *)

	let host_info = {
		Create_misc.name_label = "test host";
		xen_verstring = "unknown";
		linux_verstring = "something";
		hostname = "localhost";
		uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid;
		dom0_uuid = "dom0-uuid";
		oem_manufacturer = None;
		oem_model = None;
		oem_build_number = None;
		machine_serial_number = None;
		machine_serial_name = None;
		total_memory_mib = 1024L;
		dom0_static_max = Memory.bytes_of_mib 512L;
	} in
	Dbsync_slave.create_localhost ~__context host_info;
	Create_misc.ensure_domain_zero_records ~__context host_info;
	__context

let upgrade_vm_memory_for_dmc () = 
	let __context = make_test_database () in

	let self = List.hd (Db.VM.get_all ~__context) in

	(* Set control domain's dynamic_min <> dynamic_max <> target *)
	Db.VM.set_memory_dynamic_min ~__context ~self ~value:1L;
	Db.VM.set_memory_target ~__context ~self ~value:2L;
	Db.VM.set_memory_dynamic_max ~__context ~self ~value:3L;
	(* Apply the upgrade rule *)
	upgrade_vm_memory_for_dmc.fn ~__context; 
	let r = Db.VM.get_record ~__context ~self in
	if r.API.vM_memory_dynamic_min <> r.API.vM_memory_target
	then failwith "upgrade_vm_memory_for_dmc: control domain memory_dynamic_min <> memory_target";
	if r.API.vM_memory_dynamic_max <> r.API.vM_memory_target
	then failwith "upgrade_vm_memory_for_dmc: control domain memory_dynamic_max <> memory_target";

	(* Make this a non-control domain and change all memory fields *)
	Db.VM.set_is_control_domain ~__context ~self ~value:false;
	Db.VM.set_memory_static_min ~__context ~self ~value:5L;
	Db.VM.set_memory_dynamic_min ~__context ~self ~value:1L;
	Db.VM.set_memory_target ~__context ~self ~value:2L;
	Db.VM.set_memory_dynamic_max ~__context ~self ~value:3L;	
	Db.VM.set_memory_static_max ~__context ~self ~value:4L;
	(* Apply the upgrade rule *)
	upgrade_vm_memory_for_dmc.fn ~__context;
	let r = Db.VM.get_record ~__context ~self in
	if r.API.vM_memory_dynamic_max <> r.API.vM_memory_static_max
	then failwith "upgrade_vm_memory_for_dmc: memory_dynamic_max <> memory_static_max";
	if r.API.vM_memory_target <> r.API.vM_memory_static_max
	then failwith "upgrade_vm_memory_for_dmc: memory_target <> memory_static_max";
	if r.API.vM_memory_dynamic_min <> r.API.vM_memory_static_max
	then failwith "upgrade_vm_memory_for_dmc: memory_dynamic_min <> memory_static_max";
	if r.API.vM_memory_static_min > r.API.vM_memory_static_max
	then failwith "upgrade_vm_memory_for_dmc: memory_static_min > memory_static_max";
	Printf.printf "upgrade_vm_memory_for_dmc: OK\n"

let upgrade_bios () = 

	let check inventory bios_strings = 
		Unixext.mkdir_safe "/var/tmp" 0o755;
		Unixext.write_string_to_file "/var/tmp/.previousInventory" inventory;
		let __context = make_test_database () in
		upgrade_bios_strings.fn ~__context; 
		let _, vm_r = List.hd (Db.VM.get_all_records ~__context) in
		if vm_r.API.vM_bios_strings <> bios_strings
		then failwith "bios strings upgrade" in
	
	check "OEM_MANUFACTURER=Dell" Xapi_globs.old_dell_bios_strings;
	check "OEM_MANUFACTURER=HP" Xapi_globs.old_hp_bios_strings;
	check "" Xapi_globs.generic_bios_strings;
	Unixext.unlink_safe "/var/tmp/.previousInventory";
	Printf.printf "upgrade_bios: OK\n"

let _ = 
	upgrade_vm_memory_for_dmc ();
	upgrade_bios ()
	

