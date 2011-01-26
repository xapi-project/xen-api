(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module D = Debug.Debugger(struct let name = "xapi" (* this is set to 'xapi' deliberately! :) *) end)
open D

open Db_cache_types
open Stringext
open Pervasiveext

(* ---------------------- upgrade db file from last release schema -> current schema.

   upgrade_from_last_release contains the generic mechanism for upgrade (i.e. filling in default values
   specified in IDL).

   There are also some non-generic db upgrade rules coded specifically in non_generic_db_upgrade_rules.

   For Orlando we have to make these rules idempontent and run them on _every_ master populate. This
   makes sure we'll run them from MiamiGA->Orlando, as well as beta_x->Orlando etc. etc. (for the
   beta upgrades we don't have the luxury of a db schema version change to trigger off.) If we can
   get this done earlier for the next release we can trigger off the schema vsn change again..
*)

module Names = Db_names

(** {Release-specific custom database upgrade rules} *)

(** The type of an upgrade rule. The rules should ideally be idempotent and composable.
    All new fields will have been created with default values and new tables will exist. *)
type upgrade_rule = {
  description: string;
  version: int * int; (** rule will be applied if the schema version is <= this number *)
  fn: Database.t -> Database.t;
}

(** Apply all the rules needed for the previous_version *)
let apply_upgrade_rules rules previous_version db = 
  debug "Looking for database upgrade rules:";
  let required_rules = List.filter (fun r -> previous_version <= r.version) rules in
  List.fold_left
    (fun db r ->
		debug "Applying database upgrade rule: %s" r.description;
		try
			r.fn db
		with exn ->
			error "Database upgrade rule '%s' failed: %s" r.description (Printexc.to_string exn);
			db
    ) db required_rules
  

(* GEORGE OEM -> BODIE/MNR *)	
let upgrade_bios_strings db =
	let oem_manufacturer =
		try
			let ic = open_in "/var/tmp/.previousInventory" in
			let rec find_oem_manufacturer () =
				let line = input_line ic in
				match Xapi_inventory.parse_inventory_entry line with
				| Some (k, v) when k = "OEM_MANUFACTURER" -> Some v
				| Some _ -> find_oem_manufacturer () 
				| None -> None
			in
			Pervasiveext.finally (find_oem_manufacturer) (fun () -> close_in ic)
		with _ -> None
	in
	let update_vms bios_strings =
		let ts = Database.tableset db in
		let vm_table = TableSet.find Names.vm ts in
		let bios_strings_kvs = String_marshall_helper.map (fun x->x) (fun x->x) bios_strings in
		let update_row row = 
			Row.add Names.bios_strings bios_strings_kvs row in
		let vm_table = Table.fold (fun r row tbl -> Table.add r (update_row row) tbl) vm_table Table.empty in
		set_table Names.vm vm_table
	in
	match oem_manufacturer with
	| Some oem ->
		info "Upgrade from OEM edition (%s)." oem;
		if String.has_substr oem "HP" then begin
			debug "Using old HP BIOS strings";
			update_vms Xapi_globs.old_hp_bios_strings db
		end else if String.has_substr oem "Dell" then begin
			debug "Using old Dell BIOS strings";
			update_vms Xapi_globs.old_dell_bios_strings db
		end	else db
	| None ->
		info "Upgrade from retail edition.";
		debug "Using generic BIOS strings";
		update_vms Xapi_globs.generic_bios_strings db

let update_snapshots db = 
	(* GEORGE -> MIDNIGHT RIDE *)
	let ts = Database.tableset db in
	let vm_table = TableSet.find Names.vm ts in
	let vm_rows = Table.rows vm_table in
	let update_snapshots vm_row vm_table : Table.t =
		let vm = Row.find Names.ref vm_row in
		let snapshot_rows = List.filter (fun s -> Row.find Names.snapshot_of s = vm) vm_rows in
		let compare s1 s2 =
			let t1 = Row.find Names.snapshot_time s1 in
			let t2 = Row.find Names.snapshot_time s2 in
			compare t1 t2 in
		let ordered_snapshot_rows = List.sort compare snapshot_rows in
		debug "Snapshots(%s) = {%s}" vm (String.concat ", " (List.map (fun s -> Row.find Names.ref s) ordered_snapshot_rows));
		let rec aux snaps vm_table = match snaps with
			| [] | [_] -> vm_table
			| s1 :: s2 :: t ->
				let row' = Row.add Names.parent (Row.find Names.ref s1) s2 in
				let vm_table = Table.add (Row.find Names.ref s2) s2 vm_table in
				aux (s2 :: t) vm_table in
		aux (ordered_snapshot_rows @ [ vm_row]) vm_table in
	let vm_table = Table.fold (fun _ vm_row tbl -> update_snapshots vm_row tbl) vm_table vm_table in
	set_table Names.vm vm_table db

(** A list of all the custom database upgrade rules known to the system. *)
let upgrade_rules = 
  let george = Datamodel.george_release_schema_major_vsn, Datamodel.george_release_schema_minor_vsn in
  [ { description = "Updating snapshot parent references";
      version = george;
      fn = update_snapshots };
    { description = "Upgrading VM BIOS strings";
      version = george;
      fn = upgrade_bios_strings } ]

(** {Generic database upgrade handling} *)

(** Automatically insert blank tables and new columns with default values *)
let generic_database_upgrade db =
  let existing_table_names = TableSet.fold (fun name _ acc -> name :: acc) (Database.tableset db) [] in
  let schema_table_names = Schema.table_names (Database.schema db) in
  let created_table_names = Listext.List.set_difference schema_table_names existing_table_names in
  let deleted_table_names = Listext.List.set_difference existing_table_names schema_table_names in
  let db = Database.update
	  (fun ts ->
		  List.fold_left (fun ts tblname ->
			  debug "Adding new database table: '%s'" tblname;
			  TableSet.add tblname Table.empty ts) ts created_table_names) db in
  
  (* for each table, go through and fill in missing default values *)
  List.fold_left
      (fun db tblname ->
		  let tbl = TableSet.find tblname (Database.tableset db) in
		  let schema = Schema.table tblname (Database.schema db) in
		  let rows = Table.rows tbl in
		  let add_fields_to_row objref r db : Database.t =
			  let row = Row.add_defaults schema r in
			  let tbl = Table.add objref row tbl in
			  set_table tblname tbl db in
		  Table.fold add_fields_to_row tbl db
	  ) db schema_table_names

