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
		  let add_fields_to_row objref r tbl : Table.t =
			  let row = Row.add_defaults schema r in
			  Table.add objref row tbl in
			let tbl = Table.fold add_fields_to_row tbl Table.empty in
			set_table tblname tbl db
	  ) db schema_table_names

