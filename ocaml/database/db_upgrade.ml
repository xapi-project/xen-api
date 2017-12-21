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

module D = Debug.Make(struct let name = "xapi" (* this is set to 'xapi' deliberately! :) *) end)
open D

open Db_cache_types

(** Automatically insert blank tables and new columns with default values *)
let generic_database_upgrade db =
  let existing_table_names = TableSet.fold (fun name _ _ acc -> name :: acc) (Database.tableset db) [] in
  let schema_table_names = Schema.table_names (Database.schema db) in
  let created_table_names = Xapi_stdext_std.Listext.List.set_difference schema_table_names existing_table_names in
  let g = Manifest.generation (Database.manifest db) in
  let db = Database.update
      (fun ts ->
         List.fold_left (fun ts tblname ->
             debug "Adding new database table: '%s'" tblname;
             TableSet.add g tblname Table.empty ts) ts created_table_names) db in

  (* for each table, go through and fill in missing default values *)
  let open Xapi_stdext_deprecated.Fun in
  List.fold_left
    (fun db tblname ->
       let tbl = TableSet.find tblname (Database.tableset db) in
       let schema = Schema.table tblname (Database.schema db) in
       let add_fields_to_row objref _ r tbl : Table.t =
         let row = Row.add_defaults g schema r in
         Table.add g objref row tbl in
       let tbl = Table.fold add_fields_to_row tbl Table.empty in
       let g = Manifest.generation (Database.manifest db) in
       ((Database.update ++ (TableSet.update g tblname Table.empty)) (fun _ -> tbl)) db
    ) db schema_table_names

