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
open Db_exn
open Db_lock
open Db_cache_types
open Pervasiveext

module D = Debug.Debugger(struct let name = "sql" end)
open D

(* --------------------- Constants/data-structures for storing db contents *)

let db_FLUSH_TIMER=2.0 (* flush db write buffer every db_FLUSH_TIMER seconds *)
let display_sql_writelog_val = ref true (* compute/write sql-writelog debug string *)

(* The cache itself: *)
let database : Db_cache_types.Database.t ref = ref (Db_cache_types.Database.make (Schema.of_datamodel ()))

(* --------------------- Util functions on db datastructures *)

let update_database f = 
	database := f (!database)

let get_database () = !database


(* !!! Right now this is called at cache population time. It would probably be preferable to call it on flush time instead, so we
   don't waste writes storing non-persistent field values on disk.. At the moment there's not much to worry about, since there are
   few non-persistent fields. However, if this number increases we may want to consider calling this fn on every flush instead.. *)

(* non-persistent fields will have been flushed to disk anyway [since non-persistent just means dont trigger a flush
   if I change]. Hence we blank non-persistent fields with a suitable empty value, depending on their type *)
let blow_away_non_persistent_fields (schema: Schema.t) db =
	(* Generate a new row given a table schema *)
	let row schema row : Row.t =
		Row.fold 
			(fun name v acc ->
				try
					let col = Schema.Table.find name schema in
					let v' = if col.Schema.Column.persistent then v else col.Schema.Column.empty in
					Row.add name v' acc
				with Not_found ->
					Printf.printf "Skipping unknown column: %s\n%!" name;
					acc) row Row.empty in
	(* Generate a new table *)
	let table tblname tbl : Table.t =
		let schema = Schema.Database.find tblname schema.Schema.database in
		Table.fold
			(fun objref r acc ->
				let r = row schema r in
				Table.add objref r acc) tbl Table.empty in
	Database.update
		(fun ts -> 
			TableSet.fold 
				(fun tblname tbl acc ->
					let tbl' = table tblname tbl in
					TableSet.add tblname tbl' acc) ts TableSet.empty)
		db
  
(* after restoring from backup, we take the master's host record and make it reflect us *)
let post_restore_hook db =
  debug "Executing post_restore_hook";
  let my_installation_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
  let my_control_uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in

  let not_found = "" in

  (* Look up the pool master: *)
  let pools = TableSet.find Db_names.pool (Database.tableset db) in
  let master = Table.fold (fun _ref r acc -> Row.find Db_names.master r) pools not_found in

  let update_master_host db : Database.t = 
	  if master = not_found then begin
		  debug "No master record to update";
		  db
	  end else begin
		  set_field_in_row Db_names.host master Db_names.uuid my_installation_uuid db
	  end in

  let update_master_dom0 db : Database.t = 
	  (* Look up the pool master's control domain: *)
	  let vms = TableSet.find Db_names.vm (Database.tableset db) in
	  let master_dom0 = Table.fold (fun _ref r acc -> 
		  if
			  Row.find Db_names.resident_on r = master && 
			  (Row.find Db_names.is_control_domain r = "true") 
		  then _ref else acc) vms not_found in  
	  if master_dom0 = not_found then begin
		  debug "No master control domain record to update";
		  db
	  end else begin
		  set_field_in_row Db_names.vm master_dom0 Db_names.uuid my_control_uuid db
	  end in

  (update_master_host ++ update_master_dom0) db

