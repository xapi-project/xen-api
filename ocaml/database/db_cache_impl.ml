(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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

(** An in-memory cache, used by pool master *)

(* Locking strategy:
   1. functions which read/modify/write must acquire the db lock. Such
      functions have the suffix "_locked" to clearly identify them.
   2. functions which only read must only call "get_database" once,
      to ensure they see a consistent snapshot.
*)
open Db_exn
open Db_lock
open Pervasiveext
open Threadext

module D = Debug.Debugger(struct let name = "sql" end)
open D
module W = Debug.Debugger(struct let name = "db_write" end)
    
open Db_cache_types
open Db_ref

(* Only needed by the DB_ACCESS signature *)
let initialise () = ()

(* This fn is part of external interface, so need to take lock *)
let get_table_from_ref t objref =
	try
		Some (Database.table_of_ref objref (get_database t))
	with Not_found -> 
		None
		
let is_valid_ref t objref =
    match (get_table_from_ref t objref) with
		| Some _ -> true
		| None -> false
			
let read_field_internal t tblname fldname objref db = 
	Row.find fldname (Table.find_exn tblname objref (TableSet.find tblname (Database.tableset db)))

(* Read field from cache *)
let read_field t tblname fldname objref =
	read_field_internal t tblname fldname objref (get_database t)




(** Finds the longest XML-compatible UTF-8 prefix of the given *)
(** string, by truncating the string at the first incompatible *)
(** character. Writes a warning to the debug log if truncation *)
(** occurs.                                                    *)
let ensure_utf8_xml string =
	let length = String.length string in
	let prefix = Encodings.UTF8_XML.longest_valid_prefix string in
	if length > String.length prefix then
		warn "string truncated to: '%s'." prefix;
	prefix

		
(* Write field in cache *)
let write_field_locked t tblname objref fldname newval =
	let current_val = get_field tblname objref fldname (get_database t) in
	update_database t (set_field tblname objref fldname newval);
	Database.notify (WriteField(tblname, objref, fldname, current_val, newval)) (get_database t)
			
let write_field t tblname objref fldname newval =
	with_lock (fun () -> 
		write_field_locked t tblname objref fldname newval)

let refresh_row t tblname objref =
	update_database t (update_generation tblname objref);
	Database.notify (RefreshRow(tblname, objref)) (get_database t)

(* This function *should* only be used by db_actions code looking up Set(Ref _) fields:
   if we detect another (illegal) use we log the problem and fall back to a slow scan *)
let read_set_ref t rcd =
	let db = get_database t in
	(* The where_record should correspond to the 'one' end of a 'one to many' *)
	let one_tbl = rcd.table in
	let one_fld = rcd.where_field in
	let rels = 
		try
			Schema.one_to_many one_tbl (Database.schema db)
		with Not_found -> 
			raise (Db_exn.DBCache_NotFound("missing table", one_tbl, ""))
	in
	(* This is an 'illegal' use if: *)
	let illegal = rcd.return <> Db_names.ref || (List.filter (fun (a, _, _) -> a = one_fld) rels = []) in
	if not illegal then begin
		let _, many_tbl, many_fld = List.find (fun (a, _, _) -> a = one_fld) rels in
		let objref = rcd.where_value in
		
		let str = read_field_internal t many_tbl many_fld objref db in
		String_unmarshall_helper.set (fun x -> x) str		
	end else begin
		error "Illegal read_set_ref query { table = %s; where_field = %s; where_value = %s; return = %s }; falling back to linear scan" rcd.table rcd.where_field rcd.where_value rcd.return;
		Printf.printf "Illegal read_set_ref query { table = %s; where_field = %s; where_value = %s; return = %s }; falling back to linear scan\n%!" rcd.table rcd.where_field rcd.where_value rcd.return;
		let tbl = TableSet.find rcd.table (Database.tableset db) in
		Table.fold
			(fun rf _ _ row acc ->
				if Row.find rcd.where_field row = rcd.where_value 
				then Row.find rcd.return row :: acc else acc)
			tbl []
	end
			

(* setrefs contain the relationships from tbl to other tables in the form:
   local-classname, local-fieldname, remote-classname, remote-fieldname.
   db_read_record reads row from tbl with reference==objref [returning (fieldname, fieldvalue) list].
   and iterates through set-refs [returning (fieldname, ref list) list; where fieldname is the
   name of the Set Ref field in tbl; and ref list is the list of foreign keys from related
   table with remote-fieldname=objref] *)
let read_record t tblname objref  =
	let db = get_database t in
	let tbl = TableSet.find tblname (Database.tableset db) in
	let row = Table.find_exn tblname objref tbl in
	let fvlist = Row.fold (fun k _ _ d env -> (k,d)::env) row [] in
	(* Unfortunately the interface distinguishes between Set(Ref _) types and 
	   ordinary fields *)
	let schema = Schema.table tblname (Database.schema db) in
	let set_ref = List.filter (fun (k, _) ->
		try
			let column = Schema.Table.find k schema in
			column.Schema.Column.issetref
		with Not_found as e ->
			Printf.printf "Failed to find table %s in schema\n%!" k;
			raise e
	) fvlist in
	(* the set_ref fields must be converted back into lists *)
	let set_ref = List.map (fun (k, v) -> 
		k, String_unmarshall_helper.set (fun x -> x) v) set_ref in
	(fvlist, set_ref)

(* Delete row from tbl *)
let delete_row_locked t tblname objref =
	W.debug "delete_row %s (%s)" tblname objref;
	
	let tbl = TableSet.find tblname (Database.tableset (get_database t)) in
	let row = Table.find_exn tblname objref tbl in
	
	let db = get_database t in
	Database.notify (PreDelete(tblname, objref)) db;
	update_database t (remove_row tblname objref);
	Database.notify (Delete(tblname, objref, Row.fold (fun k _ _ v acc -> (k, v) :: acc) row [])) (get_database t)
		
let delete_row t tblname objref = 
	with_lock (fun () -> delete_row_locked t tblname objref)

(* Create new row in tbl containing specified k-v pairs *)
let create_row_locked t tblname kvs' new_objref =
	
    (* Ensure values are valid for UTF-8-encoded XML. *)
    let kvs' = List.map (fun (key, value) -> (key, ensure_utf8_xml value)) kvs' in

    (* we add the reference to the row itself so callers can use read_field_where to
	   return the reference: awkward if it is just the key *)
    let kvs' = (Db_names.ref, new_objref) :: kvs' in
	let g = Manifest.generation (Database.manifest (get_database t)) in
	let row = List.fold_left (fun row (k, v) -> Row.add g k v row) Row.empty kvs' in
	let schema = Schema.table tblname (Database.schema (get_database t)) in
    (* fill in default values if kv pairs for these are not supplied already *)
	let row = Row.add_defaults g schema row in
	W.debug "create_row %s (%s) [%s]" tblname new_objref (String.concat "," (List.map (fun (k,v)->"("^k^","^"v"^")") kvs'));
	update_database t (add_row tblname new_objref row);
	Database.notify (Create(tblname, new_objref, Row.fold (fun k _ _ v acc -> (k, v) :: acc) row [])) (get_database t)
		
let create_row t tblname kvs' new_objref =
	with_lock (fun () -> create_row_locked t tblname kvs' new_objref)

(* Do linear scan to find field values which match where clause *)
let read_field_where t rcd =
	let db = get_database t in
	let tbl = TableSet.find rcd.table (Database.tableset db) in
	Table.fold
		(fun r _ _ row acc ->
			let field = Row.find rcd.where_field row in
			if field = rcd.where_value then Row.find rcd.return row :: acc else acc
		) tbl []
		
let db_get_by_uuid t tbl uuid_val =
    match (read_field_where t
        {table=tbl; return=Db_names.ref;
        where_field=Db_names.uuid; where_value=uuid_val}) with
		| [] -> raise (Read_missing_uuid (tbl, "", uuid_val))
		| [r] -> r
		| _ -> raise (Too_many_values (tbl, "", uuid_val))
			
(** Return reference fields from tbl that matches specified name_label field *)
let db_get_by_name_label t tbl label =
    read_field_where t
        {table=tbl; return=Db_names.ref;
        where_field=(Escaping.escape_id ["name"; "label"]);
        where_value=label}
		
(* Read references from tbl *)
let read_refs t tblname =
	let tbl = TableSet.find tblname (Database.tableset (get_database t)) in
	Table.fold (fun r _ _ _ acc -> r :: acc) tbl []
		
(* Return a list of all the refs for which the expression returns true. *)
let find_refs_with_filter t (tblname: string) (expr: Db_filter_types.expr) = 
	let db = get_database t in
	let tbl = TableSet.find tblname (Database.tableset db) in
	let eval_val row = function
		| Db_filter_types.Literal x -> x
		| Db_filter_types.Field x -> Row.find x row in
	Table.fold
		(fun r _ _ row acc ->
			if Db_filter.eval_expr (eval_val row) expr
			then Row.find Db_names.ref row :: acc else acc
		) tbl []
		
let read_records_where t tbl expr =
	let reqd_refs = find_refs_with_filter t tbl expr in
	List.map (fun ref->ref, read_record t tbl ref) reqd_refs
	
let process_structured_field_locked t (key,value) tblname fld objref proc_fn_selector =
	
    (* Ensure that both keys and values are valid for UTF-8-encoded XML. *)
    let key = ensure_utf8_xml key in
    let value = ensure_utf8_xml value in
	
	let tbl = TableSet.find tblname (Database.tableset (get_database t)) in
	let row = Table.find_exn tblname objref tbl in
	let existing_str = Row.find fld row in
	let new_str = match proc_fn_selector with
		| AddSet -> add_to_set key existing_str
		| RemoveSet -> remove_from_set key existing_str
		| AddMap -> 
			begin
				try
					add_to_map key value existing_str
				with Duplicate ->
					error "Duplicate key in set or map: table %s; field %s; ref %s; key %s" tblname fld objref key;
					raise (Duplicate_key (tblname,fld,objref,key));
			end
		| RemoveMap -> remove_from_map key existing_str in
	write_field t tblname objref fld new_str
	
let process_structured_field t (key,value) tblname fld objref proc_fn_selector =
	with_lock (fun () -> 
		process_structured_field_locked t (key,value) tblname fld objref proc_fn_selector)
	
(* -------------------------------------------------------------------- *)
	
let load connections default_schema =
    
	(* We also consider populating from the HA metadata LUN and the general metadata LUN *)
	let connections = 
		Parse_db_conf.make Xapi_globs.ha_metadata_db ::
			(Parse_db_conf.make Xapi_globs.gen_metadata_db) :: connections in
	
    (* If we have a temporary_restore_path (backup uploaded in previous run of xapi process) then restore from that *)
    let populate db =
		 match Db_connections.choose connections with
			 | Some c -> Backend_xml.populate default_schema c
			 | None -> db in (* empty *) 

	let empty = Database.update_manifest (Manifest.update_schema (fun _ -> Some (default_schema.Schema.major_vsn, default_schema.Schema.minor_vsn))) (Database.make default_schema) in

	let db = 
		((Db_backend.blow_away_non_persistent_fields default_schema)
		++ Db_upgrade.generic_database_upgrade
		++ populate) empty in
		
	db

		
let sync conns db =
    (* Flush the in-memory cache to the redo-log *)
    Redo_log.flush_db_to_all_active_redo_logs db;
	(* and then to the filesystem *)
	List.iter (fun c -> Db_connections.flush c db) conns
		
let flush_dirty dbconn = Db_connections.flush_dirty_and_maybe_exit dbconn None
let flush_and_exit dbconn ret_code = ignore (Db_connections.flush_dirty_and_maybe_exit dbconn (Some ret_code))
	
	
let spawn_db_flush_threads() =
    (* Spawn threads that flush cache to db connections at regular intervals *)
    List.iter
		(fun dbconn ->
			ignore (Thread.create 
				(fun ()->
					Db_connections.inc_db_flush_thread_refcount();
					let db_path = dbconn.Parse_db_conf.path in
					Debug.name_thread ("dbflush ["^db_path^"]");
					let my_writes_this_period = ref 0 in
					
					(* the collesce_period_start records the time of the last write *)
					let coallesce_period_start = ref (Unix.gettimeofday()) in
					let period_start = ref (Unix.gettimeofday()) in
					
					(* we set a coallesce period of min(5 mins, write_limit_period / write_limit_write_cycles) *)
					(* if we're not write limiting then set the coallesce period to 5 minutes; otherwise set coallesce period to divide the
					   number of write cycles across the ... 
					*)
					let coallesce_time = float_of_int (5*60) (* coallesce writes for 5 minutes to avoid serializing db to disk all the time. *) in
					debug "In memory DB flushing thread created [%s]. %s" db_path
						(if dbconn.Parse_db_conf.mode <> Parse_db_conf.No_limit then
							"Write limited with coallesce_time="^(string_of_float coallesce_time)
						else "");
					(* check if we are currently in a coallescing_period *)
					let in_coallescing_period() = 
						(Unix.gettimeofday() -. !coallesce_period_start < coallesce_time) in
					
					while (true) do
						try
							begin
								Thread.delay Db_backend.db_FLUSH_TIMER;
								(* If I have some writing capacity left in this write period then consider doing a write; or
								   if the connection is not write-limited then consider doing a write too.
								   We also have to consider doing a write if exit_on_next_flush is set: because when this is
								   set (by a signal handler) we want to do a flush whether or not our write limit has been
								   exceeded.
								*)
								if !Db_connections.exit_on_next_flush (* always flush straight away; this request is urgent *) ||
									(* otherwise, we only write if (i) "coalesscing period has come to an end"; and (ii) "write limiting requirements are met": *)
									((not (in_coallescing_period())) (* see (i) above *) &&
										((!my_writes_this_period < dbconn.Parse_db_conf.write_limit_write_cycles) || dbconn.Parse_db_conf.mode = Parse_db_conf.No_limit (* (ii) above *)
										)
									)
								then
									begin
										(* debug "[%s] considering flush" db_path; *)
										let was_anything_flushed = Threadext.Mutex.execute Db_lock.global_flush_mutex (fun ()->flush_dirty dbconn) in
										if was_anything_flushed then
											begin
												my_writes_this_period := !my_writes_this_period + 1;
												(* when we do a write, reset the coallesce_period_start to now -- recall that this
												   variable tracks the time since last write *)
												coallesce_period_start := Unix.gettimeofday()
											end
									end;
								(* else debug "[%s] not flushing because write-limit exceeded" db_path; *)
								(* Check to see if the current write period has finished yet.. *)
								if (Unix.gettimeofday() -. !period_start > (float_of_int dbconn.Parse_db_conf.write_limit_period)) then
									begin
										(* debug "[%s] resetting write-limit counters: start of new period" db_path; *)
										(* We're at the start of a new writing period! *)
										period_start := Unix.gettimeofday();
										my_writes_this_period := 0;
									end
										(* else debug "[%s] not resetting write-limit counters: not in new period yet" db_path *)
							end
						with
								e -> debug "Exception in DB flushing thread: %s" (Printexc.to_string e)
					done) ())
		) (Db_conn_store.read_db_connections())
		
		
(* Called by server at start-of-day to initialiase cache. Populates cache and starts flushing threads *)
let make t connections default_schema =
    let db = load connections default_schema in
	let db = Database.reindex db in
	update_database t (fun _ -> db);

    spawn_db_flush_threads()
		

(** Return an association list of table name * record count *)
let stats t = 
	TableSet.fold (fun name _ _ tbl acc ->
		let size = Table.fold (fun _ _ _ _ acc -> acc + 1) tbl 0 in
		(name, size) :: acc)
		(Database.tableset (get_database t))
		[]




