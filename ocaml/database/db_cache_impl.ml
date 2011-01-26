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

open Db_exn
open Db_lock

module D = Debug.Debugger(struct let name = "sql" end)
open D
module W = Debug.Debugger(struct let name = "db_write" end)
    
open Db_cache_types
open Db_action_helper
open Db_backend

let context = Context.make "db_cache"
	
(* This fn is part of external interface, so need to take lock *)
let get_table_from_ref objref =
    with_lock
		(fun () ->
			if Hashtbl.mem ref_table_map objref
			then Some (Hashtbl.find ref_table_map objref)
			else None)
		
let is_valid_ref objref =
    match (get_table_from_ref objref) with
		| Some _ -> true
		| None -> false
			
(* Read field from cache *)
let read_field tblname fldname objref =
    with_lock
		(fun () ->
			let row = find_row cache tblname objref in
			lookup_field_in_row row fldname)

let table_of_kvs kvs =
    let row = create_empty_row () in
    List.iter (fun (k,v)-> set_field_in_row row k v) kvs;
    row

let save_in_redo_log context entry =
    if Redo_log.is_enabled() then begin
        Redo_log.write_delta (Db_cache_types.generation_of_cache Db_backend.cache) entry
			(fun () -> (* the function which will be invoked if a database write is required instead of a delta *)
				Backend_xml.flush_db_to_redo_log Db_backend.cache
			)
    end

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
let write_field tblname objref fldname newval =
    with_lock
		(fun () ->
			(* if uuid or reference then check uniqueness constraints: *)
			if fldname=uuid_fname then begin
				check_unique_table_constraints tblname (table_of_kvs [(uuid_fname, newval)]);
				Ref_index.update_uuid objref newval;
			end else if fldname=reference_fname then
				check_unique_table_constraints tblname (table_of_kvs [(reference_fname, newval)])
			else if fldname=name_label_fname then
				Ref_index.update_name_label objref newval;
			
			let row = find_row cache tblname objref in
			let current_val = lookup_field_in_row row fldname in
			
			let other_tbl_refs = Eventgen.follow_references tblname in
			let other_tbl_refs_for_this_field =
				List.filter (fun (_,fld) -> fld=fldname) other_tbl_refs in
			
			let newval = ensure_utf8_xml newval in
			
			if current_val<>newval then
				begin
					W.debug "write_field %s,%s: %s |-> %s" tblname objref fldname newval;
					invalidate_indexes_for_specific_field tblname fldname;
					
					(* Update the field in the cache whether it's persistent or not *)
					set_field_in_row row fldname newval;
					
					(* then only mark written row as dirty if we persist writes on this table && persist changes on this field *)
					if (this_table_persists tblname) && (persist_field_changes tblname fldname) then
						begin
							(* Only flush to disk if persistent *)
							Db_dirty.set_all_row_dirty_status objref Db_dirty.Modified;
							Db_dirty.set_all_dirty_table_status tblname;
							Db_cache_types.increment Db_backend.cache;
							save_in_redo_log context (Redo_log.WriteField(tblname, objref, fldname, newval))
						end;
					
					let events_old_val =
						if is_valid_ref current_val then 
							Eventgen.events_of_other_tbl_refs
								(List.map (fun (tbl,fld) ->
									(tbl, current_val, Eventgen.find_get_record tbl ~__context:context ~self:current_val)) other_tbl_refs_for_this_field) 
						else []
					in
					
					let events_new_val =
						if is_valid_ref newval then
							Eventgen.events_of_other_tbl_refs
								(List.map (fun (tbl,fld) ->
									(tbl, newval, Eventgen.find_get_record tbl ~__context:context ~self:newval)) other_tbl_refs_for_this_field) 
						else [] 
					in
					
					(* Generate event *)
					let snapshot = Eventgen.find_get_record tblname ~__context:context ~self:objref in
					let record = snapshot() in
					List.iter (function 
						| tbl, ref, None ->
							error "Failed to send MOD event for %s %s" tbl ref;
							Printf.printf "Failed to send MOD event for %s %s\n%!" tbl ref;
						| tbl, ref, Some s ->
							events_notify ~snapshot:s tbl "mod" ref
					) events_old_val;
					begin match record with
						| None ->
							error "Failed to send MOD event for %s %s" tblname objref;
							Printf.printf "Failed to send MOD event for %s %s\n%!" tblname objref;
						| Some record ->
							events_notify ~snapshot:record tblname "mod" objref;
					end;
					List.iter (function 
						| tbl, ref, None ->
							error "Failed to send MOD event for %s %s" tbl ref;
							Printf.printf "Failed to send MOD event for %s %s\n%!" tbl ref;
						| tbl, ref, Some s -> 
							events_notify ~snapshot:s tbl "mod" ref
					) events_new_val;
				end)
		
(* Read specified field from tbl where where_field == where_value, using indexing *)
let read_set_ref rcd =
    with_lock
		(fun () ->
			(* See if index exists for this lookup, if not make index *)
			let index =
				try Hashtbl.find indexes (rcd.table, rcd.where_field, rcd.return)
				with _ ->
					begin
						let tbl = lookup_table_in_cache cache rcd.table in
						let rows = get_rowlist tbl in
						let new_index = Hashtbl.create (List.length rows) in
						let rec populate_index rows =
							match rows with
									[] -> ()
								| (r::rs) ->
									let indexed_field_value = lookup_field_in_row r rcd.where_field in
									let result_field_value  = lookup_field_in_row r rcd.return in
									add_to_index new_index (indexed_field_value, result_field_value);
									populate_index rs in
						populate_index rows; (* populate new index *)
						Hashtbl.replace indexes (rcd.table, rcd.where_field, rcd.return) new_index;
						new_index
					end in 
			(* Lookup query in index *)
			try Hashtbl.find index rcd.where_value with _ -> [])
		
(* setrefs contain the relationships from tbl to other tables in the form:
   local-classname, local-fieldname, remote-classname, remote-fieldname.
   db_read_record reads row from tbl with reference==objref [returning (fieldname, fieldvalue) list].
   and iterates through set-refs [returning (fieldname, ref list) list; where fieldname is the
   name of the Set Ref field in tbl; and ref list is the list of foreign keys from related
   table with remote-fieldname=objref] *)
let read_record tbl objref  =
    with_lock
		(fun ()->
			let row = find_row cache tbl objref (* !! need fields as well as values here !! *) in
			let fvlist = fold_over_fields (fun k d env -> (k,d)::env) row [] in
			let get_set_ref tbl fld objref =
				read_set_ref {table=tbl; return=reference_fname;
				where_field=fld; where_value=objref} in
			
			let look_up_related_table_and_field obj other full_name =
				(* Set(Ref t) is actually stored in the table t *)
				let this_end = obj.Datamodel_types.name, List.hd (full_name) in
				(* XXX: relationships should store full names *)
				let obj', fld' = Datamodel_utils.Relations.other_end_of Datamodel.all_api this_end in
				(obj', fld') in
			
			(* find datamodel object that corresponds to this table *)
			let obj = List.find (fun obj -> obj.Datamodel_types.name = tbl) api_objs in
			(* read its fields *)
			let obj_fields = Datamodel_utils.fields_of_obj obj in
			
			let rec set_refs ls =
				match ls with
						[] -> []
					| ({Datamodel_types.ty = Datamodel_types.Set(Datamodel_types.Ref clsname); full_name = full_name}::fs) ->
						let obj', fld' = look_up_related_table_and_field obj clsname full_name in
						(Escaping.escape_obj obj.Datamodel_types.name, (* local classname *)
						Escaping.escape_id full_name, (* local field *)
						Escaping.escape_obj obj', (* remote classname *)
						fld' (* remote fieldname *))::(set_refs fs)
					| _::fs -> set_refs fs in
			
			let setrefs = set_refs obj_fields in
			
			let sr_fields =
				List.map (fun (_,local_fieldname,remote_classname,remote_fieldname)->
					(local_fieldname,
					get_set_ref remote_classname remote_fieldname objref)) setrefs in
			(fvlist, sr_fields))
		
(* Delete row from tbl *)
let delete_row tblname objref =
	let tbl = lookup_table_in_cache cache tblname in
	(* Look up the row first: in the event it doesn't exist, this will 
	   immediately failed with a DBCache_NotFound *)
	let (_: row) = lookup_row_in_table tbl tblname objref in
    (* NB we generate the delete event BEFORE deleting the object 
	   but then generate the mod events afterwards *)
    let generate_delete_event () = 
		match Eventgen.find_get_record tblname ~__context:context ~self:objref () with
			| None ->
				error "Failed to generate DEL event for %s %s" tblname objref;
				Printf.printf "Failed to generate DEL event for %s %s\n%!" tblname objref;
			| Some snapshot ->
				events_notify ~snapshot tblname "del" objref in
    (* Return a thunk which will cause the mod events to be generated
	   containing the object states at the time the thunk is evaluated. 
	   We create this closure while the objref is still valid *)
    let lazily_generate_mod_events () = 
		let other_tbl_refs = Eventgen.follow_references tblname in
		let other_tbl_refs =
			List.fold_left (fun accu (remote_tbl,fld) ->
				let (kv,_) = read_record tblname objref in 
				let fld_value = List.assoc fld kv in
				if is_valid_ref fld_value 
				then (remote_tbl, fld_value, Eventgen.find_get_record remote_tbl ~__context:context ~self:fld_value) :: accu 
				else accu) 
				[] other_tbl_refs in
		fun () ->
			let other_tbl_ref_events = Eventgen.events_of_other_tbl_refs other_tbl_refs in
			List.iter (function
				| tbl, ref, None ->
					error "Failed to generate MOD event on %s %s" tbl ref;
					Printf.printf "Failed to generate MOD event on %s %s\n%!" tbl ref;
				| tbl, ref, Some s -> 
					events_notify ~snapshot:s tbl "mod" ref
			) other_tbl_ref_events in	
    with_lock
		(fun () ->
			W.debug "delete_row %s (%s)" tblname objref;
			(* send event *)
			generate_delete_event();
			let mod_events = lazily_generate_mod_events () in
			
			invalidate_indexes tblname;
			
			remove_row_from_table tbl objref;
			
			(* Notify each db connection of delete *)
			List.iter (fun dbconn->Backend_xml.notify_delete dbconn tblname objref) (Db_conn_store.read_db_connections());
			
			if (this_table_persists tblname) then
				begin
					(* Update cache dirty status *)
					Db_dirty.clear_all_row_dirty_status objref;
					Db_dirty.set_all_dirty_table_status tblname;
					Db_cache_types.increment Db_backend.cache;
					save_in_redo_log context (Redo_log.DeleteRow(tblname, objref))
				end;
			Ref_index.remove objref;
			remove_ref_from_table_map objref;
			(* send the rest of the events *)
			mod_events ())
		
(* Create new row in tbl containing specified k-v pairs *)
let create_row tblname kvs new_objref =
	
    (* Ensure values are valid for UTF-8-encoded XML. *)
    let kvs = List.map (fun (key, value) -> (key, ensure_utf8_xml value)) kvs in
	
    (* fill in default values specifed in datamodel if kv pairs for these are not supplied already *)
    let kvs = add_default_kvs kvs tblname in
	
    (* add the reference to the row itself *)
    let kvs = (reference, new_objref) :: kvs in
	
    let generate_create_event() =
		let snapshot = Eventgen.find_get_record tblname ~__context:context ~self:new_objref in
		let other_tbl_refs = Eventgen.follow_references tblname in
		let other_tbl_refs =
			List.fold_left (fun accu (tbl,fld) ->
				let fld_value = List.assoc fld kvs in
				if is_valid_ref fld_value 
				then (tbl, fld_value, Eventgen.find_get_record tbl ~__context:context ~self:fld_value) :: accu
				else accu) 
				[] other_tbl_refs in
		let other_tbl_events = Eventgen.events_of_other_tbl_refs other_tbl_refs in
		begin match snapshot() with
			| None ->
				error "Failed to generate ADD event for %s %s" tblname new_objref;
				Printf.printf "Failed to generate ADD event for %s %s\n%!" tblname new_objref;
			| Some snapshot ->
				events_notify ~snapshot tblname "add" new_objref;
		end;
		List.iter (function
			| tbl, ref, None ->
				error "Failed to generate MOD event for %s %s" tbl ref;
				Printf.printf "Failed to generate MOD event for %s %s\n%!" tbl ref;
			| tbl, ref, Some s ->
				events_notify ~snapshot:s tbl "mod" ref
		) other_tbl_events in
	
    with_lock
		(fun () ->
			W.debug "create_row %s (%s) [%s]" tblname new_objref (String.concat "," (List.map (fun (k,v)->"("^k^","^"v"^")") kvs));
			invalidate_indexes tblname;
			let newrow = table_of_kvs kvs in
			let tbl = lookup_table_in_cache cache tblname in
			check_unique_table_constraints tblname newrow;
			set_row_in_table tbl new_objref newrow;
			if (this_table_persists tblname) then
				begin
					Db_dirty.set_all_row_dirty_status new_objref Db_dirty.New;
					Db_dirty.set_all_dirty_table_status tblname;
					Db_cache_types.increment Db_backend.cache;
					save_in_redo_log context (Redo_log.CreateRow(tblname, new_objref, kvs))
				end;
			add_ref_to_table_map new_objref tblname (* track ref against this table *);
			let uuid = lookup_field_in_row newrow uuid_fname in
			let name_label = try Some (lookup_field_in_row newrow name_label_fname) with _ -> None in
			Ref_index.insert {Ref_index.name_label = name_label; Ref_index.uuid = uuid; Ref_index._ref = new_objref };
			
			(* generate events *)
			begin
				try
					generate_create_event();
				with Not_found ->
					error "Failed to send a create event for %s %s" tblname new_objref
			end
		)
		
(* Do linear scan to find field values which match where clause *)
let read_field_where rcd =
    with_lock
		(fun () ->
			let tbl = lookup_table_in_cache cache rcd.table in
			let rec do_find tbl acc =
				match tbl with
						[] -> acc
					| (r::rs) ->
						let fv = lookup_field_in_row r rcd.where_field in
						if fv=rcd.where_value then do_find rs ((lookup_field_in_row r rcd.return)::acc) 
						else do_find rs acc in
			let rows = get_rowlist tbl in
			do_find rows []
		)
		
let db_get_by_uuid tbl uuid_val =
    match (read_field_where
        {table=tbl; return=reference;
        where_field=uuid; where_value=uuid_val}) with
		| [] -> raise (Read_missing_uuid (tbl, "", uuid_val))
		| [r] -> r
		| _ -> raise (Too_many_values (tbl, "", uuid_val))
			
(** Return reference fields from tbl that matches specified name_label field *)
let db_get_by_name_label tbl label =
    read_field_where
        {table=tbl; return=reference;
        where_field=(Escaping.escape_id ["name"; "label"]);
        where_value=label}
		
(* Read references from tbl *)
let read_refs tblname =
    with_lock
		(fun () ->
			get_reflist (lookup_table_in_cache cache tblname))
		
(* Return a list of all the references for which the expression returns true. *)
let find_refs_with_filter (tblname: string) (expr: Db_filter_types.expr) = 
    with_lock
		(fun ()->
			let tbl = lookup_table_in_cache cache tblname in
			let rows = get_rowlist tbl in
			let eval_val row = function
				| Db_filter_types.Literal x -> x
				| Db_filter_types.Field x -> lookup_field_in_row row x in
			let rows = List.filter (fun row ->Db_filter.eval_expr (eval_val row) expr) rows in
			List.map (fun row -> lookup_field_in_row row reference_fname) rows)
		
let read_records_where tbl expr =
    with_lock
		(fun ()->
			let reqd_refs = find_refs_with_filter tbl expr in
			List.map (fun ref->ref, read_record tbl ref) reqd_refs
		)
		
let process_structured_field (key,value) tbl fld objref proc_fn_selector =
	
    (* Ensure that both keys and values are valid for UTF-8-encoded XML. *)
    let key = ensure_utf8_xml key in
    let value = ensure_utf8_xml value in
	
    let add_set = (fun fv->add_key_to_set key fv) in
    let remove_set = (fun fv->List.filter (function SExpr.String x -> x <> key | _ -> true) fv) in
    let add_map = (fun fv->
		let kv = SExpr.Node [ SExpr.String key; SExpr.String value ] in
		let duplicate = List.fold_left (||) false 
			(List.map (function SExpr.Node (SExpr.String k :: _) when k = key -> true
				| _ -> false) fv) in
		if duplicate then begin
			error "Duplicate key in set or map: table %s; field %s; ref %s; key %s" tbl fld objref key;
			raise (Duplicate_key (tbl,fld,objref,key));
		end;
		kv::fv) in
    let remove_map =
		(fun fv->List.filter (function SExpr.Node [ SExpr.String x; _ ] -> x <> key
			| _ -> true) fv) in
    let proc_fn =
		begin
			match proc_fn_selector with
					AddSet -> add_set
				| RemoveSet -> remove_set
				| AddMap -> add_map
				| RemoveMap -> remove_map
		end in
    with_lock
		(fun () ->
			let row = find_row cache tbl objref in
			let existing_str = lookup_field_in_row row fld in
			let existing = parse_sexpr existing_str in
			let processed = proc_fn existing in
			let processed_str = SExpr.string_of (SExpr.Node processed) in
			write_field tbl objref fld processed_str)
		
		
(* -------------------------------------------------------------------- *)
		
		
(* Executed on the master to post-process database after populating cache from db stored on disk *)
let post_populate_hook () =
    (* Remove the temporary file used for staging from the metadata LUN --
     * there's no need to keep it and it's preferable for it not to hang
     * around. *)
    Unixext.unlink_safe Xapi_globs.ha_metadata_db;
    (* non-persistent fields will have been flushed to disk anyway [since non-persistent just means dont trigger a flush
	   if I change]. Hence we blank non-persistent fields with a suitable empty value, depending on their type *)
    Db_backend.blow_away_non_persistent_fields();
    (* Flush the in-memory cache to the redo-log *)
    Backend_xml.flush_db_to_redo_log Db_backend.cache
		
let populate_cache () =
    let connections = Db_conn_store.read_db_connections () in
    
    (* Include a fake connection representing the HA metadata db
	   (if available). This isn't a full flushing connection per-se but
	   is only considered as a population source. *)
    let fake_ha_dbconn = { Parse_db_conf.dummy_conf with
        Parse_db_conf.path = Xapi_globs.ha_metadata_db } in
    let connections = 
		if Sys.file_exists Xapi_globs.ha_metadata_db
		then fake_ha_dbconn :: connections else connections in
	
    let fake_gen_dbconn = { Parse_db_conf.dummy_conf with
        Parse_db_conf.path = Xapi_globs.gen_metadata_db } in
    let connections = 
		if Sys.file_exists Xapi_globs.gen_metadata_db
		then fake_gen_dbconn :: connections else connections in
	
    (* If we have a temporary_restore_path (backup uploaded in previous run of xapi process) then restore from that *)
    let db = 
		if Sys.file_exists Xapi_globs.db_temporary_restore_path then begin
			(* we know that the backup is XML format so, to get the manifest, we jump right in and use the xml backend directly here.. *)
			let manifest = Backend_xml.populate_and_read_manifest Parse_db_conf.backup_file_dbconn in
			Db_backend.post_restore_hook manifest;
			(* delete file that contained backup *)
			Db_backend.try_and_delete_db_file Xapi_globs.db_temporary_restore_path;
			Parse_db_conf.backup_file_dbconn
		end
		else (* if there's no backup to restore from then.. *)
			begin
				(* Check schema vsn is current; if not try and upgrade; if can't do that then fail startup.. *)
				let most_recent_db = Db_connections.pick_most_recent_db connections in
				(* populate gets all field names from the existing (old) db file, not the (current) schema... which is nice: *)
				Backend_xml.populate most_recent_db;
				most_recent_db
			end in
    (* Always perform the generic database upgrade stuff *)
    Db_upgrade.generic_database_upgrade ();
	
    (* Then look to see whether we have specific upgrade rules to consider *)
    if Sys.file_exists db.Parse_db_conf.path then Db_upgrade.maybe_upgrade db;
	
    post_populate_hook ()
		
let sync_all_db_connections() =
    (* Unconditionally force-flush all databases. *)
    List.iter Db_connections.force_flush_all (List.map snd (Db_connections.get_dbs_and_gen_counts()))
		
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
					let min (x,y) = if x<=y then x else y in
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
let initialise () =
    populate_cache();
    sync_all_db_connections();
    spawn_db_flush_threads()
		
(* entry point for xapi-db-process; initialises a db cache without syncing all db connections "to tip" *)
let initialise_db_cache_nosync() =
    populate_cache();
    spawn_db_flush_threads()
		
let dump_db_cache fd =
	let db_cache_manifest = Db_cache_types.manifest_of_cache Db_backend.cache in
    let time = Unix.gettimeofday() in
    (* Snapshot the cache (uses the lock) and then slowly serialise the copy *)
    Db_xml.To.fd fd (db_cache_manifest, snapshot Db_backend.cache);
    debug "Written xml to fd: (time %f)" (Unix.gettimeofday() -. time)
		
(** Return an association list of table name * record count *)
let stats () = 
    with_lock 
		(fun () ->
			fold_over_tables (fun name tbl acc ->
				let size = fold_over_rows (fun _ _ acc -> acc + 1) tbl 0 in
				(name, size) :: acc) Db_backend.cache [])



