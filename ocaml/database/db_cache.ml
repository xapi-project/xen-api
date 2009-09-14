

(* ---------------------------------------------------------------------------------------------
   The dbcache provides low-level access to the data in the db (both getting and setting).
   It is below the event-level: calling the functions in this module will not cause any events
   to be generated. For that reason, regular xapi code accessing the database must call in via
   the Db.* API (or, absolute worst-case (!) via the DB_ACCESS layer)

   The DBCache module contains 2 instances of the DB_CACHE module type: one instance for pool
   masters (that is an in-memory db-cache, with async write log pushed to sqlite db for persistent
   store); and one instance for pool slaves (that calls a marshalling protocol that retrieves/
   modifies directly from the ppol master).

   The DBCache module is also itself an instance of DB_CACHE: it's DB_CACHE fns are just a
   dispatcher (yes I know it's just a vtable and I could use objects ;) that calls Master/Slave
   depending on value in Pool_role.is_master ()
   --------------------------------------------------------------------------------------------- *)

(* Note: read_record could be improved - see comment below. It transfers data over network needlessly
   (although not much data really) -- main problem is it's somewhat confusing in its current
   form *)

open Db_lock
open Db_exn

module D=Debug.Debugger(struct let name="db_cache" end)
open D

type database_mode = Master | Slave

let database_mode : database_mode option ref = ref None 

exception Must_initialise_database_mode

(** Interface of DB_Cache implementations *)
module type DB_CACHE =
sig

  val dump_db_cache : Db_cache_types.db_dump_manifest -> Unix.file_descr -> unit

  val stats : unit -> (string * int) list

  val populate_cache : unit -> unit
  val populate_from  : Parse_db_conf.db_connection -> unit (* populate cache from specified file/format *)
  val spawn_db_flush_threads : unit -> unit

  (* Called by server to initialise db resources.
     Master implementation populates cache _and_ spawns db flush threads;
     Slave implementation sets up resources reqd for master connection + associated watchdog
  *)
  val initialise_db_cache : unit -> unit

  (* This will initialise a dbcache, but without syncing all db connections to the latest vsn;
     it's used by xapi-db-tool -- allowing it to read the xapi database without generating
     any writes to disk.. *)
  val initialise_db_cache_nosync : unit -> unit

  val display_sql_writelog : bool -> unit
  val flush_dirty : Parse_db_conf.db_connection -> bool
  val flush_and_exit : Parse_db_conf.db_connection -> int (* exit code *) -> unit

  val get_table_from_ref : string -> string option
  val is_valid_ref : string -> bool
  val read_refs : string -> string list
  val read_field_where : Db_cache_types.where_record -> string list
  val read_set_ref : Db_cache_types.where_record -> string list
  val create_row : Context.t -> string -> (string*string) list -> string -> unit
  val delete_row : Context.t -> string -> string -> unit
  val write_field : Context.t -> string -> string -> string -> string -> unit
  val read_field : Context.t -> string -> string -> string -> string
  val find_refs_with_filter : string -> Db_filter_types.expr -> string list

  val process_structured_field : Context.t -> (string*string) -> string -> string -> string -> Db_cache_types.structured_op_t -> unit
  val apply_delta_to_cache : Redo_log.t -> unit

  type db_record = (string * string) list * (string * (string list)) list (* dictionary of regular fields x dictionary of associated set_ref values *)
  val read_record : string -> string -> db_record
  val read_records_where : string -> Db_filter_types.expr -> (string * db_record) list
end

(** The dbcache that switches between master/slave accordingly *)
module DBCache : DB_CACHE =
struct
  type db_record = (string * string) list * (string * (string list)) list
      (* ---------------------------------------------------------------------------------------------------
	 Pool master implementation of DB_CACHE
	 --------------------------------------------------------------------------------------------------- *)

  (** An in-memory cache, used by pool master [not exposed beyond internals of DBCache] *)
  module Master : DB_CACHE =
  struct
    type db_record = (string * string) list * (string * (string list)) list
    module D = Debug.Debugger(struct let name = "sql" end)
    open D
    module W = Debug.Debugger(struct let name = "db_write" end)
    module R = Debug.Debugger(struct let name = "redo_log" end)
    
    open Db_cache_types
    open Db_action_helper
    open Db_backend

    let display_sql_writelog b = display_sql_writelog_val := b
		
    let redo_log_context_name = "redo_log"

    (* This fn is part of external interface, so need to take lock *)
    let get_table_from_ref objref =
      with_lock
	(fun () ->
	   try
	     Some (Hashtbl.find ref_table_map objref)
	   with Not_found -> None)

    let is_valid_ref objref =
      match (get_table_from_ref objref) with
	Some _ -> true
      | None -> false
	
    (** Return an association list of table name * record count *)
    let stats () = 
      with_lock 
	(fun () ->
	  fold_over_tables (fun name tbl acc ->
	    let size = fold_over_rows (fun _ _ acc -> acc + 1) tbl 0 in
	    (name, size) :: acc) cache [])

    let flush_dirty dbconn = Db_connections.flush_dirty_and_maybe_exit dbconn None
    let flush_and_exit dbconn ret_code = ignore (Db_connections.flush_dirty_and_maybe_exit dbconn (Some ret_code))
	
    (* Read field from cache *)
    let read_field context tblname fldname objref =
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
        Redo_log.write_delta (Generation.read_generation()) entry
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
    let write_field context tblname objref fldname newval =
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
		   Generation.increment();
		   save_in_redo_log context (Redo_log.WriteField(tblname, objref, fldname, newval))
		 end;

		 let events_old_val =
		   if is_valid_ref current_val then begin
		     List.iter (fun (tbl, fld) ->
		       let row = lookup_row_in_table (lookup_table_in_cache Db_backend.cache tbl) tbl current_val in
		       bump_event_number_in_row row) other_tbl_refs_for_this_field;
		     Eventgen.events_of_other_tbl_refs
		       (List.map (fun (tbl,fld) ->
		         (tbl, current_val, Eventgen.find_get_record tbl ~__context:context ~self:current_val)) other_tbl_refs_for_this_field);
		   end
		   else []
		in
		 
		 let events_new_val =
		   if is_valid_ref newval then begin
		     List.iter (fun (tbl, fld) -> 
		       let row = lookup_row_in_table (lookup_table_in_cache Db_backend.cache tbl) tbl newval in
		       bump_event_number_in_row row) other_tbl_refs_for_this_field;
		     Eventgen.events_of_other_tbl_refs
		       (List.map (fun (tbl,fld) ->
		         (tbl, newval, Eventgen.find_get_record tbl ~__context:context ~self:newval)) other_tbl_refs_for_this_field);
		   end
		   else [] 
		 in
		 
		 (* Generate event *)
		 let snapshot = Eventgen.find_get_record tblname ~__context:context ~self:objref in
		 let record = snapshot() in
		 List.iter (fun (tbl, ref, s) -> events_notify ~snapshot:s tbl "mod" ref) events_old_val;
		 events_notify ~snapshot:record tblname "mod" objref;
		 List.iter (fun (tbl, ref, s) -> events_notify ~snapshot:s tbl "mod" ref) events_new_val;
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
		 (Gen_schema.sql_of_obj obj.Datamodel_types.name, (* local classname *)
		  Gen_schema.sql_of_id full_name, (* local field *)
		  Gen_schema.sql_of_obj obj', (* remote classname *)
		  fld' (* remote fieldname *))::(set_refs fs)
	     | _::fs -> set_refs fs in
	   
	   let setrefs = set_refs obj_fields in

	   let sr_fields =
	     List.map (fun (_,local_fieldname,remote_classname,remote_fieldname)->
			 (local_fieldname,
			  get_set_ref remote_classname remote_fieldname objref)) setrefs in
	   (fvlist, sr_fields))

    (* Delete row from tbl *)
    let delete_row context tblname objref =

      (* NB we generate the delete event BEFORE deleting the object 
	 but then generate the mod events afterwards *)
      let generate_delete_event () = 
	let snapshot = Eventgen.find_get_record tblname ~__context:context ~self:objref () in
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
		      then begin
		        bump_event_number_in_row (lookup_row_in_table (lookup_table_in_cache cache remote_tbl) remote_tbl fld_value);
		        (remote_tbl, fld_value, Eventgen.find_get_record remote_tbl ~__context:context ~self:fld_value) :: accu
		      end
		      else accu) 
          [] other_tbl_refs in
	fun () ->
	  let other_tbl_ref_events = Eventgen.events_of_other_tbl_refs other_tbl_refs in
	  List.iter (fun (tbl, ref, s) -> 
		       events_notify ~snapshot:s tbl "mod" ref) other_tbl_ref_events in	
      with_lock
	(fun () ->
	   W.debug "delete_row %s (%s)" tblname objref;
	   (* send event *)
	   generate_delete_event();
	   let mod_events = lazily_generate_mod_events () in

	   invalidate_indexes tblname;
	   let tbl = lookup_table_in_cache cache tblname in

	   remove_row_from_table tbl objref;
	   
	   (* Notify each db connection of delete *)
	   List.iter (fun dbconn->Db_connections.notify_delete dbconn tblname objref) (Db_conn_store.read_db_connections());

	   if (this_table_persists tblname) then
	     begin
	       (* Update cache dirty status *)
	       Db_dirty.clear_all_row_dirty_status objref;
	       Db_dirty.set_all_dirty_table_status tblname;
	       Generation.increment();
	       save_in_redo_log context (Redo_log.DeleteRow(tblname, objref))
	     end;
	   Ref_index.remove objref;
	   remove_ref_from_table_map objref;
	   (* send the rest of the events *)
	   mod_events ())
	
    (* Create new row in tbl containing specified k-v pairs *)
    let create_row context tblname kvs new_objref =

      (* Ensure values are valid for UTF-8-encoded XML. *)
      let kvs = List.map (fun (key, value) -> (key, ensure_utf8_xml value)) kvs in

      (* fill in default values specifed in datamodel if kv pairs for these are not supplied already *)
      let kvs = add_default_kvs kvs tblname in

      let generate_create_event() =
	let snapshot = Eventgen.find_get_record tblname ~__context:context ~self:new_objref in
	let other_tbl_refs = Eventgen.follow_references tblname in
	let other_tbl_refs =
	  List.fold_left (fun accu (tbl,fld) ->
		      let fld_value = List.assoc fld kvs in
		      if is_valid_ref fld_value 
		      then begin
		        bump_event_number_in_row (lookup_row_in_table (lookup_table_in_cache cache tbl) tbl fld_value);
		        (tbl, fld_value, Eventgen.find_get_record tbl ~__context:context ~self:fld_value) :: accu
		      end
		      else accu) 
		      [] other_tbl_refs in
	let record = snapshot() in
	let other_tbl_events = Eventgen.events_of_other_tbl_refs other_tbl_refs in
	events_notify ~snapshot:(snapshot ()) tblname "add" new_objref;
	List.iter (fun (tbl, ref, s) -> events_notify ~snapshot:s tbl "mod" ref) other_tbl_events in

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
	       Generation.increment();
	       save_in_redo_log context (Redo_log.CreateRow(tblname, new_objref, kvs))
	     end;
	   add_ref_to_table_map new_objref tblname (* track ref against this table *);
	   let uuid = lookup_field_in_row newrow uuid_fname in
	   let name_label = try Some (lookup_field_in_row newrow name_label_fname) with _ -> None in
	   Ref_index.insert {Ref_index.name_label = name_label; Ref_index.uuid = uuid; Ref_index._ref = new_objref };
	   
	   (* generate events *)
	   generate_create_event();
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

    (* Read references from tbl *)
    let read_refs tblname =
      with_lock
	(fun () ->
	   get_reflist (lookup_table_in_cache cache tblname))

    let populate_from connection_spec =
      Db_connections.populate connection_spec db_table_names;
      Db_backend.blow_away_non_persistent_fields()

    let sync_all_db_connections() =
      (* if any of the connections have an on-disk generation count <> the gen count of the db we populated from
	 then we need to force_flush them to "pull them to tip" *)
      List.iter 
	(fun (gen,db)->
	   let current_gen = Generation.read_generation() in
	   if gen<>current_gen then
	     begin
	       debug "Database '%s' out of sync; syncing up now." db.Parse_db_conf.path;
	       Db_connections.force_flush_all db
	     end)
	(Db_connections.get_dbs_and_gen_counts())

    (* Executed on the master to post-process database after populating cache from db stored on disk *)
    let post_populate_hook () =
      (* Remove the temporary file used for staging from the metadata LUN --
       * there's no need to keep it and it's preferable for it not to hang
       * around. *)
      Unixext.unlink_safe Xapi_globs.ha_metadata_db;
      Db_backend.blow_away_non_persistent_fields();
      (* Flush the in-memory cache to the redo-log *)
      Backend_xml.flush_db_to_redo_log Db_backend.cache

    let populate_cache () =
      let connections = Db_conn_store.read_db_connections () in
      
      (* Include a fake connection representing the HA metadata db
	  (if available). This isn't a full flushing connection per-se but
	  is only considered as a population source. *)
      let fake_ha_dbconn = { Parse_db_conf.dummy_conf with
        Parse_db_conf.path = Xapi_globs.ha_metadata_db;
        format = Parse_db_conf.Xml } in
      let connections = 
		if Sys.file_exists Xapi_globs.ha_metadata_db
		then fake_ha_dbconn :: connections else connections in

      let fake_gen_dbconn = { Parse_db_conf.dummy_conf with
        Parse_db_conf.path = Xapi_globs.gen_metadata_db;
        format = Parse_db_conf.Xml } in
      let connections = 
		if Sys.file_exists Xapi_globs.gen_metadata_db
		then fake_gen_dbconn :: connections else connections in
	
      Db_connections.init_gen_count connections;
      (* If we have a temporary_restore_path (backup uploaded in previous run of xapi process) then restore from that *)
      if Sys.file_exists Xapi_globs.db_temporary_restore_path then
		begin
		  (* we know that the backup is XML format so, to get the manifest, we jump right in and use the xml backend directly here.. *)
		  let manifest = Backend_xml.populate_and_read_manifest Parse_db_conf.backup_file_dbconn in
		  Db_backend.post_restore_hook manifest;
		  (* non-persistent fields will have been flushed to disk anyway [since non-persistent just means dont trigger a flush
		     if I change]. Hence we blank non-persistent fields with a suitable empty value, depending on their type *);
		  post_populate_hook();
		  debug "removed non-persistent fields after backup";
		  (* since we just restored from backup then flush new cache to all db connections *)
		  List.iter 
		    (fun (_,db)->
		       debug "Database '%s' syncing up after restore from backup." db.Parse_db_conf.path;
		       Db_connections.force_flush_all db
		    )
		    (Db_connections.get_dbs_and_gen_counts());
		  (* delete file that contained backup *)
		  Db_backend.try_and_delete_db_file Xapi_globs.db_temporary_restore_path;
		end
      else (* if there's no backup to restore from then.. *)
		begin
		  (* Check schema vsn is current; if not try and upgrade; if can't do that then fail startup.. *)
		  let most_recent_db = Db_connections.pick_most_recent_db connections in
		  debug "Path that I'm looking at to consider whether to upgrade = %s" most_recent_db.Parse_db_conf.path;
		  if Sys.file_exists most_recent_db.Parse_db_conf.path then
		    Db_upgrade.maybe_upgrade most_recent_db;
	
		  (* populate from most recent database *)
		  Db_connections.populate most_recent_db db_table_names;
		  post_populate_hook ()
		end

    let spawn_db_flush_threads() =
      (* Spawn threads that flush cache to db connections at regular intervals *)
      List.iter
	(fun dbconn ->
	   ignore (Thread.create 
		     (fun ()->
			Db_connections.inc_db_flush_thread_refcount();
			let db_path = dbconn.Parse_db_conf.path in
			name_thread ("dbflush ["^db_path^"]");
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
			      Thread.delay db_FLUSH_TIMER;
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
    let initialise_db_cache() =
      populate_cache();
      sync_all_db_connections();
      spawn_db_flush_threads()

    (* entry point for xapi-db-process; initialises a db cache without syncing all db connections "to tip" *)
    let initialise_db_cache_nosync() =
      populate_cache();
      spawn_db_flush_threads()

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
	
    let process_structured_field context (key,value) tbl fld objref proc_fn_selector =

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
	   write_field context tbl objref fld processed_str)
	    
    let dump_db_cache db_cache_manifest fd =
      let time = Unix.gettimeofday() in
      (* Snapshot the cache (uses the lock) and then slowly serialise the copy *)
      Db_xml.To.fd fd (db_cache_manifest, snapshot cache);
      debug "Written xml to fd: (time %f)" (Unix.gettimeofday() -. time)

    let apply_delta_to_cache entry =
      let context = Context.make redo_log_context_name in
      match entry with 
      | Redo_log.CreateRow(tblname, objref, kvs) ->
        R.debug "Redoing create_row %s (%s)" tblname objref;
        create_row context tblname kvs objref
      | Redo_log.DeleteRow(tblname, objref) ->
        R.debug "Redoing delete_row %s (%s)" tblname objref;
        delete_row context tblname objref
      | Redo_log.WriteField(tblname, objref, fldname, newval) ->
        R.debug "Redoing write_field %s (%s) [%s -> %s]" tblname objref fldname newval;
        write_field context tblname objref fldname newval

  end (* of DBCache.Master *)


  (* ---------------------------------------------------------------------------------------------------
     Pool slave implementation of DB_CACHE
     --------------------------------------------------------------------------------------------------- *)

  module Slave : DB_CACHE =
  struct
    open Db_remote_marshall
    type db_record = (string * string) list * (string * (string list)) list
    module D = Debug.Debugger(struct let name = "slave_db_cache" end)
    open D

    let populate_from _ = ()
    let populate_cache() = ()
    let spawn_db_flush_threads() = ()
    
    let initialise_db_cache () =
      ignore (Master_connection.start_master_connection_watchdog());
      ignore (Master_connection.open_secure_connection())

    let initialise_db_cache_nosync () = () (* not used on slave *)

    let flush_dirty _ = false
    let display_sql_writelog b = ()
    let flush_and_exit _ i = exit i

    let stats () = [] 

    exception Remote_db_server_returned_unknown_exception
    (* Process an exception returned from server, throwing local exception *)
    let process_exception_xml xml =
      match XMLRPC.From.array (fun x->x) xml with
	[exn_name_xml; exn_params_xml] ->
	  let exn_name = XMLRPC.From.string exn_name_xml in
	  begin
	    match exn_name with
	    | "dbcache_notfound" ->
		let (x,y,z) = unmarshall_3strings exn_params_xml in
		raise (DBCache_NotFound (x,y,z))
	    | "duplicate_key_of" ->
		let (w,x,y,z) = unmarshall_4strings exn_params_xml in
		raise (Duplicate_key (w,x,y,z))
	    | "uniqueness_constraint_violation" ->
		let (x,y,z) = unmarshall_3strings exn_params_xml in
		raise (Uniqueness_constraint_violation (x,y,z))
	    | _ -> raise DB_remote_marshall_error
	  end
      | _ -> raise Remote_db_server_returned_unknown_exception

    exception Remote_db_server_returned_bad_message
    let do_remote_call marshall_args unmarshall_resp fn_name args =
      let xml = marshall_args args in
      let xml = XMLRPC.To.array [XMLRPC.To.string fn_name; XMLRPC.To.string !Xapi_globs.pool_secret; xml] in
      let resp = Master_connection.execute_remote_fn xml Constants.remote_db_access_uri in
      match XMLRPC.From.array (fun x->x) resp with
	[status_xml; resp_xml] ->
	  let status = XMLRPC.From.string status_xml in
	  if status="success" then unmarshall_resp resp_xml
	  else process_exception_xml resp_xml
      | _ -> raise Remote_db_server_returned_bad_message
	  
    let get_table_from_ref x =
      do_remote_call
	marshall_get_table_from_ref_args
	unmarshall_get_table_from_ref_response
	"get_table_from_ref"
	x

    let is_valid_ref x =
      do_remote_call
	marshall_is_valid_ref_args
	unmarshall_is_valid_ref_response
	"is_valid_ref"
	x

    let read_refs x =
      do_remote_call
	marshall_read_refs_args
	unmarshall_read_refs_response
	"read_refs"
	x

    let read_field_where x =
      do_remote_call
	marshall_read_field_where_args
	unmarshall_read_field_where_response
	"read_field_where"
	x

    let read_set_ref x =
      do_remote_call
	marshall_read_set_ref_args
	unmarshall_read_set_ref_response
	"read_set_ref"
	x


    let create_row _ x y z =
      do_remote_call
	marshall_create_row_args
	unmarshall_create_row_response
	"create_row"
	(x,y,z)

    let delete_row _ x y =
      do_remote_call
	marshall_delete_row_args
	unmarshall_delete_row_response
	"delete_row"
	(x,y)

    let write_field _ a b c d =
      do_remote_call
	marshall_write_field_args
	unmarshall_write_field_response
	"write_field"
	(a,b,c,d)

    let read_field context x y z =
      do_remote_call
	marshall_read_field_args
	unmarshall_read_field_response
	"read_field"
	(x,y,z)

    let find_refs_with_filter s e =
      do_remote_call
	marshall_find_refs_with_filter_args
	unmarshall_find_refs_with_filter_response
	"find_refs_with_filter"
	(s,e)

    let read_record x y =
      do_remote_call
	marshall_read_record_args
	unmarshall_read_record_response
	"read_record"
	(x,y)

    let read_records_where x e =
      do_remote_call
	marshall_read_records_where_args
	unmarshall_read_records_where_response
	"read_records_where"
	(x,e)

    let process_structured_field _ a b c d e =
      do_remote_call
	marshall_process_structured_field_args
	unmarshall_process_structured_field_response
	"process_structured_field"
	(a,b,c,d,e)

    let dump_db_cache _ _ = () (* this is master-only *)

    let apply_delta_to_cache _ = () (* this is master-only *)

  end (* of DBCache.Slave *)


  (* ---------------------------------------------------------------------------------------------------
     Master/slave dispatch table.
     --------------------------------------------------------------------------------------------------- *)
    
  module D = Debug.Debugger(struct let name = "sql" end)
  open D

  let sw master slave = match !database_mode with
    | Some Master -> master
    | Some Slave -> slave
    | None -> raise Must_initialise_database_mode

  let context_to_task_string context = 
    let task,ctxname = Context.get_task_id_string_name context in
    let task_name = match Ref_index.lookup task with
      | Some x -> (match x.Ref_index.name_label with Some y -> Printf.sprintf " (%s)" y | None -> "")
      | None -> ""
    in
    task ^ task_name ^ " (" ^ ctxname ^ ")"

  let populate_from s =
    (sw Master.populate_from Slave.populate_from) s
  let populate_cache() =
    (sw Master.populate_cache Slave.populate_cache) ()
  let spawn_db_flush_threads() =
    (sw Master.spawn_db_flush_threads Slave.spawn_db_flush_threads) ()
  let initialise_db_cache () =
    (sw Master.initialise_db_cache Slave.initialise_db_cache) ()
  let initialise_db_cache_nosync () =
    (sw Master.initialise_db_cache_nosync Slave.initialise_db_cache_nosync) ()
  let display_sql_writelog b =
    (sw Master.display_sql_writelog Slave.display_sql_writelog) b
  let flush_dirty dbconn =
    (sw Master.flush_dirty Slave.flush_dirty) dbconn
  let flush_and_exit dbconn ret_code =
    (sw Master.flush_and_exit Slave.flush_and_exit) dbconn ret_code
  let stats () = 
    (sw Master.stats Slave.stats) ()
  let get_table_from_ref s =
    (sw Master.get_table_from_ref Slave.get_table_from_ref) s
  let is_valid_ref s =
    (sw Master.is_valid_ref Slave.is_valid_ref) s
  let read_refs s =
    (sw Master.read_refs Slave.read_refs) s
  let read_field_where w =
    (sw Master.read_field_where Slave.read_field_where) w
  let read_set_ref w =
    (sw Master.read_set_ref Slave.read_set_ref) w
  let create_row context s1 s2 s3 =
    Stats.log_db_call (Some (context_to_task_string context)) (s1) Stats.Create;
    (sw Master.create_row Slave.create_row) context s1 s2 s3
  let delete_row context s1 s2 =
    Stats.log_db_call (Some (context_to_task_string context)) (s1) Stats.Drop;
    (sw Master.delete_row Slave.delete_row) context s1 s2
  let write_field context s1 s2 s3 s4 =
    Stats.log_db_call (Some (context_to_task_string context)) (s1^"."^s3) Stats.Write;
    (sw Master.write_field Slave.write_field) context s1 s2 s3 s4
  let read_field context s1 s2 s3 =
    Stats.log_db_call (Some (context_to_task_string context)) (s1^"."^s2) Stats.Read;
    (sw Master.read_field Slave.read_field) context s1 s2 s3
  let find_refs_with_filter s e =
    (sw Master.find_refs_with_filter Slave.find_refs_with_filter) s e
  let process_structured_field context s1 s2 s3 s4 op =
    (sw Master.process_structured_field Slave.process_structured_field) context s1 s2 s3 s4 op
  let read_record s1 s2 =
    Stats.log_db_call None ("record:"^s1) Stats.Read;
    (sw Master.read_record Slave.read_record) s1 s2
  let read_records_where s e =
    Stats.log_db_call None ("record(where):"^s) Stats.Read;
    (sw Master.read_records_where Slave.read_records_where) s e
  let dump_db_cache manifest fd =
    (sw Master.dump_db_cache Slave.dump_db_cache) manifest fd
  let apply_delta_to_cache entry =
    (sw Master.apply_delta_to_cache Slave.apply_delta_to_cache) entry

end
  
