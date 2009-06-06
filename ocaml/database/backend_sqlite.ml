
module D = Debug.Debugger(struct let name = "sql" end)
open D

open Db_backend

(* queries are built up ready to be written to the persistent database *)
type query = string * (string list)   (* query string with "holes" and parameters *)

(* Record batched queries that, when executed, will delete objects from persistent database *)
(* the hashtbl key is the db path; when a delete happens we add the pending delete to all db
   connections. On flush, we clear the deletes pending for that particular connection *)
let deletes_pending_table : (string, query list) Hashtbl.t = Hashtbl.create 20

(* NOTE: Notify delete is called whilst holding the db lock. Hence we can access global deletes_pending_table without
   requiring further locking calls *)
let notify_delete dbconn tblname objref =
  let persistent_sql_table_names =
    List.filter (fun tbl_name->(Hashtbl.find table_persist_options tbl_name)<>Datamodel_types.PersistNothing) db_table_names in
  (* we only add something to our persistent delete log if we're deleting from a table that has its persistent flag set *)
  if List.mem tblname persistent_sql_table_names then
    let row_dirty_status = Db_dirty.read_my_row_dirty_status dbconn objref in
    (* if row_dirty_status is New then the object doesn't exist in the
       persistent db yet, so don't add it to deletes_pending *)
    if row_dirty_status <> Db_dirty.New then
      begin
	let my_path = dbconn.Parse_db_conf.path in  
	let current_deletes = try Hashtbl.find deletes_pending_table my_path with _ -> [] in
	Hashtbl.replace deletes_pending_table my_path ((Db_action_helper.deleterow_query tblname objref)::current_deletes)
      end

let populate dbconn tbl_names =
  (* Given table name, read all rows from db and store in cache *)
  let populate_tbl dbconn tbl =
    debug "Populating cache: %s" tbl;
    let sql = Printf.sprintf "SELECT * FROM %s" tbl in
    let res = Sql.open_database_with_filename dbconn.Parse_db_conf.path (fun db -> Sql.do_select sql db) in
    let newtbl = Hashtbl.create 20 in
    let addrow r = Hashtbl.replace newtbl (Hashtbl.find r reference_fname) r in
    List.iter addrow res;
    Hashtbl.replace cache tbl newtbl;
    (* And track the references in the rows as belonging to this table: *)
    List.iter
      (fun row->
	 let objref = lookup_field_in_row row reference_fname in
	 let uuid = lookup_field_in_row row uuid_fname in
	 let name_label = try Some (lookup_field_in_row row name_label_fname) with _ -> None in
	 add_ref_to_table_map objref tbl;
	 Ref_index.insert {Ref_index.name_label = name_label; Ref_index.uuid = uuid; Ref_index._ref = objref}
      ) res in
  List.iter (populate_tbl dbconn) tbl_names

(* Some util fns used by various flushing fns *)
let sql_create_from_row tblname row =
  let fvlist = Hashtbl.fold (fun k d env -> (k,d)::env) row [] in
  Sql.insert_query tblname fvlist
let sql_update_from_row tblname objref row =
  (Db_action_helper.deleterow_query tblname objref)::(sql_create_from_row tblname row)::[]

(* Flush the database to the specified connection. This fn is responsible for ensuring that
   (i) the db lock is held in order to guarantee an atomic snapshot of the database is flushed;
   and (ii) that the db lock is _not_ held whilst writing to remote storage.
   In this case (ii) is easy since the db is scanned whilst writing to an in-memory write-log.
   So we just hold the db_lock whilst generating the write-log and then serialise that to
   storage without holding the db_lock..
*)
let flush_dirty dbconn =
  let sql_write_log:(query list ref) = ref [] in
  let scan_rows tblname =
    let tbl = lookup_table_in_cache tblname in
    let rows = get_rowlist tbl in
    let rec do_scan rows =
      match rows with
	[] -> []
      | (r::rs) ->
	  let objref = lookup_field_in_row r reference_fname in
	  let dirty_status = Db_dirty.read_my_row_dirty_status dbconn objref in
	  begin
	    begin
	      match dirty_status with
		Db_dirty.New ->
		  sql_write_log := (sql_create_from_row tblname r)::!sql_write_log
	      | Db_dirty.Modified ->
		  sql_write_log := (sql_update_from_row tblname objref r) @ !sql_write_log
	      | _ -> ()
	    end;
	    Db_dirty.clear_my_row_dirty_status dbconn objref;
	    do_scan rs
	  end in
    do_scan rows in
  let rec scan_tables tbls =
    match tbls with
      [] -> ()
    | (t::ts) ->
	if Db_dirty.read_my_dirty_table_status dbconn t then
	  (scan_rows t;
	   Db_dirty.clear_my_dirty_table_status dbconn t);
	scan_tables ts in      

  let time=Unix.gettimeofday() in
  (* take db lock and atomically: (i) snapshot deletes pending; (ii) snapshot cache changes/clear dirty fields; (iii) clear deletes pending *)
  let deletes_pending =
    Db_lock.with_lock
      (fun ()->
	 (* snapshot deletes_pending *)
	 let deletes_pending = (try Hashtbl.find deletes_pending_table dbconn.Parse_db_conf.path with _ -> []) in
	 let sql_table_names_to_flush = List.filter (fun tbl_name->(Hashtbl.find table_persist_options tbl_name)<>Datamodel_types.PersistNothing) db_table_names in
	 (* build up local write-log from cache and clear dirty bits *)
	 scan_tables sql_table_names_to_flush;
	 (* clear deletes pending log for this connection *)
	 Hashtbl.replace deletes_pending_table dbconn.Parse_db_conf.path [];
	 (* return snapshot of deletes_pending *)
	 deletes_pending
      ) in
  let list_not_empty l = match l with [] -> false | _ -> true in
  let debug_string() =
    "\n"^
      (String.concat ";\n" (List.map (fun (s,ss)->"("^s^":"^(String.concat "," ss)^")") (deletes_pending @ !sql_write_log))) in
  
  let anything_to_flush = (list_not_empty deletes_pending) || (list_not_empty !sql_write_log) in
  (* only flush to database backend if we have anything to write... *)
  if anything_to_flush then
    begin
      Sql.open_database_with_filename dbconn.Parse_db_conf.path
	(
	  fun db ->
	    Sql.begin_transaction db;
	    List.iter (fun (query,params)->Sql.run_query query params db) deletes_pending;
	    List.iter (fun (query,params)->Sql.run_query query params db) !sql_write_log;
	    Sql.end_transaction db
	);
      debug "Flushing write buffer: %s"
	(if !display_sql_writelog_val then debug_string() else "");
      debug "SQL backend [%s] -- Write buffer flushed. Time: %f" dbconn.Parse_db_conf.path (Unix.gettimeofday() -. time);
    end;
  anything_to_flush (* return true if we did some flushing, false otherwise *)

(* If we get given a dbcache we use that; otherwise we use the global in-memory cache *)
let force_flush_all dbconn optional_cache =
  let filename = dbconn.Parse_db_conf.path in
  (* is there anything for me to flush? *)
  let sql_table_names_to_flush =
    Db_lock.with_lock (* paranoia because we're accessing shared db state, but I think we could do without this lock here since
			 we only write to the table_persist_options at startup time.. *)
      (fun () ->
	 List.filter (fun tbl_name->(Hashtbl.find table_persist_options tbl_name)<>Datamodel_types.PersistNothing) db_table_names) in
  let add_table_to_write_log cache_to_use write_log_sofar tblname =
    let tbl = Hashtbl.find cache_to_use tblname in
    let rows = get_rowlist tbl in
    match rows with
      [] -> ([], None)::write_log_sofar
    | r::_ ->
	(* this gives us an insert query sql with the holes in the same order as fields are ordered in field_list *)
	let fvlist = Hashtbl.fold (fun k d env -> (k,d)::env) r [] in
	let field_list = List.map fst fvlist in
	let sql,_ = Sql.insert_query tblname fvlist in
	(List.fold_left (fun acc r -> 
			   let params = List.map (fun field_name->lookup_field_in_row r field_name) field_list in
			   params::acc) [] rows,
	 Some sql)::write_log_sofar in
  let debug_string() = "\n" in
      (*^(String.concat ";\n" (List.fold (fun (s,ss)->"("^s^":"^(String.concat "," ss)^")") (!sql_write_log))) *)
  let db_tmp_file = Filenameext.temp_file_in_dir filename in

  let write_log = 
    match optional_cache with
	(* if we're scanning global db cache then do this with lock -- note that this is just scanning to create a
	   write-log, so we're OK to hold the global db lock here.. The act of flushing (which must not hold db_lock
	 if we're writing to remote storage is done lower down in this fn.
      *)
      None -> Db_lock.with_lock (fun () -> List.fold_left (add_table_to_write_log cache) [] sql_table_names_to_flush)
	(* if we've been given a local cache to flush then just scan it without the global db lock *)
    | Some c -> List.fold_left (add_table_to_write_log c) [] sql_table_names_to_flush
  in
  
  let time=Unix.gettimeofday() in
  try
    debug "Doing complete flush of db-cache: %s"
      (if !display_sql_writelog_val then debug_string() else "");
    let write_table db (rows,sql) =
      match sql with 
	| Some sql ->
	    let stmt = Sqlsqlite3.do_prepare sql db in
	    let iterfn row =
	      Sqlite3.reset stmt;
	      ignore(Sql.do_exec_prepared (fun _ _ -> ()) stmt row)
	    in
	    List.iter iterfn rows;
	    Sqlsqlite3.do_finalize stmt
	| None -> ()
    in
    Sql.open_database_with_filename db_tmp_file
      (
	fun db ->
	  Sql.begin_transaction db;
	  Sqlite3.exec db (Sql.read_schema()) (fun _ _ -> ());
	  List.iter (write_table db) write_log;
	  Sql.end_transaction db
      );
    Unix.rename db_tmp_file filename;
    debug "SQL backend [%s] -- Entire cache flushed. Time: %f" filename (Unix.gettimeofday() -. time);
  with e ->
    begin
      Unix.unlink db_tmp_file;
      raise e
    end

let read_schema_vsn dbconn =
  let sql = Printf.sprintf "SELECT * FROM %s" Gen_schema.version_table in
  let res = Sql.open_database_with_filename dbconn.Parse_db_conf.path (fun db -> Sql.do_select sql db) in
  match res with
    [row] ->
      let major_vsn = int_of_string (Hashtbl.find row "major") in
      let minor_vsn = int_of_string (Hashtbl.find row "minor") in
      debug "Read schema vsn from db: %d.%d" major_vsn minor_vsn;
      major_vsn,minor_vsn
  | [] ->
      raise Db_action_helper.Cannot_read_schema_version

let create_empty_db dbconn =
  let path = dbconn.Parse_db_conf.path in
  let dirname = Filename.dirname path in
  Unixext.mkdir_rec dirname 0o755;
  Sql.create_empty_db dbconn.Parse_db_conf.path
