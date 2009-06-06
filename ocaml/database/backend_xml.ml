
module D = Debug.Debugger(struct let name = "sql" end)
open D

open Db_backend
open Pervasiveext

(** Given a fresh cache, update ref/uuid/name_label indices *)
let update_index tables =
  Hashtbl.iter
    (fun name table ->
       Hashtbl.iter
	 (fun rf row ->
	    let uuid = lookup_field_in_row row uuid_fname in
	    let name_label = try Some (lookup_field_in_row row name_label_fname) with _ -> None in
	    add_ref_to_table_map rf name;
	    Ref_index.insert {Ref_index.name_label = name_label; Ref_index.uuid = uuid; Ref_index._ref = rf}
	 ) table
    ) tables

let unmarshall dbconn = 
  let filename = dbconn.Parse_db_conf.path in
  if not dbconn.Parse_db_conf.compress
  then Db_xml.From.file filename
  else 
    let compressed = Unix.openfile filename [ Unix.O_RDONLY ] 0o0 in
    finally
      (fun () -> 
	 let result = ref None in
	 Gzip.decompress_passive compressed 
	   (fun uncompressed -> 
	      result := Some (Db_xml.From.channel (Unix.in_channel_of_descr uncompressed))
	   );
	 match !result with
	 | None -> failwith "unmarshal failure"
	 | Some x -> x
      )
      (fun () -> Unix.close compressed)    

(* Given table name, read all rows from db and store in cache *)
let populate_and_read_manifest dbconn =
  debug "attempting to restore database from %s" dbconn.Parse_db_conf.path;
  let manifest, unmarshalled_db = unmarshall dbconn in
  debug "finished parsing xml";
  (* version_check manifest; *) 
  update_index unmarshalled_db;
  Hashtbl.iter (fun name table -> set_table_in_cache name table) unmarshalled_db;
  manifest

let populate dbconn tbls =
  ignore (populate_and_read_manifest dbconn)

let atomically_write_to_db_file filename marshall =
  let tmp_file = Filenameext.temp_file_in_dir filename in
  try
    debug "writing db as xml to file '%s'." filename;
    marshall tmp_file;
    Unix.rename tmp_file filename
  with e -> (debug "Exception writing db xml: %s" (Printexc.to_string e); log_backtrace();
	     Unix.unlink tmp_file; (* make sure we don't leak temp files *)
	     raise e)

(* Write the given database to the redo-log *)
let flush_db_to_redo_log cache_to_flush =
  if Redo_log.is_enabled () then begin
    (* Atomically read the generation count and take a deep copy of the cache *)
    let (gen_count, cache_copy) = Db_lock.with_lock (fun () -> Generation.read_generation(), Db_backend.snapshot cache_to_flush) in
    debug "Flushing cache to redo-log";
    let db_cache_manifest = Db_cache_types.gen_manifest gen_count in
    let write_db_to_fd = (fun out_fd -> Db_xml.To.fd out_fd (db_cache_manifest, cache_copy)) in
    Redo_log.write_db gen_count write_db_to_fd
  end

(* atomically flush entire db cache to disk. If we are given a cache then flush that, otherwise flush the
   current state of the global in-memory cache *)

(* Flush the database to the specified connection. This fn is responsible for ensuring that
   (i) the db lock is held in order to guarantee an atomic snapshot of the database is flushed;
   and (ii) that the db lock is _not_ held whilst writing to remote storage.
   
   For simplicity we enforce these 2 constraints by taking an entire snapshot of the db with the
   global lock held and then flushing this snapshot to disk. However, this process is optimised
   so that we only snapshot when writing to _remote storage_; in the local storage case we hold
   the db_lock whilst writing.
*)
let flush_common dbconn optional_cache =
  let time = Unix.gettimeofday() in

  let do_flush cache_to_flush filename =
    flush_db_to_redo_log cache_to_flush;
    let db_cache_manifest = Db_cache_types.gen_manifest (Generation.read_generation()) in
    if not dbconn.Parse_db_conf.compress
    then Db_xml.To.file filename (db_cache_manifest, cache_to_flush)
    else 
      let compressed = Unix.openfile filename [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o0 in
      finally
	(fun () -> Gzip.compress compressed (fun uncompressed -> Db_xml.To.fd uncompressed (db_cache_manifest, cache_to_flush)))
	(fun () -> Unix.close compressed) in

  let marshall filename =
    match optional_cache with
      (* if we're flushing global db cache then snapshot with db_lock if we're writing to remote storage;
	 otherwise don't snapshot and do entire flush to local storage whilst holding db_lock *)
      None -> 
	if dbconn.Parse_db_conf.is_on_remote_storage then (* writing to remote storage -- can't hold db_lock during flush *)
	  let snapshot_of_global_cache = Db_lock.with_lock (fun () -> snapshot cache) in
	  do_flush snapshot_of_global_cache filename
	else (* writing to local storage -- just hold db_lock and flush directly from global cache (saving memory for snapshot in common case) *)
	  Db_lock.with_lock (fun () -> do_flush cache filename)
	(* if we were given a specific cache to flush then just do it; no locks required *)
    | Some c -> do_flush c filename in
  let filename = dbconn.Parse_db_conf.path in
  atomically_write_to_db_file filename marshall;
  debug "XML backend [%s] -- Write buffer flushed. Time: %f" filename (Unix.gettimeofday() -. time)


(* We don't do any incremental flushing in this backend - we just check if any tables are dirty; if so we
   flush everything and reset dirty status to clean *)

(* Since this fn is not called with the db_lock we have to take this lock explicitly when accessing shared db state
   -- e.g. the global tables that record what's dirty per database connection
*)
let flush_dirty dbconn =
  (* is there anything for me to flush? *)
  let sql_table_names_to_flush =
    Db_lock.with_lock (* paranoia.. can almost certainly get away without taking db lock here.. *)
      (fun () ->
	 List.filter (fun tbl_name->(Hashtbl.find table_persist_options tbl_name)<>Datamodel_types.PersistNothing) db_table_names) in
  let anything_dirty =
    Db_lock.with_lock (* definitely need db lock here becuase we're accessing shared db state *)
      (fun () ->
	 List.fold_left (fun acc x -> acc || x) false (List.map (fun tbl -> Db_dirty.read_my_dirty_table_status dbconn tbl) sql_table_names_to_flush)) in
  let clear_all_dirty_flags () =
    Db_lock.with_lock (* definitely need db lock here because we're accessing shared db state *)
      (fun () ->
	 List.iter
	   (fun tbl_name ->
	      Db_dirty.clear_my_dirty_table_status dbconn tbl_name;
	      let tbl = lookup_table_in_cache tbl_name in
	      let rows = get_rowlist tbl in
	      List.iter
		(fun row ->
		   let objref = lookup_field_in_row row reference_fname in
		   Db_dirty.clear_my_row_dirty_status dbconn objref)
		rows
	   )
	   sql_table_names_to_flush
      ) in
  if anything_dirty then
    begin
      flush_common dbconn None;
      clear_all_dirty_flags()
    end;
  anything_dirty (* return true if we did some flushing, false otherwise *)

let force_flush_all dbconn optional_cache =
  flush_common dbconn optional_cache
  
let read_schema_vsn dbconn =
  (* inefficient to read whole db file just to parse schema vsn; but since this fn is
     only called at startup I don't think we care.. *)
  let manifest, unmarshalled_db = unmarshall dbconn in
  manifest.Db_cache_types.schema_major_vsn, manifest.Db_cache_types.schema_minor_vsn
    
let create_empty_db dbconn =
  let empty_cache = Hashtbl.create 20 in
  let empty_row = Hashtbl.create 1 in
  List.iter (fun tbl -> Hashtbl.replace empty_cache tbl empty_row) db_table_names;
  let manifest = Db_cache_types.gen_manifest 0L (* new gen count *) in
  let marshall filename = Db_lock.with_lock (fun () -> Db_xml.To.file filename (manifest, empty_cache)) in
  atomically_write_to_db_file dbconn.Parse_db_conf.path marshall;
