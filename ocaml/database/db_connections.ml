module D = Debug.Debugger(struct let name = "sql" end)
module R = Debug.Debugger(struct let name = "redo_log" end)
open D

let get_dbs_and_gen_counts() =
  List.map (fun conn->(Generation.read conn, conn)) (Db_conn_store.read_db_connections())
    
let init_gen_count connections =
  let gens = List.map Generation.read connections in
  let max = List.fold_left (fun max x -> if x>max then x else max) 0L gens in
  Generation.set_count max

(* This returns the most recent of the db connections to populate from. It also initialises the in-memory
   generation count to the largest of the db connections' generation counts *)
let pick_most_recent_db connections =
  let conns_and_gens = List.map (fun conn -> Generation.read conn, conn) connections in
  let _ = List.iter (fun (g,conn)->debug "Dbconf contains: %Ld, %s" g conn.Parse_db_conf.path) conns_and_gens in
  let conn = ref Parse_db_conf.dummy_conf in
  let rec pick_largest l sofar =
    match l,sofar with
      [], g -> !conn
    | (this_g, this_conn)::cs, largest_g ->
	if this_g > largest_g then
	  begin
	    conn := this_conn;
	    pick_largest cs this_g
	  end
	else pick_largest cs largest_g in
  let most_recent = pick_largest conns_and_gens (Int64.sub 0L 1L) in
  debug "Most recent db in db.conf file is '%s'" most_recent.Parse_db_conf.path;
  most_recent

let preferred_write_db () =
  List.hd (Db_conn_store.read_db_connections()) (* !!! FIX ME *)  

(* ------------------- Fns that dispatch to generic backends *)

let notify_delete dbconn tblname objref =
  match dbconn.Parse_db_conf.format with
    Parse_db_conf.Sqlite -> Backend_sqlite.notify_delete dbconn tblname objref
  | _ -> ()

let read_schema_vsn dbconn =
  match dbconn.Parse_db_conf.format with
    Parse_db_conf.Sqlite -> Backend_sqlite.read_schema_vsn dbconn
  | Parse_db_conf.Xml -> Backend_xml.read_schema_vsn dbconn    
  
let populate dbconn table_names =
  match dbconn.Parse_db_conf.format with
    Parse_db_conf.Sqlite -> Backend_sqlite.populate dbconn table_names
  | Parse_db_conf.Xml -> Backend_xml.populate dbconn table_names

(* This is set by signal handlers. It instructs the db thread to call exit after the next flush *)
let exit_on_next_flush = ref false


(* db flushing thread refcount: the last thread out of the door does the exit(0) when flush_on_exit is true *)
let db_flush_thread_refcount_m = Mutex.create()
let db_flush_thread_refcount = ref 0
let inc_db_flush_thread_refcount() =
  Threadext.Mutex.execute db_flush_thread_refcount_m
    (fun () -> db_flush_thread_refcount := !db_flush_thread_refcount + 1)
let dec_and_read_db_flush_thread_refcount() =
  Threadext.Mutex.execute db_flush_thread_refcount_m
    (fun () ->
       db_flush_thread_refcount := !db_flush_thread_refcount - 1;
       !db_flush_thread_refcount
    )

let pre_exit_hook () =
  if Redo_log.is_enabled() then begin
    (* We're about to exit. Close the redo log. *)
    Redo_log.shutdown();
    R.debug "Closed redo log"
  end

(* The connection flushing calls each lock the connection they're flushing to.
   The backend flush calls have to do enough locking (i.e. with the db_lock) to ensure that they
   flush a consistent snapshot. Backends must also ensure that they do not hold the global db_lock
   whilst they are writing to non-local storage.
*)
let flush_dirty_and_maybe_exit dbconn exit_spec =
  Db_conn_store.with_db_conn_lock dbconn
    (fun () ->
       let flush_conn dbconn =
	 let was_anything_flushed =
	     match dbconn.Parse_db_conf.format with
	     Parse_db_conf.Sqlite -> Backend_sqlite.flush_dirty dbconn
	   | Parse_db_conf.Xml -> Backend_xml.flush_dirty dbconn in
	 if was_anything_flushed then
	   Generation.write_out dbconn;
     was_anything_flushed in

       (* if we're being told to shutdown by signal handler then flush every connection
	  - the rationale is that we're not sure which db connections will be available on next restart *)
       if !exit_on_next_flush then
	 begin
	   flush_conn dbconn;
	   let refcount = dec_and_read_db_flush_thread_refcount() in
	   (* last flushing thread close the door on the way out.. *)
	   if refcount = 0 then
	     begin
	       debug "refcount is 0; exiting";
	       pre_exit_hook();
	       exit 0
	     end
	   else
	     debug "refcount is %d; not exiting" refcount
	 end;
       
       let was_anything_flushed = flush_conn dbconn in
       
       (* exit if we've been told to by caller *)
       begin
	 match exit_spec with
	   None -> ()
	 | (Some ret_code) -> pre_exit_hook(); exit ret_code
       end;
       was_anything_flushed
    )

let create_empty_db dbconn =
  Generation.create_fresh dbconn;
  match dbconn.Parse_db_conf.format with
    Parse_db_conf.Sqlite -> Backend_sqlite.create_empty_db dbconn
  | Parse_db_conf.Xml -> Backend_xml.create_empty_db dbconn
  
let maybe_create_new_db dbconn =
  if not (Sys.file_exists dbconn.Parse_db_conf.path) then
    begin
      Generation.create_fresh dbconn;
      match dbconn.Parse_db_conf.format with
	Parse_db_conf.Sqlite -> Backend_sqlite.create_empty_db dbconn
      | Parse_db_conf.Xml -> Backend_xml.create_empty_db dbconn
    end

let force_flush_all dbconn =
  Db_conn_store.with_db_conn_lock dbconn
    (fun () ->
       begin
	 match dbconn.Parse_db_conf.format with
	   Parse_db_conf.Sqlite -> Backend_sqlite.force_flush_all dbconn None
	 | Parse_db_conf.Xml -> Backend_xml.force_flush_all dbconn None
       end;
       Generation.write_out dbconn
    )

let force_flush_specified_cache dbconn generation_count cache =
  Db_conn_store.with_db_conn_lock dbconn
    (fun () ->
       begin
	 match dbconn.Parse_db_conf.format with
	   Parse_db_conf.Sqlite -> Backend_sqlite.force_flush_all dbconn (Some cache)
	 | Parse_db_conf.Xml -> Backend_xml.force_flush_all dbconn (Some cache)
       end;
       Generation.write_out_specified_count dbconn generation_count
    )
