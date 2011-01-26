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
module D = Debug.Debugger(struct let name = "xapi" end)
module R = Debug.Debugger(struct let name = "redo_log" end)
open D

let get_dbs_and_gen_counts() =
  List.map (fun conn->(Generation.read conn, conn)) (Db_conn_store.read_db_connections())
    
exception No_databases

(* This returns the most recent of the db connections to populate from. It also initialises the in-memory
   generation count to the largest of the db connections' generation counts *)
let pick_most_recent_db = function
| [] -> raise No_databases
| (c :: cs) as connections ->
	List.iter (fun c -> debug "Dbconf contains: %s (generation %Ld)" c.Parse_db_conf.path (Generation.read c)) connections;
	let gen, most_recent = List.fold_left (fun (g, c) c' -> 
		let g' = Generation.read c' in 
		if g' > g then (g', c') else (g, c)) 
		(Generation.read c, c) cs in
	debug "Most recent db is %s (generation %Ld)" most_recent.Parse_db_conf.path gen;
	most_recent

let preferred_write_db () =
  List.hd (Db_conn_store.read_db_connections()) (* !!! FIX ME *)  

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
       (* if we're being told to shutdown by signal handler then flush every connection
	  - the rationale is that we're not sure which db connections will be available on next restart *)
       if !exit_on_next_flush then
	 begin
	   Backend_xml.flush_dirty dbconn;
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
       
       let was_anything_flushed = Backend_xml.flush_dirty dbconn in
       
       (* exit if we've been told to by caller *)
       begin
	 match exit_spec with
	   None -> ()
	 | (Some ret_code) -> pre_exit_hook(); exit ret_code
       end;
	   was_anything_flushed
    )
(*
let create_empty_db (major, minor) dbconn =
  Generation.create_fresh dbconn;
  Backend_xml.create_empty_db (major, minor) dbconn
  *)
let maybe_create_new_db (major,minor) dbconn =
	if not (Sys.file_exists dbconn.Parse_db_conf.path) 
	then Backend_xml.create_empty_db (major,minor) dbconn

let force_flush_all dbconn =
	debug "About to flush database: %s" dbconn.Parse_db_conf.path;
	Db_conn_store.with_db_conn_lock dbconn
		(fun () ->
			Backend_xml.force_flush_all dbconn None
		)

let force_flush_specified_cache dbconn cache =
	Db_conn_store.with_db_conn_lock dbconn
		(fun () ->
			Backend_xml.force_flush_all dbconn (Some cache)
		)

