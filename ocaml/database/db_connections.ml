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
module D = Debug.Make(struct let name = "xapi" end)
module R = Debug.Make(struct let name = "redo_log" end)
open D

let get_dbs_and_gen_counts() =
  List.map (fun conn->(Parse_db_conf.generation_read conn, conn)) (Db_conn_store.read_db_connections())

(** Returns true if the supplied connection actually exists, false otherwise.
    	Note that, although the two files should be present (or absent) together,
    	after upgrade we only have a database. In this case the generation
    	defaults back to 0L *)
let exists connection =
  Sys.file_exists connection.Parse_db_conf.path

(* This returns the most recent of the db connections to populate from. It also initialises the in-memory
   generation count to the largest of the db connections' generation counts *)
let choose connections = match List.filter exists connections with
  | [] -> None
  | (c :: cs) as connections ->
    List.iter (fun c -> debug "Dbconf contains: %s (generation %Ld)" c.Parse_db_conf.path (Parse_db_conf.generation_read c)) connections;
    let gen, most_recent = List.fold_left (fun (g, c) c' ->
        let g' = Parse_db_conf.generation_read c' in
        if g' > g then (g', c') else (g, c))
        (Parse_db_conf.generation_read c, c) cs in
    debug "Most recent db is %s (generation %Ld)" most_recent.Parse_db_conf.path gen;
    Some most_recent

let preferred_write_db =
  match Db_conn_store.read_db_connections() with
  | [] -> None
  | db :: _ -> Some db

(* This is set by signal handlers. It instructs the db thread to call exit after the next flush *)
let exit_on_next_flush = ref false

(* db flushing thread refcount: the last thread out of the door does the exit(0) when flush_on_exit is true *)
open Xapi_stdext_threads
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
  (* We're about to exit. Close the active redo logs. *)
  Redo_log.with_active_redo_logs (Redo_log.shutdown);
  R.debug "Closed all active redo logs."

(* The connection flushing calls each lock the connection they're flushing to.
   The backend flush calls have to do enough locking (i.e. with the db_lock) to ensure that they
   flush a consistent snapshot. Backends must also ensure that they do not hold the global db_lock
   whilst they are writing to non-local storage.
*)
let flush_dirty_and_maybe_exit dbconn ?(fsync=false) exit_spec =
  Db_conn_store.with_db_conn_lock dbconn
    (fun () ->
       (* if we're being told to shutdown by signal handler then flush every connection
          	  - the rationale is that we're not sure which db connections will be available on next restart *)
       if !exit_on_next_flush then
         begin
           let (_: bool) = Backend_xml.flush_dirty dbconn ~fsync in
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

       let was_anything_flushed = Backend_xml.flush_dirty dbconn ~fsync in

       (* exit if we've been told to by caller *)
       begin
         match exit_spec with
           None -> ()
         | (Some ret_code) -> pre_exit_hook(); exit ret_code
       end;
       was_anything_flushed
    )

let flush dbconn db =
  debug "About to flush database: %s" dbconn.Parse_db_conf.path;
  Db_conn_store.with_db_conn_lock dbconn
    (fun () ->
       Backend_xml.flush dbconn db ~fsync:(dbconn.Parse_db_conf.fsync_enabled)
    )



