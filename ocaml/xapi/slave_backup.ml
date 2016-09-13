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
(**
 * @group Pool Management
*)

(** Immediately fetch a database backup from the master. If a flush_spec is given, with a list of db connections,
    then the backup is flushed to those connections; if no flush spec is given then the backup is flushed to all
    db conections.
*)
type write_entry = {period_start_time: float; writes_this_period: int}
let backup_write_table : (Parse_db_conf.db_connection, write_entry) Hashtbl.t = Hashtbl.create 20
let backup_m = Mutex.create()
let with_backup_lock f = Stdext.Threadext.Mutex.execute backup_m f

(* lookup a write entry, returning one if there's one there. If there isn't one there then create a new one,
   log it in table and return that *)
(* IMPORTANT: must be holding backup_m mutex when you call this function.. *)
let lookup_write_entry dbconn =
  try
    Hashtbl.find backup_write_table dbconn
  with _ ->
    begin
      let new_write_entry = {period_start_time=Unix.gettimeofday(); writes_this_period = 0} in
      Hashtbl.replace backup_write_table dbconn new_write_entry;
      new_write_entry
    end

(* Reset period_start_time, writes_this_period if period has expired *)
let tick_backup_write_table() =
  with_backup_lock
    (fun () ->
       Hashtbl.iter
         (fun dbconn write_entry ->
            match dbconn.Parse_db_conf.mode with
              Parse_db_conf.Write_limit ->
              if (int_of_float (Unix.gettimeofday() -. write_entry.period_start_time)) > dbconn.Parse_db_conf.write_limit_period then
                Hashtbl.replace backup_write_table dbconn {period_start_time=Unix.gettimeofday(); writes_this_period=0}
            | _ -> ()
         )
         backup_write_table)

(* Can we write to specified connection *)
let can_we_write dbconn =
  with_backup_lock
    (fun () ->
       match dbconn.Parse_db_conf.mode with
         Parse_db_conf.No_limit -> true
       | Parse_db_conf.Write_limit ->
         let write_entry = lookup_write_entry dbconn in
         (* we can write if we haven't used up all our write-cycles for this period: *)
         (write_entry.writes_this_period < dbconn.Parse_db_conf.write_limit_write_cycles)
    )

(* Update writes_this_period for dbconn *)
let notify_write dbconn =
  with_backup_lock
    (fun () ->
       let write_entry = lookup_write_entry dbconn in (* will create fresh one if reqd *)
       Hashtbl.replace backup_write_table dbconn {write_entry with writes_this_period = write_entry.writes_this_period + 1})

(* Read connections and gen counts and find if any are behind. If they are behind then figure out if we're allowed to
   write to them. Return list of connections that satisfy both these properties *)
let determine_backup_connections generation_count =
  tick_backup_write_table(); (* reset existing write_entries if any periods expire *)
  let dbconns_and_gen_counts = Db_connections.get_dbs_and_gen_counts() in
  (* throw out dbconns that are up-to-date *)
  let dbconns_and_gen_counts = List.filter (fun (gen,_) -> gen<>generation_count) dbconns_and_gen_counts in
  let dbconns = List.map snd dbconns_and_gen_counts in
  (* throw out dbconns that we can't write to *)
  List.filter can_we_write dbconns
