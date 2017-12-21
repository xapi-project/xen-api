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

module D = Debug.Make(struct let name = "sql" end)
open D

(* --------------------- Constants/data-structures for storing db contents *)

let db_FLUSH_TIMER=2.0 (* flush db write buffer every db_FLUSH_TIMER seconds *)
let display_sql_writelog_val = ref true (* compute/write sql-writelog debug string *)

(* --------------------- Util functions on db datastructures *)

let master_database = ref (Db_cache_types.Database.make Schema.empty)

let __test_set_master_database db = master_database := db

let make () = Db_ref.in_memory (ref master_database)

(* !!! Right now this is called at cache population time. It would probably be preferable to call it on flush time instead, so we
   don't waste writes storing non-persistent field values on disk.. At the moment there's not much to worry about, since there are
   few non-persistent fields. However, if this number increases we may want to consider calling this fn on every flush instead.. *)

(* non-persistent fields will have been flushed to disk anyway [since non-persistent just means dont trigger a flush
   if I change]. Hence we blank non-persistent fields with a suitable empty value, depending on their type *)
let blow_away_non_persistent_fields (schema: Schema.t) db =
  let g = Manifest.generation (Database.manifest db) in
  (* Generate a new row given a table schema *)
  let row schema row : Row.t * int64 =
    Row.fold
      (fun name { Stat.created; modified } v (acc,max_upd) ->
         try
           let col = Schema.Table.find name schema in
           let empty = col.Schema.Column.empty in
           let v',modified' = if col.Schema.Column.persistent then v,modified else empty,g in
           (Row.update modified' name empty (fun _ -> v') (Row.add created name v' acc),max max_upd modified')
         with Not_found ->
           Printf.printf "Skipping unknown column: %s\n%!" name;
           (acc,max max_upd modified)) row (Row.empty,0L) in
  (* Generate a new table *)
  let table tblname tbl : Table.t =
    let schema = Schema.Database.find tblname schema.Schema.database in
    Table.fold
      (fun objref { Stat.created; modified } r acc ->
         let (r,updated) = row schema r in
         Table.update modified objref Row.empty (fun _ -> r) (Table.add created objref r acc)) tbl Table.empty in
  Database.update
    (fun ts ->
       TableSet.fold
         (fun tblname { Stat.created; modified } tbl acc ->
            let tbl' = table tblname tbl in
            TableSet.add modified tblname tbl' acc) ts TableSet.empty)
    db

let db_registration_mutex = Mutex.create ()
let foreign_databases: ((string, Db_ref.t) Hashtbl.t) = Hashtbl.create 5

open Xapi_stdext_threads.Threadext
let create_registered_session create_session db_ref =
  Mutex.execute db_registration_mutex
    (fun () ->
       let session = create_session () in
       Hashtbl.replace foreign_databases session db_ref;
       session)

let unregister_session session =
  Mutex.execute db_registration_mutex
    (fun () -> Hashtbl.remove foreign_databases session)

let is_session_registered session =
  Mutex.execute db_registration_mutex
    (fun () -> Hashtbl.mem foreign_databases session)

let get_registered_database session =
  Mutex.execute db_registration_mutex
    (fun () ->
       if Hashtbl.mem foreign_databases session then
         Some (Hashtbl.find foreign_databases session)
       else
         None)
