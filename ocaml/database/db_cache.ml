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


open Db_interface

module D=Debug.Make(struct let name="db_cache" end)
open D


(** Masters will use this to modify the in-memory cache directly *)
module Local_db : DB_ACCESS = Db_cache_impl

(** Slaves will use this to call the master by XMLRPC *)
module Remote_db : DB_ACCESS = Db_rpc_client_v1.Make(struct
    let initialise () =
      ignore (Master_connection.Master_connection.start_master_connection_watchdog());
      ignore (Master_connection.Master_connection.open_secure_connection())
    let rpc request = Master_connection.Master_connection.execute_remote_fn request
  end)

let get = function
  | Db_ref.In_memory _ -> (module Local_db  : DB_ACCESS)
  | Db_ref.Remote      -> (module Remote_db : DB_ACCESS)

let apply_delta_to_cache entry db_ref =
  let module DB = (Local_db : DB_ACCESS) in
  match entry with
  | Redo_log.CreateRow(tblname, objref, kvs) ->
    debug "Redoing create_row %s (%s)" tblname objref;
    DB.create_row db_ref tblname kvs objref
  | Redo_log.DeleteRow(tblname, objref) ->
    debug "Redoing delete_row %s (%s)" tblname objref;
    DB.delete_row db_ref tblname objref
  | Redo_log.WriteField(tblname, objref, fldname, newval) ->
    debug "Redoing write_field %s (%s) [%s -> %s]" tblname objref fldname newval;
    DB.write_field db_ref tblname objref fldname newval
