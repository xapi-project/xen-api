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

module D=Debug.Debugger(struct let name="db_cache" end)
open D


(** Masters will use this to modify the in-memory cache directly *)
module Local_db : DB_ACCESS = Db_cache_impl

(** Slaves will use this to call the master by XMLRPC *)
module Remote_db : DB_ACCESS = Db_rpc_client_v1.Make(struct
	let initialise () = 
		ignore (Master_connection.start_master_connection_watchdog());
		ignore (Master_connection.open_secure_connection())
	let rpc request = Master_connection.execute_remote_fn request Constants.remote_db_access_uri
end)

exception Must_initialise_database_mode
let implementation = ref None
let set_master = function
	| false ->
		implementation := Some (module Remote_db : DB_ACCESS)
	| true ->
		implementation := Some (module Local_db : DB_ACCESS)
let get () = match !implementation with
	| None -> raise Must_initialise_database_mode
	| Some m -> m

let apply_delta_to_cache entry =
	let module DB = (val (get ()) : DB_ACCESS) in	
    let context = Context.make "redo_log" in
    match entry with 
		| Redo_log.CreateRow(tblname, objref, kvs) ->
			debug "Redoing create_row %s (%s)" tblname objref;
			DB.create_row tblname kvs objref
		| Redo_log.DeleteRow(tblname, objref) ->
			debug "Redoing delete_row %s (%s)" tblname objref;
			DB.delete_row tblname objref
		| Redo_log.WriteField(tblname, objref, fldname, newval) ->
			debug "Redoing write_field %s (%s) [%s -> %s]" tblname objref fldname newval;
			DB.write_field tblname objref fldname newval
