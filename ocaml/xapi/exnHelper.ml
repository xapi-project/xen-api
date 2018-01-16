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
(* Code to convert ocaml exceptions into XAPI exceptions *)

(* XXX: need to push some of this into the datamodel *)

open XMLRPC
open Api_errors
open Printf

module D = Debug.Make(struct let name="backtrace" end)
open D

let error_of_exn e =
  match e with
  | Stunnel.Stunnel_error msg ->
    internal_error, [ "Connection failed: " ^ (String.lowercase_ascii msg) ^ "." ]
  | Db_exn.DBCache_NotFound ("missing reference", tblname, reference) ->
    (* whenever a reference has been destroyed *)
    handle_invalid, [tblname; reference ]
  | Db_exn.Too_many_values(tbl, objref, uuid) ->
    (* Very bad: database has duplicate references or UUIDs *)
    internal_error, [ sprintf "duplicate objects in database: tbl='%s'; object_ref='%s'; uuid='%s'" tbl objref uuid ]
  | Db_exn.DBCache_NotFound (reason,p1,p2) ->
    begin
      match reason with
        "missing row" -> handle_invalid, [p1; p2]
      | s -> internal_error, [reason; p1; p2]
    end
  | Db_exn.Duplicate_key (tbl,fld,uuid,key) ->
    map_duplicate_key, [ tbl; fld; uuid; key ]
  | Db_exn.Read_missing_uuid (tbl,ref,uuid) ->
    uuid_invalid, [ tbl; uuid ]
  | Db_actions.DM_to_String.StringEnumTypeError s
  | Db_actions.DM_to_String.DateTimeError s
  | Db_actions.String_to_DM.StringEnumTypeError s ->
    invalid_value, [ s ]

  (* These are the two catch-all patterns. If ever an Errors.Server_error exception		*)
  (* is raised, this is assumed to be an API error, and passed straight on. Any other *)
  (* exception at this point is regarded as an 'internal error', and returned as such *)

  | Api_errors.Server_error (e,l) ->
    e,l
  | Forkhelpers.Spawn_internal_error(stderr, stdout, Unix.WEXITED n) ->
    internal_error, [ Printf.sprintf "Subprocess exited with unexpected code %d; stdout = [ %s ]; stderr = [ %s ]" n stdout stderr ]
  | Invalid_argument x ->
    internal_error, [ Printf.sprintf "Invalid argument: %s" x ]
  | Db_filter.Expression_error (expr, exc) ->
    invalid_value, [ expr; (Printexc.to_string exc)]
  | e ->
    internal_error, [ Printexc.to_string e ]

let string_of_exn exn =
  let e, l = error_of_exn exn in
  Printf.sprintf "%s: [ %s ]" e (String.concat "; " l)
