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
(* Store and retrieve some host-specific data as key-value pairs. This can
   be used in emergency mode since every slave has its own copy. *)

open Stdext.Threadext
open Stdext.Pervasiveext

module D=Debug.Make(struct let name="localdb" end)
open D

let db = Hashtbl.create 10 (* in-memory cache *)
let loaded = ref false

let to_db (output: Xmlm.output) = Hashtbl_xml.to_xml db output
let of_db (input: Xmlm.input) =
  let db' = Hashtbl_xml.of_xml input in
  Hashtbl.clear db;
  Hashtbl.iter (fun k v -> Hashtbl.add db k v) db'

let assert_loaded () =
  if not(!loaded) then begin
    try
      ignore(Unix.stat Xapi_globs.local_database);
      let ic = open_in Xapi_globs.local_database in
      finally
        (fun () -> of_db (Xmlm.make_input (`Channel ic)); loaded := true)
        (fun () -> close_in ic);
      Hashtbl.iter (fun k v -> debug "%s = %s" k v) db
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
      debug "Local database %s doesn't currently exist. Continuing." Xapi_globs.local_database
    | Xmlm.Error _ ->
      debug "Xml error processing local database %s. Moving it out of the way." Xapi_globs.local_database;
      let corrupt_fname = Xapi_globs.local_database^".corrupt" in
      Stdext.Unixext.unlink_safe corrupt_fname;
      Unix.rename Xapi_globs.local_database corrupt_fname
  end

exception Missing_key of string

(* serialise all read/writes (avoids sqlite3 corruption bug) *)
let m = Mutex.create ()

let get (key: string) =
  Mutex.execute m
    (fun () ->
       assert_loaded ();
       try
         Hashtbl.find db key
       with Not_found -> raise (Missing_key key)
    )

let get_with_default (key: string) (default: string) =
  try
    get key
  with Missing_key _ -> default

(* Returns true if a change was made and should be flushed *)
let put_one (key: string) (v: string) =
  if Hashtbl.mem db key && Hashtbl.find db key = v
  then false (* no change necessary *)
  else (Hashtbl.replace db key v; true)

let flush () =
  let b = Buffer.create 256 in
  to_db (Xmlm.make_output (`Buffer b));
  let s = Buffer.contents b in
  Stdext.Unixext.write_string_to_file Xapi_globs.local_database s

let put (key: string) (v: string) =
  Mutex.execute m
    (fun () ->
       assert_loaded ();
       if put_one key v;
       then flush ())

let putv (all: (string * string) list) =
  Mutex.execute m
    (fun () ->
       assert_loaded ();
       let changes_made = List.map (fun (k, v) -> put_one k v) all in
       if List.fold_left (||) false changes_made (* if any changes were made flush the lot *)
       then flush ())

let del (key : string) =
  Mutex.execute m
    (fun () ->
       assert_loaded ();
       Hashtbl.remove db key; (* Does nothing if the key isn't there *)
       flush ())
