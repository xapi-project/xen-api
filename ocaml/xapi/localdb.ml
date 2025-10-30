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

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module D = Debug.Make (struct let name = "localdb" end)

open D

let db = Hashtbl.create 10 (* in-memory cache *)

let loaded = ref false

let to_db (output : Xmlm.output) = Hashtbl_xml.to_xml db output

let of_db (input : Xmlm.input) =
  let db' = Hashtbl_xml.of_xml input in
  Hashtbl.clear db ;
  Hashtbl.iter (fun k v -> Hashtbl.add db k v) db'

let assert_loaded () =
  if not !loaded then (
    try
      ignore (Unix.stat Xapi_globs.local_database) ;
      let ic = open_in Xapi_globs.local_database in
      finally
        (fun () ->
          of_db (Xmlm.make_input (`Channel ic)) ;
          loaded := true
        )
        (fun () -> close_in ic) ;
      Hashtbl.iter (fun k v -> debug "%s = %s" k v) db
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        debug "Local database %s doesn't currently exist. Continuing."
          Xapi_globs.local_database
    | Xmlm.Error _ ->
        debug
          "Xml error processing local database %s. Moving it out of the way."
          Xapi_globs.local_database ;
        let corrupt_fname = Xapi_globs.local_database ^ ".corrupt" in
        Xapi_stdext_unix.Unixext.unlink_safe corrupt_fname ;
        Unix.rename Xapi_globs.local_database corrupt_fname
  )

exception Missing_key of string

(* serialise all read/writes (avoids sqlite3 corruption bug) *)
let m = Mutex.create ()

let get (key : string) =
  let __FUN = __FUNCTION__ in
  let ( let* ) = Option.bind in
  with_lock m (fun () ->
      let* () =
        try assert_loaded () ; Some ()
        with e ->
          warn "%s: unexpected error, ignoring it: %s" __FUN
            (Printexc.to_string e) ;
          None
      in
      Hashtbl.find_opt db key
  )

let get_exn key =
  match get key with Some x -> x | None -> raise (Missing_key key)

let get_of_string of_string key = Option.bind (get key) of_string

let get_bool key = get_of_string bool_of_string_opt key

let get_int key = get_of_string int_of_string_opt key

(* Returns true if a change was made and should be flushed *)
let put_one (key : string) (v : string) =
  match Hashtbl.find_opt db key with
  | Some x when x = v ->
      false (* no change necessary *)
  | _ ->
      Hashtbl.replace db key v ; true

let flush () =
  let b = Buffer.create 256 in
  to_db (Xmlm.make_output (`Buffer b)) ;
  let s = Buffer.contents b in
  Xapi_stdext_unix.Unixext.write_string_to_file Xapi_globs.local_database s

let put (key : string) (v : string) =
  with_lock m (fun () ->
      assert_loaded () ;
      if put_one key v then
        flush ()
  )

let putv (all : (string * string) list) =
  with_lock m (fun () ->
      assert_loaded () ;
      let changes_made = List.map (fun (k, v) -> put_one k v) all in
      if
        List.fold_left ( || ) false changes_made
        (* if any changes were made flush the lot *)
      then
        flush ()
  )

let del (key : string) =
  with_lock m (fun () ->
      assert_loaded () ;
      Hashtbl.remove db key ;
      (* Does nothing if the key isn't there *)
      flush ()
  )
