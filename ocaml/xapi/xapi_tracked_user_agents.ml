(*
 * Copyright (C) Cloud Software Group
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

module D = Debug.Make (struct let name = __MODULE__ end)

let max_user_agent_strlen = 64

let max_num = 128

let t = Hashtbl.create max_num

let q = Queue.create ()

let m = Mutex.create ()

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let update name version =
  with_lock m @@ fun () ->
  match Hashtbl.find_opt t name with
  | Some v ->
      if v <> version then Hashtbl.replace t name version
  | None ->
      ( if Queue.length q >= max_num then
          let to_remove = Queue.pop q in
          Hashtbl.remove t to_remove
      ) ;
      Queue.push name q ; Hashtbl.add t name version

let get () = with_lock m @@ fun () -> Hashtbl.to_seq t |> List.of_seq

let reset () = with_lock m @@ fun () -> Hashtbl.clear t ; Queue.clear q

(* Record the user agent in memory hash table and exposed (name, version) list
   by the interface get.
   User-Agent format is like "name/version comment", see rfc2616.
   Parse it as
   - name: the first part of the string, up to the first space or slash, and
     in the whitelist
   - version: the part after the slash, up to the next space or end of string,
     if no slash is found, then the version is empty
   - comment: the rest of the string, which is ignored
   Example:
   "XAPI/1.0" -> ("XAPI", "1.0")
   "XAPI/1.0 comment" -> ("XAPI", "1.0")
   "XAPI" -> ("XAPI", "")
   "XAPI 1.0 comment" -> ("XAPI", "")
   "XAPI1.0" -> ("XAPI1.0", "")
   When different versions of the same name are seen, keep the last-seen version.
   Safety:
   - Drop the user agent if its length exceeds max_user_agent_strlen.
   - Remove the oldest entry if the record list length exceeds max_num.
*)
let track ~__context =
  let ( let@ ) o f = Option.iter f o in
  let@ user_agent = Context.get_user_agent __context in
  let user_agent_strlen = String.length user_agent in
  if user_agent_strlen <= max_user_agent_strlen then
    let@ name, coming_version =
      match String.split_on_char ' ' user_agent with
      | name_version :: _ -> (
        match String.split_on_char '/' name_version with
        | name :: version :: _ ->
            Some (name, version)
        | name :: [] ->
            Some (name, "")
        | [] ->
            None
      )
      | [] ->
          None
    in
    update name coming_version
