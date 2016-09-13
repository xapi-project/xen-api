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
(* Simple code to serialise and deserialise a (string, string) Hashtbl.t as XML via Xmlm *)

module D=Debug.Make(struct let name="hashtbl_xml" end)
open D

type h = (string, string) Hashtbl.t

exception Unmarshall_error of string

let name x = ("", x) (* No namespace associated with our tags *)
let make_tag n attrs = (name n), List.map (fun (k, v) -> name k, v) attrs

let to_xml (db: h) (output: Xmlm.output) =
  let elts  = Hashtbl.fold (fun k v acc -> `El ( (make_tag "row" ["key",k; "value",v]), [] ) :: acc) db [] in
  let table = `El (make_tag "config" [], elts) in
  let id x = x in
  Xmlm.output output (`Dtd None);
  Xmlm.output_tree id output table

let of_xml (input: Xmlm.input) =
  debug "Converting %s" (match Xmlm.peek input with `El_start ((t,_),_) -> "Start: "^ t | `Data s -> "Data: " ^ s | `El_end -> "End" | `Dtd _ -> "dtd");
  let db = Hashtbl.create 10 in
  let el (tag: Xmlm.tag) acc = match tag with
    | (_, "config"), attrs -> List.flatten acc
    | (_, "row"), attrs ->
      let key=List.assoc ("","key") attrs in
      let value=List.assoc ("","value") attrs in
      (key,value)::List.flatten acc
    | (ns, name), attrs -> raise (Unmarshall_error (Printf.sprintf "Unknown tag: (%s,%s)" ns name))
  in
  let data str = [] in
  let _, kvs = Xmlm.input_doc_tree ~el ~data input in
  List.iter (fun (k,v) -> Hashtbl.add db k v) kvs;
  db
