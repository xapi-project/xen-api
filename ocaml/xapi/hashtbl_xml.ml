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

module D=Debug.Debugger(struct let name="hashtbl_xml" end)
open D

type h = (string, string) Hashtbl.t

exception Unmarshall_error of string

let name x = ("", x) (* No namespace associated with our tags *)
let make_tag n attrs = (name n), List.map (fun (k, v) -> name k, v) attrs

let to_xml (db: h) (output: Xmlm.output) =
  let table = make_tag "config" [] in
  Xmlm.output_signal output (`S table);
  Hashtbl.iter (fun k v -> 
    let row = make_tag "row" ["key",k; "value",v] in
    Xmlm.output_signal output (`S row);
    Xmlm.output_signal output (`E);
  ) db;
  Xmlm.output_signal output (`E)

let of_xml (input: Xmlm.input) =
  let db = Hashtbl.create 10 in
  let s (tag: Xmlm.tag) acc = match tag with
    | (_, "config"), attrs -> acc
    | (_, "row"), attrs -> 
	let key=List.assoc ("","key") attrs in
	let value=List.assoc ("","value") attrs in
	(key,value)::acc
    | (ns, name), attrs -> raise (Unmarshall_error (Printf.sprintf "Unknown tag: (%s,%s)" ns name))
  in
  let e (tag: Xmlm.tag) acc = acc in
  let kvs = Xmlm.input ~s ~e [] input in
  List.iter (fun (k,v) -> Hashtbl.add db k v) kvs;
  db
