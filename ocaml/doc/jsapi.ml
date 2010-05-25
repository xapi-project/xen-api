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
 
open Datamodel_types
open Stringext

type change_t = lifecycle_change * string * string
and changes_t = change_t list
with rpc

let obj_change_in_release rel o =
	let rec find_rel rel = function
		| [] -> None
		| (transition, release, doc) :: tl when release = rel -> Some (transition, o.name, doc)
		| _ :: tl -> find_rel rel tl
	in
	find_rel rel o.obj_lifecycle
	
let msg_change_in_release rel m =
	let rec find_rel rel = function
		| [] -> None
		| (transition, release, doc) :: tl when release = rel -> Some (transition, m.msg_name, doc)
		| _ :: tl -> find_rel rel tl
	in
	find_rel rel m.msg_lifecycle

let field_change_in_release rel f =
	let rec find_rel rel = function
		| [] -> None
		| (transition, release, doc) :: tl when release = rel -> Some (transition, f.field_name, doc)
		| _ :: tl -> find_rel rel tl
	in
	find_rel rel f.lifecycle
	
let _ =
	let api = (Datamodel.all_api) in
	let objs = Dm_api.objects_of_api api in
	let create_json obj =
		let name = obj.name in
		let s = Jsonrpc.to_string (rpc_of_obj obj) in
		Unixext.write_string_to_file ("api/" ^ name ^ ".json") ("clsdoc = " ^ s);
		name
	in
	let names = List.map create_json objs in
	let class_list = String.concat ", " (List.map (fun s -> "'" ^ s ^ "'") names) in
	Unixext.write_string_to_file "api/index.json" ("classes = [" ^ class_list ^ "]");
	
	let new_in_release rel =
		let search_obj obj =
			let obj_changes : changes_t = 
				match obj_change_in_release rel obj with
				| None -> []
				| Some x -> [x]
			in
				
			let msgs = List.filter (fun m -> not m.msg_hide_from_docs) obj.messages in
			let msg_changes : changes_t = List.fold_left
				(fun l m -> match msg_change_in_release rel m with None -> l | Some x -> x :: l) [] msgs in
				
			let flds = List.filter (function Field f -> true | _ -> false) obj.contents in
			let field_changes : changes_t = List.fold_left
				(fun l (Field f) -> match field_change_in_release rel f with None -> l | Some x -> x :: l) [] flds in
				
			"{'cls': '" ^ obj.name ^ "', 'obj_changes': " ^ Jsonrpc.to_string (rpc_of_changes_t obj_changes) ^ ", 'field_changes': " ^ Jsonrpc.to_string (rpc_of_changes_t field_changes) ^ ", 'msg_changes': " ^ Jsonrpc.to_string (rpc_of_changes_t msg_changes) ^ "}"
		in
		let release_info = String.concat ", " (List.map search_obj objs) in
		Unixext.write_string_to_file ("api/" ^ rel ^ ".json") ("release_info = [" ^ release_info ^ "]")
	in
	List.iter new_in_release release_order;
	let release_list = String.concat ", " (List.map (fun s -> "'" ^ s ^ "'") release_order) in
	Unixext.write_string_to_file "api/releases.json" ("releases = [" ^ release_list ^ "]");
