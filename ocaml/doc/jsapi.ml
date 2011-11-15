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
	
	let changes_in_release rel =
		let search_obj obj =
			let changes = List.filter (fun (transition, release, doc) -> release = rel) obj.obj_lifecycle in
			let obj_changes : changes_t = 
				List.map (fun (transition, release, doc) ->
					(transition, obj.name, if doc = "" && transition = Published then obj.description else doc)
				) changes in
			
			let changes_for_msg m =
				let changes = List.filter (fun (transition, release, doc) -> release = rel) m.msg_lifecycle in
				List.map (fun (transition, release, doc) ->
					(transition, m.msg_name, if doc = "" && transition = Published then m.msg_doc else doc)
				) changes
			in
			let msgs = List.filter (fun m -> not m.msg_hide_from_docs) obj.messages in
			let msg_changes : changes_t = List.fold_left (fun l m -> l @ (changes_for_msg m)) [] msgs in
			
			let changes_for_field f =
				let changes = List.filter (fun (transition, release, doc) -> release = rel) f.lifecycle in
				let field_name = String.concat "_" f.full_name in
				List.map (fun (transition, release, doc) ->
					(transition, field_name, if doc = "" && transition = Published then f.field_description else doc)
				) changes
			in
			let rec flatten_contents contents =
				List.fold_left (fun l -> function
					| Field f -> f :: l
					| Namespace (name, contents) -> flatten_contents contents @ l
				) [] contents
			in
			let fields = flatten_contents obj.contents in
			let fields = List.filter (fun f -> not f.internal_only) fields in
			let field_changes : changes_t = List.fold_left (fun l f -> l @ (changes_for_field f)) [] fields in
			
			"{'cls': '" ^ obj.name ^ "', 'obj_changes': " ^ Jsonrpc.to_string (rpc_of_changes_t obj_changes) ^ ", 'field_changes': " ^ Jsonrpc.to_string (rpc_of_changes_t field_changes) ^ ", 'msg_changes': " ^ Jsonrpc.to_string (rpc_of_changes_t msg_changes) ^ "}"
		in
		let release_info = String.concat ", " (List.map search_obj objs) in
		Unixext.write_string_to_file ("api/" ^ rel ^ ".json") ("release_info = [" ^ release_info ^ "]")
	in
	List.iter changes_in_release release_order;
	let release_list = String.concat ", " (List.map (fun s -> "'" ^ s ^ "'") release_order) in
	Unixext.write_string_to_file "api/releases.json" ("releases = [" ^ release_list ^ "]");
