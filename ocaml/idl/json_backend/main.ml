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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
 * GNU Lesser General Public License for more details.
 *)
(* main.ml *)

open Datamodel_types
open Dm_api

(* JSON *)

let escape_json s =
	let len = String.length s in
	if len > 0 then begin
		let buf = Buffer.create len in
		for i = 0 to len - 1 do
		match s.[i] with
		| '\"' -> Buffer.add_string buf "\\\""
		| '\\' -> Buffer.add_string buf "\\\\"
		| '\b' -> Buffer.add_string buf "\\b"
		| '\n' -> Buffer.add_string buf "\\n"
		| '\r' -> Buffer.add_string buf "\\r"
		| '\t' -> Buffer.add_string buf "\\t"
		| c -> Buffer.add_char buf c
		done;
		Buffer.contents buf
	end
	else ""

type json =
	| JObject of (string * json) list
	| JArray of json list
	| JString of string
	| JNumber of float
	| JTrue
	| JFalse
	| JEmpty

let endl n =
	if n = 0 then ""
	else "\n" ^ String.make (2*n - 2) ' '

let rec string_of_json n = function
	| JObject l -> (endl n) ^ "{ " ^ (String.concat ("," ^ (endl (n+1))) (List.map (fun (s, j) -> "\"" ^ s ^ "\": " ^ (string_of_json (n+2) j)) l)) ^ " }"
	| JArray l -> "[ " ^ (String.concat ", " (List.map (fun j -> (string_of_json n j)) l)) ^ " ]"
	| JString s -> "\"" ^ (escape_json s) ^ "\""
	| JNumber n -> Printf.sprintf "%.4f" n
	| JTrue -> "true"
	| JFalse -> "false"
	| JEmpty -> "\"\""


(* Datamodel *)

let rec string_of_ty ty =
	match ty with
		| String -> "string"
		| Int -> "int"
		| Float -> "float"
		| Bool -> "bool"
		| DateTime -> "datetime"
		| Enum (name, _) -> "enum " ^ name
		| Set (ty) -> (string_of_ty ty) ^ " set"
		| Map (ty1, ty2) -> Printf.sprintf "(%s -> %s) map" (string_of_ty ty1) (string_of_ty ty2)
		| Ref r -> r ^ " ref"
		| Record r -> r ^ " record"

let fields_of_obj obj =
	let rec flatten_contents contents =
		List.fold_left (fun l -> function
			| Field f -> f :: l
			| Namespace (name, contents) -> flatten_contents contents @ l
		) [] contents
	in
	let fields = flatten_contents obj.contents in
	let fields = List.filter (fun f -> not f.internal_only) fields in
	List.map (fun field ->
		JObject [
			"name", JString field.field_name;
			"description", JString field.field_description;
			"type", JString (string_of_ty field.ty);
			"tag", JString "snapshots";
		]
	) fields

let messages_of_obj obj =
	let msgs = List.filter (fun m -> not m.msg_hide_from_docs) obj.messages in
	List.map (fun msg ->
		JObject [
			"name", JString msg.msg_name;
			"description", JString msg.msg_doc;
			"tag", JString "snapshots";
		]
	) msgs

let _ =
	let api = Datamodel_utils.add_implicit_messages (Datamodel.all_api) in
	let objs = objects_of_api api in
	let json = JArray (List.map (fun obj ->
		JObject [
			"name", JString obj.name;
			"fields", JArray (fields_of_obj obj);
			"messages", JArray (messages_of_obj obj);
		]
	) objs) in
	print_endline (string_of_json 0 json)

