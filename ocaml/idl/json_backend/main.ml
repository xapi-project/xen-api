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
open Datamodel_utils
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

let rec string_of_ty_with_enums ty =
	match ty with
		| String -> "string", []
		| Int -> "int", []
		| Float -> "float", []
		| Bool -> "bool", []
		| DateTime -> "datetime", []
		| Enum (name, kv) -> "enum " ^ name, [name, kv]
		| Set (ty) ->
			let s, e = string_of_ty_with_enums ty in
			s ^ " set", e
		| Map (ty1, ty2) ->
			let s1, e1 = string_of_ty_with_enums ty1 in
			let s2, e2 = string_of_ty_with_enums ty2 in
			Printf.sprintf "(%s -> %s) map" s1 s2, e1 @ e2
		| Ref r -> r ^ " ref", []
		| Record r -> r ^ " record", []

let string_of_qualifier = function
	| RW -> "read/write"
	| StaticRO | DynamicRO -> "read only"

let rec string_of_default = function
	| VString x -> "\"" ^ x ^ "\""
	| VInt x -> Int64.to_string x
	| VFloat x -> string_of_float x
	| VBool x -> string_of_bool x
	| VDateTime x -> Date.to_string x
	| VEnum x -> x
	| VMap x -> Printf.sprintf "{%s}" (String.concat ", " (List.map (fun (a, b) -> Printf.sprintf "%s -> %s" (string_of_default a) (string_of_default b)) x))
	| VSet x -> Printf.sprintf "{%s}" (String.concat ", " (List.map string_of_default x))
	| VRef x -> if x = "" then "Null" else x

let jarray_of_lifecycle lc =
	JArray (List.map (fun (t, r, d) ->
		JObject [
			"transition", JString (string_of_lifecycle_transition t);
			"release", JString r;
			"description", JString d;
		]
	) lc)

let fields_of_obj_with_enums obj =
	let rec flatten_contents contents =
		List.fold_left (fun l -> function
			| Field f -> f :: l
			| Namespace (name, contents) -> flatten_contents contents @ l
		) [] contents
	in
	let fields = flatten_contents obj.contents in
	let fields = List.filter (fun f -> not f.internal_only) fields in
	List.fold_left (fun (fields, enums) field ->
		let ty, e = string_of_ty_with_enums field.ty in
		JObject (
			("name", JString field.field_name) ::
			("description", JString field.field_description) ::
			("type", JString ty) ::
			("qualifier", JString (string_of_qualifier field.qualifier)) ::
			("tag", JString (match field.field_doc_tags with [] -> "" | t :: _ -> string_of_doc_tag t)) ::
			("lifecycle", jarray_of_lifecycle field.lifecycle) ::
			match field.default_value with Some d -> ["default", JString (string_of_default d)] | None -> []
		) :: fields,
		enums @ e
	) ([], []) fields

let jarray_of_result_with_enums = function
	| None -> JArray [JString "void"], []
	| Some (t, d) ->
		let t', enums = string_of_ty_with_enums t in
		JArray [JString t'; JString d], enums

let jarray_of_params_with_enums ps =
	let params, enums = List.fold_left (fun (params, enums) p ->
		let t, e = string_of_ty_with_enums p.param_type in
		JObject [
			"type", JString t;
			"name", JString p.param_name;
			"doc", JString p.param_doc;
		] :: params,
		enums @ e
	) ([], []) ps in
	JArray (List.rev params), enums

let jarray_of_errors es =
	JArray (List.map (fun e ->
		JObject [
			"name", JString e.err_name;
			"doc", JString e.err_doc;
		]
	) es )

let jarray_of_roles = function
	| None -> JArray []
	| Some rs -> JArray (List.map (fun s -> JString s) rs)

let messages_of_obj_with_enums obj =
	let msgs = List.filter (fun m -> not m.msg_hide_from_docs) obj.messages in
	List.fold_left (fun (msgs, enums) msg ->
		let result, enums1 = jarray_of_result_with_enums msg.msg_result in
		let params, enums2 = jarray_of_params_with_enums msg.msg_params in
		JObject [
			"name", JString msg.msg_name;
			"description", JString msg.msg_doc;
			"result", result;
			"params", params;
			"errors", jarray_of_errors msg.msg_errors;
			"roles", jarray_of_roles msg.msg_allowed_roles;
			"tag", JString (match msg.msg_doc_tags with [] -> "" | t :: _ -> string_of_doc_tag t);
			"lifecycle", jarray_of_lifecycle msg.msg_lifecycle;
		] :: msgs,
		enums @ enums1 @ enums2
	) ([], []) msgs

let jarray_of_enums enums =
	JArray (List.map (fun (name, vs) ->
		JObject [
			"name", JString name;
			"values", JArray (List.map (fun (v, d) -> JObject [
				"name", JString v;
				"doc", JString d;
			]) vs);
		]
	) enums)

let _ =
	let api = Datamodel.all_api in
	let objs = objects_of_api api in
	let json = JArray (List.map (fun obj ->
		let fields, enums1 = fields_of_obj_with_enums obj in
		let messages, enums2 = messages_of_obj_with_enums obj in
		let enums = Listext.List.setify (enums1 @ enums2) in
		JObject [
			"name", JString obj.name;
			"description", JString obj.description;
			"fields", JArray fields;
			"messages", JArray messages;
			"enums", jarray_of_enums enums;
			"lifecycle", jarray_of_lifecycle obj.obj_lifecycle;
			"tag", JString (match obj.obj_doc_tags with [] -> "" | t :: _ -> string_of_doc_tag t);
		]
	) objs) in
	print_endline (string_of_json 0 json)

