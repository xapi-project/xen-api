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
  | JBoolean of bool
  | JEmpty

let endl n =
  if n = 0 then ""
  else "\n" ^ String.make (2*n - 2) ' '

let rec string_of_json n = function
  | JObject l -> (endl n) ^ "{ " ^ (String.concat ("," ^ (endl (n+1))) (List.map (fun (s, j) -> "\"" ^ s ^ "\": " ^ (string_of_json (n+2) j)) l)) ^ " }"
  | JArray l -> "[ " ^ (String.concat ", " (List.map (fun j -> (string_of_json n j)) l)) ^ " ]"
  | JString s -> "\"" ^ (escape_json s) ^ "\""
  | JNumber n -> Printf.sprintf "%.4f" n
  | JBoolean b -> if b = true then "true" else "false"
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
  | RW -> "RW"
  | StaticRO -> "RO/constructor"
  | DynamicRO -> "RO/runtime"

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
  | VCustom (_,y) -> string_of_default y

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
  List.fold_left (fun (fields, enums) field ->
      let ty, e = string_of_ty_with_enums field.ty in
      JObject (
        ("name", JString (String.concat "_" field.full_name)) ::
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

let session_id =
  {
    param_type = Ref Datamodel_common._session;
    param_name = "session_id";
    param_doc = "Reference to a valid session";
    param_release = Datamodel_common.rio_release;
    param_default = None;
  }

let messages_of_obj_with_enums obj =
  List.fold_left (fun (msgs, enums) msg ->
      let params =
        if msg.msg_session then
          session_id :: msg.msg_params
        else
          msg.msg_params
      in
      let ctor =
        if msg.msg_tag = FromObject Make then
          let ctor_fields =
            List.filter (function { qualifier = (StaticRO | RW) } -> true | _ -> false) (fields_of_obj obj)
            |> List.map (fun f -> String.concat "_" f.full_name ^ (if f.default_value = None then "*" else ""))
          in
          Printf.sprintf "\nThe constructor args are: %s (* = non-optional)." (String.concat ", " ctor_fields)
        else
          ""
      in
      let result, enums1 = jarray_of_result_with_enums msg.msg_result in
      let params, enums2 = jarray_of_params_with_enums params in
      JObject [
        "name", JString msg.msg_name;
        "description", JString (msg.msg_doc ^ ctor);
        "result", result;
        "params", params;
        "errors", jarray_of_errors msg.msg_errors;
        "roles", jarray_of_roles msg.msg_allowed_roles;
        "tag", JString (match msg.msg_doc_tags with [] -> "" | t :: _ -> string_of_doc_tag t);
        "lifecycle", jarray_of_lifecycle msg.msg_lifecycle;
        "implicit", JBoolean (msg.msg_tag <> Custom);
      ] :: msgs,
      enums @ enums1 @ enums2
    ) ([], []) obj.messages

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

let json_of_objs objs =
  JArray (List.map (fun obj ->
      let fields, enums1 = fields_of_obj_with_enums obj in
      let messages, enums2 = messages_of_obj_with_enums obj in
      let enums = Xapi_stdext_std.Listext.List.setify (enums1 @ enums2) in
      JObject [
        "name", JString obj.name;
        "description", JString obj.description;
        "fields", JArray fields;
        "messages", JArray messages;
        "enums", jarray_of_enums enums;
        "lifecycle", jarray_of_lifecycle obj.obj_lifecycle;
        "tag", JString (match obj.obj_doc_tags with [] -> "" | t :: _ -> string_of_doc_tag t);
      ]
    ) objs)

let jobject_of_change (t, n, l, s) =
  JObject [
    "transition", JString (string_of_lifecycle_transition t ^ " " ^ s);
    "name", JString n;
    "log", JString l;
  ]

let compare_changes (a_t, a_n, _, a_k) (b_t, b_n, _, b_k) =
  let int_of_transition = function
    | Published -> 0
    | Extended -> 10
    | Changed -> 20
    | Deprecated -> 30
    | Removed -> 40
    | Prototyped -> 50
  in
  let int_of_kind = function
    | "class" -> 0
    | "field" -> 1
    | "message" -> 2
    | _ -> 3
  in
  let cmp = compare ((int_of_transition a_t) + (int_of_kind a_k)) ((int_of_transition b_t) + (int_of_kind b_k)) in
  if cmp = 0 then
    compare a_n b_n
  else
    cmp

let releases objs =
  let changes_in_release rel =
    let search_obj obj =
      let changes = List.filter (fun (transition, release, doc) -> release = code_name_of_release rel) obj.obj_lifecycle in
      let obj_changes =
        List.map (fun (transition, release, doc) ->
            transition,
            obj.name,
            (if doc = "" && transition = Published then obj.description else doc),
            "class"
          ) changes in

      let changes_for_msg m =
        let changes = List.filter (fun (transition, release, doc) -> release = code_name_of_release rel) m.msg_lifecycle in
        List.map (fun (transition, release, doc) ->
            transition,
            obj.name ^ "." ^ m.msg_name,
            (if doc = "" && transition = Published then m.msg_doc else doc),
            "message"
          ) changes
      in
      (* Don't include implicit messages *)
      let msgs = List.filter (fun m -> m.msg_tag = Custom) obj.messages in
      let msg_changes = List.fold_left (fun l m -> l @ (changes_for_msg m)) [] msgs in

      let changes_for_field f =
        let changes = List.filter (fun (transition, release, doc) -> release = code_name_of_release rel) f.lifecycle in
        let field_name = String.concat "_" f.full_name in
        List.map (fun (transition, release, doc) ->
            transition,
            obj.name ^ "." ^ field_name,
            (if doc = "" && transition = Published then f.field_description else doc),
            "field"
          ) changes
      in
      let rec flatten_contents contents =
        List.fold_left (fun l -> function
            | Field f -> f :: l
            | Namespace (name, contents) -> flatten_contents contents @ l
          ) [] contents
      in
      let fields = flatten_contents obj.contents in
      let field_changes = List.fold_left (fun l f -> l @ (changes_for_field f)) [] fields in

      obj_changes @ field_changes @ msg_changes
    in
    JArray (List.map search_obj objs |> List.flatten |> List.sort compare_changes |> List.map jobject_of_change)
  in
  let release_info = JObject (List.map (fun rel -> code_name_of_release rel, changes_in_release rel) release_order) in
  Xapi_stdext_unix.Unixext.write_string_to_file ("release_info.json") (string_of_json 0 release_info)

let _ =
  let api = Datamodel.all_api in
  (* Add all implicit messages *)
  let api = add_implicit_messages api in
  (* Only include messages that are visible to a XenAPI client *)
  let api = filter (fun _ -> true) (fun _ -> true) on_client_side api in
  (* And only messages marked as not hidden from the docs, and non-internal fields *)
  let api = filter (fun _ -> true) (fun f -> not f.internal_only) (fun m -> not m.msg_hide_from_docs) api in

  let objs = objects_of_api api in
  Xapi_stdext_unix.Unixext.write_string_to_file "xenapi.json" (objs |> json_of_objs |> string_of_json 0);
  releases objs

