(* Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

(* Generator of Go bindings from the datamodel *)

open Datamodel_types
open CommonFunctions
module Types = Datamodel_utils.Types

let templates_dir = "templates"

let ( // ) = Filename.concat

let snake_to_camel (s : string) : string =
  Astring.String.cuts ~sep:"_" s
  |> List.map (fun s -> Astring.String.cuts ~sep:"-" s)
  |> List.concat
  |> List.map String.capitalize_ascii
  |> String.concat ""

let render_template template_file json ?(newline = false) () =
  let templ =
    string_of_file (templates_dir // template_file) |> Mustache.of_string
  in
  let renndered = Mustache.render templ json in
  if newline then renndered ^ "\n" else renndered

let generate_file ~rendered ~destdir ~output_file =
  let out_chan = open_out (destdir // output_file) in
  Fun.protect
    (fun () -> output_string out_chan rendered)
    ~finally:(fun () -> close_out out_chan)

module Json = struct
  type enum = (string * string) list

  module StringMap = Map.Make (String)

  type enums = enum StringMap.t

  let choose_enum _key a _b = Some a

  let merge_maps m maps =
    List.fold_left (fun acc map -> StringMap.union choose_enum acc map) m maps

  let rec get_fp_type ty =
    match ty with
    | SecretString | String ->
        "String"
    | Int ->
        "Int"
    | Float ->
        "Float"
    | Bool ->
        "Bool"
    | DateTime ->
        "Time"
    | Enum (name, _) ->
        "Enum" ^ snake_to_camel name
    | Set ty ->
        get_fp_type ty ^ "Set"
    | Map (ty1, ty2) ->
        let fp_type1 = get_fp_type ty1 in
        let fp_type2 = get_fp_type ty2 in
        fp_type1 ^ "To" ^ fp_type2 ^ "Map"
    | Ref r ->
        snake_to_camel r ^ "Ref"
    | Record r ->
        snake_to_camel r ^ "Record"
    | Option ty ->
        get_fp_type ty

  let rec string_of_ty_with_enums ty : string * enums =
    match ty with
    | SecretString | String ->
        ("string", StringMap.empty)
    | Int ->
        ("int", StringMap.empty)
    | Float ->
        ("float64", StringMap.empty)
    | Bool ->
        ("bool", StringMap.empty)
    | DateTime ->
        ("time.Time", StringMap.empty)
    | Enum (name, kv) ->
        let name = snake_to_camel name in
        (name, StringMap.singleton name kv)
    | Set ty ->
        let s, e = string_of_ty_with_enums ty in
        ("[]" ^ s, e)
    | Map (ty1, ty2) ->
        let s1, e1 = string_of_ty_with_enums ty1 in
        let s2, e2 = string_of_ty_with_enums ty2 in
        let ty = "map[" ^ s1 ^ "]" ^ s2 in
        (ty, StringMap.union choose_enum e1 e2)
    | Ref r ->
        (snake_to_camel r ^ "Ref", StringMap.empty)
    | Record r ->
        (snake_to_camel r ^ "Record", StringMap.empty)
    | Option ty ->
        string_of_ty_with_enums ty

  let of_enum name vs =
    let name = snake_to_camel name in
    let of_value (v, d) =
      `O
        [
          ("value", `String v)
        ; ("doc", `String d)
        ; ("name", `String (name ^ snake_to_camel v))
        ; ("type", `String name)
        ]
    in
    `O [("name", `String name); ("values", `A (List.map of_value vs))]

  let of_field field =
    let concat_and_convert field =
      let concated =
        String.concat "" (List.map snake_to_camel field.full_name)
      in
      match concated with
      | "Uuid" | "Id" ->
          String.uppercase_ascii concated
      | _ ->
          concated
    in
    let ty, _e = string_of_ty_with_enums field.ty in
    `O
      [
        ("name", `String (concat_and_convert field))
      ; ("description", `String (String.trim field.field_description))
      ; ("type", `String ty)
      ]

  let modules_of_type = function
    | DateTime ->
        [`O [("name", `String "time"); ("sname", `Null)]]
    | _ ->
        []

  let modules_of_types types =
    let common = [`O [("name", `String "fmt"); ("sname", `Null)]] in
    let items =
      List.map modules_of_type types |> List.concat |> List.append common
    in
    `O [("import", `Bool true); ("items", `A items)]

  let all_enums objs =
    let enums =
      Types.of_objects objs
      |> List.map (fun ty ->
             let _, e = string_of_ty_with_enums ty in
             e
         )
      |> merge_maps StringMap.empty
    in
    `O
      [
        ( "enums"
        , `A (StringMap.fold (fun k v acc -> of_enum k v :: acc) enums [])
        )
      ]

  let get_event_snapshot name =
    if String.lowercase_ascii name = "event" then
      [
        `O
          [
            ("name", `String "Snapshot")
          ; ( "description"
            , `String
                "The record of the database object that was added, changed or \
                 deleted"
            )
          ; ("type", `String "RecordInterface")
          ]
      ]
    else
      []

  let get_event_session_value = function
    | "event" ->
        [("event", `Bool true); ("session", `Null)]
    | "session" ->
        [("event", `Null); ("session", `Bool true)]
    | _ ->
        [("event", `Null); ("session", `Null)]

  let of_result obj msg =
    match msg.msg_result with
    | None ->
        `Null
    | Some (t, _d) ->
        if obj.name = "event" && String.lowercase_ascii msg.msg_name = "from"
        then
          `O
            [
              ("type", `String "EventBatch")
            ; ("func_partial_type", `String "EventBatch")
            ]
        else
          let t', _ = string_of_ty_with_enums t in
          `O
            [
              ("type", `String t')
            ; ("func_partial_type", `String (get_fp_type t))
            ]

  let group_params params =
    List.fold_left
      (fun groups (param_release, info) ->
        match groups with
        | [] ->
            [(param_release, [info])]
        | group :: tl -> (
          match group with
          | pr, infos when pr = param_release ->
              (pr, info :: infos) :: tl
          | _ ->
              (param_release, [info]) :: groups
        )
      )
      [] params

  let scan_left func init list =
    let rec aux func init list result =
      match list with
      | [] ->
          result
      | x :: xs ->
          let init = func init x in
          aux func init xs (init :: result)
    in
    List.rev @@ aux func init list []

  let combine_groups groups =
    let scans =
      scan_left
        (fun (len, group) (_, infos) -> (len + List.length infos, group @ infos))
        (0, []) groups
    in
    match scans with [] -> [] | [x] -> [x] | _ :: xs -> xs

  let rec get_last = function
    | [] ->
        failwith "empty list"
    | [x] ->
        x
    | _ :: tl ->
        get_last tl

  let of_param_groups class_name params =
    let name_internal name =
      let name = name |> snake_to_camel |> String.uncapitalize_ascii in
      match name with "type" -> "typeKey" | "interface" -> "inter" | _ -> name
    in
    let base_assoc_list (t, name, doc, type_name) =
      [
        ("session", `Bool (name = "session_id"))
      ; ("type", `String t)
      ; ("name", `String name)
      ; ("name_internal", `String (name_internal name))
      ; ("doc", `String doc)
      ; ("func_partial_type", `String type_name)
      ]
    in
    let deal_with_logout name =
      match (class_name, name) with
      | "session", "session_id" ->
          ([("param_ignore", `Bool true); ("session_class", `Bool true)], true)
      | "session", _ ->
          ([("param_ignore", `Bool false); ("session_class", `Bool true)], false)
      | _ ->
          ([("param_ignore", `Null); ("session_class", `Null)], false)
    in
    let get_assoc_list (t, name, doc, type_name) =
      let fields, could_ignore = deal_with_logout name in
      (fields @ base_assoc_list (t, name, doc, type_name), could_ignore)
    in
    let rec add_first = function
      | head :: rest ->
          let assoc_list, could_ignore = get_assoc_list head in
          let assoc_list = ("first", `Bool (not could_ignore)) :: assoc_list in
          let others =
            if could_ignore then
              add_first rest
            else
              List.map
                (fun item ->
                  let assoc_list, _ = get_assoc_list item in
                  `O (("first", `Bool false) :: assoc_list)
                )
                rest
          in
          `O assoc_list :: others
      | [] ->
          []
    in
    let groups =
      params
      |> List.rev_map (fun p ->
             let fp_type = get_fp_type p.param_type in
             let t, _e = string_of_ty_with_enums p.param_type in
             (p.param_release, (t, p.param_name, p.param_doc, fp_type))
         )
      |> group_params
      |> combine_groups
      |> List.map (fun (num, params) -> (num, `A (add_first params)))
    in
    match get_last groups with
    | 0, _ ->
        groups
    | _, last ->
        groups @ [(0, last)]

  let of_error e = `O [("name", `String e.err_name); ("doc", `String e.err_doc)]

  let of_errors = function
    | [] ->
        `Null
    | errors ->
        `A (List.map of_error errors)

  let add_session_info class_name method_name =
    match (class_name, method_name) with
    | "session", "login_with_password"
    | "session", "slave_local_login_with_password" ->
        [
          ("session_class", `Bool true)
        ; ("session_login", `Bool true)
        ; ("session_logout", `Bool false)
        ]
    | "session", "logout" | "session", "local_logout" ->
        [
          ("session_class", `Bool true)
        ; ("session_login", `Bool false)
        ; ("session_logout", `Bool true)
        ]
    | "session", _ ->
        [
          ("session_class", `Bool true)
        ; ("session_login", `Bool false)
        ; ("session_logout", `Bool false)
        ]
    | _ ->
        [
          ("session_class", `Bool false)
        ; ("session_login", `Bool false)
        ; ("session_logout", `Bool false)
        ]

  let desc_of_msg msg ctor_fields =
    let ctor =
      if msg.msg_tag = FromObject Make then
        Printf.sprintf " The constructor args are: %s (* = non-optional)."
          ctor_fields
      else
        ""
    in
    match msg.msg_doc ^ ctor with
    | "" ->
        `Null
    | desc ->
        `String (String.trim desc)

  let ctor_fields_of_obj obj =
    Datamodel_utils.fields_of_obj obj
    |> List.filter (function
         | {qualifier= StaticRO | RW; _} ->
             true
         | _ ->
             false
         )
    |> List.map (fun f ->
           String.concat "_" f.full_name
           ^ if f.default_value = None then "*" else ""
       )
    |> String.concat ", "

  let messages_of_obj obj =
    let ctor_fields = ctor_fields_of_obj obj in
    let method_name_exported obj msg num =
      let method_name = snake_to_camel msg.msg_name in
      let not_ends_with str suffix = not (String.ends_with str ~suffix) in
      let num =
        match (obj.name, msg.msg_name) with
        | "session", method_name
          when not_ends_with method_name "login_with_password" && num > 0 ->
            num - 1
        | _ ->
            num
      in
      match num with 0 -> method_name | _ -> method_name ^ string_of_int num
    in
    let params_in_msg msg =
      if msg.msg_session then
        session_id :: msg.msg_params
      else
        msg.msg_params
    in
    obj.messages
    |> List.rev_map (fun msg ->
           let of_message (num, params) =
             let base_assoc_list =
               [
                 ("method_name", `String msg.msg_name)
               ; ("class_name", `String obj.name)
               ; ("class_name_exported", `String (snake_to_camel obj.name))
               ; ( "method_name_exported"
                 , `String (method_name_exported obj msg num)
                 )
               ; ("description", desc_of_msg msg ctor_fields)
               ; ("result", of_result obj msg)
               ; ("params", params)
               ; ("errors", of_errors msg.msg_errors)
               ; ("has_error", `Bool (msg.msg_errors <> []))
               ; ("async", `Bool msg.msg_async)
               ]
             in
             `O (add_session_info obj.name msg.msg_name @ base_assoc_list)
           in
           params_in_msg msg |> of_param_groups obj.name |> List.map of_message
       )
    |> List.concat

  let xenapi objs =
    List.map
      (fun obj ->
        let obj_name = snake_to_camel obj.name in
        let name_internal = String.uncapitalize_ascii obj_name in
        let fields = Datamodel_utils.fields_of_obj obj in
        let types = List.map (fun field -> field.ty) fields in
        let modules =
          match obj.messages with [] -> `Null | _ -> modules_of_types types
        in
        let base_assoc_list =
          [
            ("name", `String obj_name)
          ; ("name_internal", `String name_internal)
          ; ("description", `String (String.trim obj.description))
          ; ( "fields"
            , `A (get_event_snapshot obj.name @ List.map of_field fields)
            )
          ; ("modules", modules)
          ; ("messages", `A (messages_of_obj obj))
          ]
        in
        let assoc_list = base_assoc_list @ get_event_session_value obj.name in
        (String.lowercase_ascii obj.name, `O assoc_list)
      )
      objs

  let of_api_message_or_error info =
    let snake_to_camel (s : string) : string =
      String.split_on_char '_' s
      |> List.map (fun seg ->
             let lower = String.lowercase_ascii seg in
             match lower with
             | "vm"
             | "cpu"
             | "tls"
             | "xml"
             | "url"
             | "id"
             | "uuid"
             | "ip"
             | "api"
             | "eof" ->
                 String.uppercase_ascii lower
             | _ ->
                 String.capitalize_ascii lower
         )
      |> String.concat ""
    in
    `O [("name", `String (snake_to_camel info)); ("value", `String info)]

  let api_messages =
    List.map (fun (msg, _) -> of_api_message_or_error msg) !Api_messages.msgList

  let api_errors = List.map of_api_message_or_error !Api_errors.errors
end
