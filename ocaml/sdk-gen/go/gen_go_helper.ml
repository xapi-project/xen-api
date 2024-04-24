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
  let renndered = Mustache.render ~strict:true templ json in
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

  let rec suffix_of_type ty =
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
        suffix_of_type ty ^ "Set"
    | Map (ty1, ty2) ->
        let k_suffix = suffix_of_type ty1 in
        let v_suffix = suffix_of_type ty2 in
        k_suffix ^ "To" ^ v_suffix ^ "Map"
    | Ref r ->
        snake_to_camel r ^ "Ref"
    | Record r ->
        snake_to_camel r ^ "Record"
    | Option ty ->
        suffix_of_type ty

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
            ; ("func_name_suffix", `String "EventBatch")
            ]
        else
          let t', _ = string_of_ty_with_enums t in
          `O
            [
              ("type", `String t')
            ; ("func_name_suffix", `String (suffix_of_type t))
            ]

  let of_params params =
    let name_internal name =
      let name = name |> snake_to_camel |> String.uncapitalize_ascii in
      match name with "type" -> "typeKey" | "interface" -> "inter" | _ -> name
    in
    let of_param param =
      let suffix_of_type = suffix_of_type param.param_type in
      let t, _e = string_of_ty_with_enums param.param_type in
      let name = param.param_name in
      [
        ("is_session_id", `Bool (name = "session_id"))
      ; ("type", `String t)
      ; ("name", `String name)
      ; ("name_internal", `String (name_internal name))
      ; ("doc", `String param.param_doc)
      ; ("func_name_suffix", `String suffix_of_type)
      ]
    in
    (* We use ',' to seprate params in Go function, we should ignore ',' before first param,
       for example `func(a type1, b type2)` is wanted rather than `func(, a type1, b type2)`.
    *)
    let add_first = function
      | head :: rest ->
          let head = `O (("first", `Bool true) :: of_param head) in
          let rest =
            List.map
              (fun item -> `O (("first", `Bool false) :: of_param item))
              rest
          in
          head :: rest
      | [] ->
          []
    in
    `A (add_first params)

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
        [("session_login", `Bool true); ("session_logout", `Bool false)]
    | "session", "logout" | "session", "local_logout" ->
        [("session_login", `Bool false); ("session_logout", `Bool true)]
    | _ ->
        [("session_login", `Bool false); ("session_logout", `Bool false)]

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
    let params_in_msg msg =
      if msg.msg_session then
        session_id :: msg.msg_params
      else
        msg.msg_params
    in
    List.map
      (fun msg ->
        let params = params_in_msg msg |> of_params in
        let base_assoc_list =
          [
            ("method_name", `String msg.msg_name)
          ; ("class_name", `String obj.name)
          ; ("class_name_exported", `String (snake_to_camel obj.name))
          ; ("method_name_exported", `String (snake_to_camel msg.msg_name))
          ; ("description", desc_of_msg msg ctor_fields)
          ; ("result", of_result obj msg)
          ; ("params", params)
          ; ("errors", of_errors msg.msg_errors)
          ; ("has_error", `Bool (msg.msg_errors <> []))
          ; ("async", `Bool msg.msg_async)
          ]
        in
        (* Since the param of `session *Session` isn't needed in functions of session object,
           we add a special "func_params" field for session object to ignore `session *Session`.*)
        if obj.name = "session" then
          `O
            (("func_params", msg.msg_params |> of_params)
            :: (add_session_info obj.name msg.msg_name @ base_assoc_list)
            )
        else
          `O base_assoc_list
      )
      obj.messages

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

  let api_messages =
    List.map (fun (msg, _) -> `O [("name", `String msg)]) !Api_messages.msgList

  let api_errors =
    List.map (fun error -> `O [("name", `String error)]) !Api_errors.errors
end
