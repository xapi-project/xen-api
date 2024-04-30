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
module StringSet = Set.Make (String)

let templates_dir = "templates"

let ( // ) = Filename.concat

let acronyms =
  [
    "id"
  ; "ip"
  ; "vm"
  ; "api"
  ; "uuid"
  ; "cpu"
  ; "tls"
  ; "https"
  ; "url"
  ; "db"
  ; "xml"
  ; "eof"
  ]
  |> StringSet.of_list

let is_acronym word = StringSet.mem word acronyms

let snake_to_camel (s : string) : string =
  Astring.String.cuts ~sep:"_" s
  |> List.concat_map (fun s -> Astring.String.cuts ~sep:"-" s)
  |> List.map (function
       | s when is_acronym s ->
           String.uppercase_ascii s
       | s ->
           String.capitalize_ascii s
       )
  |> String.concat ""

let records =
  List.map
    (fun obj ->
      let obj_name = snake_to_camel obj.name ^ "Record" in
      (obj_name, Datamodel_utils.fields_of_obj obj)
    )
    objects

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
  open Xapi_stdext_std

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
        let _, e = string_of_ty_with_enums ty in
        let name = suffix_of_type ty in
        ("Option" ^ name, e)

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
    let items = List.concat_map modules_of_type types |> List.append common in
    `O [("import", `Bool true); ("items", `A items)]

  let all_enums objs =
    let enums =
      Datamodel_utils.Types.of_objects objs
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

  let group_params msg =
    let published_release releases =
      match (published_release_for_param releases, releases) with
      | "", [rel] ->
          rel
      | rel, _ ->
          rel
    in
    let do_group params =
      let params =
        List.map
          (fun p -> (published_release p.param_release.internal, p))
          params
      in
      let uniq_sorted_rels =
        List.map fst params
        |> Listext.List.setify
        |> List.fast_sort compare_versions
      in
      List.map
        (fun rel ->
          let params =
            List.filter_map
              (fun (r, param) ->
                match compare_versions r rel with
                | n when n > 0 ->
                    None
                | _ ->
                    Some param
              )
              params
          in
          (params, rel)
        )
        uniq_sorted_rels
    in
    let only_session_group =
      [([session_id], published_release msg.msg_release.internal)]
    in
    let groups =
      match (msg.msg_session, do_group msg.msg_params) with
      | true, [] ->
          only_session_group
      | true, groups ->
          List.map (fun (params, rel) -> (session_id :: params, rel)) groups
      | false, groups ->
          groups
    in
    (* The bool label in the tuple below is tagged to distinguish whether it is the latest parameters group.
       If it's the latest parameters group, then we should not add the number of parameters to the method's name.
    *)
    match List.rev groups with
    | (params, rel) :: _ as groups ->
        (true, params, rel)
        :: List.map (fun (params, rel) -> (false, params, rel)) groups
    | [] ->
        failwith "Empty params group should not exist."

  let of_param param =
    let name_internal name =
      let name = name |> snake_to_camel |> String.uncapitalize_ascii in
      match name with "type" -> "typeKey" | "interface" -> "inter" | _ -> name
    in
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

  (* We use ',' to seprate params in Go function, we should ignore ',' before first param,
     for example `func(a type1, b type2)` is wanted rather than `func(, a type1, b type2)`.
  *)
  let of_params = function
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

  let of_error e = `O [("name", `String e.err_name); ("doc", `String e.err_doc)]

  let of_errors = function
    | [] ->
        `Null
    | errors ->
        `A (List.map of_error errors)

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

  let method_name_exported method_name params latest =
    let method_name = snake_to_camel method_name in
    if latest then
      method_name
    else
      method_name ^ string_of_int (List.length params)

  (* Since the param of `session *Session` isn't needed in functions of session object,
     we add a special "func_params" field for session object to ignore `session *Session`.*)
  let addtion_info_of_session method_name params latest =
    let add_session_info method_name =
      match method_name with
      | "login_with_password" | "slave_local_login_with_password" ->
          [("session_login", `Bool true); ("session_logout", `Bool false)]
      | "logout" | "local_logout" ->
          [("session_login", `Bool false); ("session_logout", `Bool true)]
      | _ ->
          [("session_login", `Bool false); ("session_logout", `Bool false)]
    in
    let name = method_name_exported method_name params latest in
    ("func_params", `A (of_params params))
    :: ("method_name_exported", `String name)
    :: add_session_info method_name

  let addtion_info msg params latest =
    let method_name = msg.msg_name in
    match (String.lowercase_ascii msg.msg_obj_name, msg.msg_session) with
    | "session", true ->
        addtion_info_of_session method_name (List.tl params) latest
    | "session", false ->
        addtion_info_of_session method_name params latest
    | _ ->
        let name = method_name_exported method_name params latest in
        [("method_name_exported", `String name)]

  let messages_of_obj obj =
    let ctor_fields = ctor_fields_of_obj obj in
    obj.messages
    |> List.rev_map (fun msg ->
           let of_message (latest, params, rel_version) =
             let base_assoc_list =
               [
                 ("method_name", `String msg.msg_name)
               ; ("class_name", `String obj.name)
               ; ("class_name_exported", `String (snake_to_camel obj.name))
               ; ("description", desc_of_msg msg ctor_fields)
               ; ("result", of_result obj msg)
               ; ("params", `A (of_params params))
               ; ("errors", of_errors msg.msg_errors)
               ; ("has_error", `Bool (msg.msg_errors <> []))
               ; ("async", `Bool msg.msg_async)
               ; ("version", `String rel_version)
               ]
             in
             `O (base_assoc_list @ addtion_info msg params latest)
           in
           msg |> group_params |> List.map of_message
       )
    |> List.concat

  let of_option ty =
    let name, _ = string_of_ty_with_enums ty in
    `O
      [
        ("type", `String name); ("type_name_suffix", `String (suffix_of_type ty))
      ]

  let of_options types =
    types
    |> List.filter_map (function Option ty -> Some ty | _ -> None)
    |> List.map of_option

  let of_option ty =
    let name, _ = string_of_ty_with_enums ty in
    `O
      [
        ("type", `String name); ("type_name_suffix", `String (suffix_of_type ty))
      ]

  let of_options types =
    types
    |> List.filter_map (function Option ty -> Some ty | _ -> None)
    |> List.map of_option

  let xenapi objs =
    List.map
      (fun obj ->
        let obj_name = snake_to_camel obj.name in
        let name_internal = String.uncapitalize_ascii obj_name in
        let fields = Datamodel_utils.fields_of_obj obj in
        let types =
          List.map (fun field -> field.ty) fields |> Listext.List.setify
        in
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
          ; ("option", `A (of_options types))
          ]
        in
        let assoc_list = base_assoc_list @ get_event_session_value obj.name in
        (String.lowercase_ascii obj.name, `O assoc_list)
      )
      objs

  let of_api_message_or_error info =
    let xapi_constants_renaming (s : string) : string =
      String.split_on_char '_' s
      |> List.map (fun seg ->
             let lower = String.lowercase_ascii seg in
             match lower with
             | s when is_acronym s ->
                 String.uppercase_ascii lower
             | _ ->
                 String.capitalize_ascii lower
         )
      |> String.concat ""
    in
    `O
      [
        ("name", `String (xapi_constants_renaming info)); ("value", `String info)
      ]

  let api_messages =
    List.map (fun (msg, _) -> of_api_message_or_error msg) !Api_messages.msgList

  let api_errors = List.map of_api_message_or_error !Api_errors.errors
end

module Convert = struct
  type params = {func_suffix: string; value_ty: string}

  type params_of_option = {func_suffix: string}

  type params_of_set = {
      func_suffix: string
    ; value_ty: string
    ; item_fp_type: string
  }

  type params_of_record_field = {
      name: string
    ; name_internal: string
    ; name_exported: string
    ; func_suffix: string
    ; type_option: bool
  }

  type params_of_record = {
      func_suffix: string
    ; value_ty: string
    ; fields: params_of_record_field list
  }

  type params_of_enum_item = {value: string; name: string}

  type params_of_enum = {
      func_suffix: string
    ; value_ty: string
    ; items: params_of_enum_item list
  }

  type params_of_map = {
      func_suffix: string
    ; value_ty: string
    ; key_ty: string
    ; val_ty: string
  }

  type convert_params =
    | Simple of params
    | Int of params
    | Float of params
    | Time of params
    | Ref of params
    | Option of params_of_option
    | Set of params_of_set
    | Enum of params_of_enum
    | Record of params_of_record
    | Map of params_of_map

  let template_of_convert : convert_params -> string = function
    | Simple _ ->
        "ConvertSimpleType.mustache"
    | Int _ ->
        "ConvertInt.mustache"
    | Float _ ->
        "ConvertFloat.mustache"
    | Time _ ->
        "ConvertTime.mustache"
    | Ref _ ->
        "ConvertRef.mustache"
    | Set _ ->
        "ConvertSet.mustache"
    | Record _ ->
        "ConvertRecord.mustache"
    | Map _ ->
        "ConvertMap.mustache"
    | Enum _ ->
        "ConvertEnum.mustache"
    | Option _ ->
        "ConvertOption.mustache"

  let to_json : convert_params -> Mustache.Json.value = function
    | Simple params | Int params | Float params | Time params | Ref params ->
        `O
          [
            ("func_name_suffix", `String params.func_suffix)
          ; ("type", `String params.value_ty)
          ]
    | Option params ->
        `O [("func_name_suffix", `String params.func_suffix)]
    | Set params ->
        `O
          [
            ("func_name_suffix", `String params.func_suffix)
          ; ("type", `String params.value_ty)
          ; ("item_func_suffix", `String params.item_fp_type)
          ]
    | Record params ->
        let fields =
          List.rev_map
            (fun (field : params_of_record_field) ->
              `O
                [
                  ("name", `String field.name)
                ; ("name_internal", `String field.name_internal)
                ; ("name_exported", `String field.name_exported)
                ; ("func_name_suffix", `String field.func_suffix)
                ; ("type_option", `Bool field.type_option)
                ]
            )
            params.fields
        in
        `O
          [
            ("func_name_suffix", `String params.func_suffix)
          ; ("type", `String params.value_ty)
          ; ("fields", `A fields)
          ]
    | Enum params ->
        let of_value item =
          `O [("value", `String item.value); ("name", `String item.name)]
        in
        `O
          [
            ("type", `String params.value_ty)
          ; ("func_name_suffix", `String params.func_suffix)
          ; ("items", `A (List.map of_value params.items))
          ]
    | Map params ->
        `O
          [
            ("func_name_suffix", `String params.func_suffix)
          ; ("type", `String params.value_ty)
          ; ("key_type", `String params.key_ty)
          ; ("value_type", `String params.val_ty)
          ]

  let fields record_name =
    let fields =
      List.assoc_opt record_name records
      |> Option.value ~default:[]
      |> List.rev_map (fun field ->
             ( String.concat "_" field.full_name
             , Json.suffix_of_type field.ty
             , match field.ty with Option _ -> true | _ -> false
             )
         )
    in
    if record_name = "EventRecord" then
      ("snapshot", "RecordInterface", false) :: fields
    else
      fields

  let of_ty = function
    | SecretString | String ->
        Simple {func_suffix= "String"; value_ty= "string"}
    | Int ->
        Int {func_suffix= "Int"; value_ty= "int"}
    | Float ->
        Float {func_suffix= "Float"; value_ty= "float64"}
    | Bool ->
        Simple {func_suffix= "Bool"; value_ty= "bool"}
    | DateTime ->
        Time {func_suffix= "Time"; value_ty= "time.Time"}
    | Enum (name, kv) as ty ->
        let name = snake_to_camel name in
        let items =
          List.map (fun (k, _) -> {value= k; name= name ^ snake_to_camel k}) kv
        in
        Enum {func_suffix= Json.suffix_of_type ty; value_ty= name; items}
    | Set ty as set ->
        let fp_ty = Json.suffix_of_type ty in
        let ty, _ = Json.string_of_ty_with_enums ty in
        Set
          {
            func_suffix= Json.suffix_of_type set
          ; value_ty= ty
          ; item_fp_type= fp_ty
          }
    | Map (ty1, ty2) as ty ->
        let name, _ = Json.string_of_ty_with_enums ty in
        Map
          {
            func_suffix= Json.suffix_of_type ty
          ; value_ty= name
          ; key_ty= Json.suffix_of_type ty1
          ; val_ty= Json.suffix_of_type ty2
          }
    | Ref _ as ty ->
        let name = Json.suffix_of_type ty in
        Ref {func_suffix= name; value_ty= name}
    | Record r ->
        let name = snake_to_camel r ^ "Record" in
        let fields =
          List.map
            (fun (name, func_suffix, is_option_type) ->
              let camel_name = snake_to_camel name in
              {
                name
              ; name_internal= String.uncapitalize_ascii camel_name
              ; name_exported= camel_name
              ; func_suffix
              ; type_option= is_option_type
              }
            )
            (fields name)
        in
        Record {func_suffix= name; value_ty= name; fields}
    | Option ty ->
        Option {func_suffix= Json.suffix_of_type ty}

  let of_serialize params =
    `O [("serialize", `A [to_json params]); ("deserialize", `Null)]

  let of_deserialize params =
    `O [("serialize", `Null); ("deserialize", `A [to_json params])]

  let event_batch : Mustache.Json.t =
    `O
      [
        ( "deserialize"
        , `A
            [
              `O
                [
                  ("func_name_suffix", `String "EventBatch")
                ; ("type", `String "EventBatch")
                ; ( "elements"
                  , `A
                      [
                        `O
                          [
                            ("name", `String "token")
                          ; ("name_internal", `String "token")
                          ; ("name_exported", `String "Token")
                          ; ("func_name_suffix", `String "String")
                          ]
                      ; `O
                          [
                            ("name", `String "validRefCounts")
                          ; ("name_internal", `String "validRefCounts")
                          ; ("name_exported", `String "ValidRefCounts")
                          ; ("func_name_suffix", `String "StringToIntMap")
                          ]
                      ; `O
                          [
                            ("name", `String "events")
                          ; ("name_internal", `String "events")
                          ; ("name_exported", `String "Events")
                          ; ("func_name_suffix", `String "EventRecordSet")
                          ]
                      ]
                  )
                ]
            ]
        )
      ]

  let interface : Mustache.Json.t =
    `O
      [
        ( "deserialize"
        , `A
            [
              `O
                [
                  ("func_name_suffix", `String "RecordInterface")
                ; ("type", `String "RecordInterface")
                ]
            ]
        )
      ]
end
