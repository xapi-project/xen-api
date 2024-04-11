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

let templates_dir = "templates"

let ( // ) = Filename.concat

let snake_to_camel (s : string) : string =
  Astring.String.cuts ~sep:"_" s
  |> List.map (fun s -> Astring.String.cuts ~sep:"-" s)
  |> List.concat
  |> List.map String.capitalize_ascii
  |> String.concat ""

let render_template template_file json =
  let templ =
    string_of_file (templates_dir // template_file) |> Mustache.of_string
  in
  Mustache.render templ json

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

  let fields_of_obj_with_enums obj =
    let rec flatten_contents contents =
      List.fold_left
        (fun l -> function
          | Field f ->
              f :: l
          | Namespace (_name, contents) ->
              flatten_contents contents @ l
        )
        [] contents
    in
    let fields = flatten_contents obj.contents in
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
    List.fold_left
      (fun (fields, enums, has_time_type) field ->
        let is_time_type =
          match field.ty with DateTime -> true | _ -> false
        in
        let ty, e = string_of_ty_with_enums field.ty in
        ( `O
            [
              ("name", `String (concat_and_convert field))
            ; ("description", `String (String.trim field.field_description))
            ; ("type", `String ty)
            ]
          :: fields
        , StringMap.union choose_enum enums e
        , if is_time_type then true else has_time_type
        )
      )
      ([], StringMap.empty, false)
      fields

  let enums_from_result obj msg =
    match msg.msg_result with
    | None ->
        StringMap.empty
    | Some (t, _d) ->
        if obj.name = "event" && String.lowercase_ascii msg.msg_name = "from"
        then
          StringMap.empty
        else
          let _, enums = string_of_ty_with_enums t in
          enums

  let enums_from_params ps =
    List.fold_left
      (fun enums p ->
        let _t, e = string_of_ty_with_enums p.param_type in
        StringMap.union choose_enum enums e
      )
      StringMap.empty ps

  let enums_in_messages_of_obj obj =
    List.fold_left
      (fun enums msg ->
        let params =
          if msg.msg_session then
            session_id :: msg.msg_params
          else
            msg.msg_params
        in
        let enums1 = enums_from_result obj msg in
        let enums2 = enums_from_params params in
        enums
        |> StringMap.union choose_enum enums1
        |> StringMap.union choose_enum enums2
      )
      StringMap.empty obj.messages

  let get_modules messages has_time_type =
    match messages with
    | [] ->
        `Null
    | _ ->
        let items =
          match has_time_type with
          | true ->
              [
                `O [("name", `String "fmt"); ("sname", `Null)]
              ; `O [("name", `String "time"); ("sname", `Null)]
              ]
          | false ->
              [`O [("name", `String "fmt"); ("sname", `Null)]]
        in
        `O [("import", `Bool true); ("items", `A items)]

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

  let xenapi objs =
    let objs, enums =
      List.fold_left
        (fun (objs_acc, enums_acc) obj ->
          let fields, enums_in_fields, has_time_type =
            fields_of_obj_with_enums obj
          in
          let enums_in_msgs = enums_in_messages_of_obj obj in
          let enums =
            enums_acc
            |> StringMap.union choose_enum enums_in_msgs
            |> StringMap.union choose_enum enums_in_fields
          in
          let obj_name = snake_to_camel obj.name in
          let modules = get_modules obj.messages has_time_type in
          let base_assoc_list =
            [
              ("name", `String obj_name)
            ; ("description", `String (String.trim obj.description))
            ; ("fields", `A (get_event_snapshot obj.name @ fields))
            ; ("modules", modules)
            ]
          in
          let assoc_list = get_event_session_value obj.name @ base_assoc_list in
          ((String.lowercase_ascii obj.name, `O assoc_list) :: objs_acc, enums)
        )
        ([], StringMap.empty) objs
    in
    let enums =
      `O
        [
          ( "enums"
          , `A (StringMap.fold (fun k v acc -> of_enum k v :: acc) enums [])
          )
        ]
    in
    (objs, enums)

  let api_messages =
    List.map (fun (msg, _) -> `O [("name", `String msg)]) !Api_messages.msgList

  let api_errors =
    List.map (fun error -> `O [("name", `String error)]) !Api_errors.errors
end
