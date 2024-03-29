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
open Datamodel_utils
open Dm_api
open CommonFunctions

let dest_dir = "autogen"

let templates_dir = "templates"

let ( // ) = Filename.concat

let src_dir = dest_dir // "src"

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

let generate_file rendered output_file =
  let out_chan = open_out (src_dir // output_file) in
  Fun.protect
    (fun () -> output_string out_chan rendered)
    ~finally:(fun () -> close_out out_chan)

module Json = struct
  let rec string_of_ty ty =
    match ty with
    | SecretString | String ->
        "string"
    | Int ->
        "int"
    | Float ->
        "float64"
    | Bool ->
        "bool"
    | DateTime ->
        "time.Time"
    | Enum (name, _kv) ->
        snake_to_camel name
    | Set ty ->
        "[]" ^ string_of_ty ty
    | Map (ty1, ty2) ->
        let s1 = string_of_ty ty1 in
        let s2 = string_of_ty ty2 in
        "map[" ^ s1 ^ "]" ^ s2
    | Ref r ->
        snake_to_camel r ^ "Ref"
    | Record r ->
        snake_to_camel r ^ "Record"
    | Option ty ->
        string_of_ty ty

  let fields_of_obj obj =
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
    List.map
      (fun field ->
        let ty = string_of_ty field.ty in
        `O
          [
            ("name", `String (concat_and_convert field))
          ; ("description", `String (String.trim field.field_description))
          ; ("type", `String ty)
          ]
      )
      fields

  let xenapi objs =
    List.map
      (fun obj ->
        let fields = fields_of_obj obj in
        let event_snapshot =
          if String.lowercase_ascii obj.name = "event" then
            [
              `O
                [
                  ("name", `String "Snapshot")
                ; ( "description"
                  , `String
                      "The record of the database object that was added, \
                       changed or deleted"
                  )
                ; ("type", `String "RecordInterface")
                ]
            ]
          else
            []
        in
        let obj_name = snake_to_camel obj.name in
        let event_session_value = function
          | "event" ->
              [("event", `Bool true); ("session", `Null)]
          | "session" ->
              [("event", `Null); ("session", `Bool true)]
          | _ ->
              [("event", `Null); ("session", `Null)]
        in
        let base_assoc_list =
          [
            ("name", `String obj_name)
          ; ("description", `String (String.trim obj.description))
          ; ("fields", `A (event_snapshot @ fields))
          ]
        in
        let assoc_list = event_session_value obj.name @ base_assoc_list in
        (String.lowercase_ascii obj.name, `O assoc_list)
      )
      objs
end

let objects =
  let api = Datamodel.all_api in
  (* Add all implicit messages *)
  let api = add_implicit_messages api in
  (* Only include messages that are visible to a XenAPI client *)
  let api = filter (fun _ -> true) (fun _ -> true) on_client_side api in
  (* And only messages marked as not hidden from the docs, and non-internal fields *)
  let api =
    filter
      (fun _ -> true)
      (fun f -> not f.internal_only)
      (fun m -> not m.msg_hide_from_docs)
      api
  in
  objects_of_api api
