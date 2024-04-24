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

open Datamodel_types
open Datamodel_utils
open Dm_api

let ( // ) = Filename.concat

let write_string ~path str =
  Xapi_stdext_unix.Unixext.write_string_to_file path str

let destdir' = ref "."

let parse_args () =
  Arg.parse
    [
      ( "-destdir"
      , Arg.Set_string destdir'
      , "the destination directory for the generated files"
      )
    ]
    (fun x -> Printf.printf "Ignoring anonymous argument %s" x)
    "Generates documentation for the datamodel classes. See -help."

(* Datamodel *)

module Json : sig
  val xenapi : Datamodel_types.obj list -> Yojson.Safe.t

  val release_info :
    api_release list -> Datamodel_types.obj list -> Yojson.Safe.t
end = struct
  let rec string_of_ty_with_enums ty =
    match ty with
    | SecretString | String ->
        ("string", [])
    | Int ->
        ("int", [])
    | Float ->
        ("float", [])
    | Bool ->
        ("bool", [])
    | DateTime ->
        ("datetime", [])
    | Enum (name, kv) ->
        ("enum " ^ name, [(name, kv)])
    | Set ty ->
        let s, e = string_of_ty_with_enums ty in
        (s ^ " set", e)
    | Map (ty1, ty2) ->
        let s1, e1 = string_of_ty_with_enums ty1 in
        let s2, e2 = string_of_ty_with_enums ty2 in
        (Printf.sprintf "(%s -> %s) map" s1 s2, e1 @ e2)
    | Ref r ->
        (r ^ " ref", [])
    | Record r ->
        (r ^ " record", [])
    | Option ty ->
        let s, e = string_of_ty_with_enums ty in
        (s ^ " option", e)

  let string_of_qualifier = function
    | RW ->
        "RW"
    | StaticRO ->
        "RO/constructor"
    | DynamicRO ->
        "RO/runtime"

  let rec string_of_default = function
    | VString x ->
        "\"" ^ x ^ "\""
    | VInt x ->
        Int64.to_string x
    | VFloat x ->
        string_of_float x
    | VBool x ->
        string_of_bool x
    | VDateTime x ->
        Date.to_rfc3339 x
    | VEnum x ->
        x
    | VMap x ->
        Printf.sprintf "{%s}"
          (String.concat ", "
             (List.map
                (fun (a, b) ->
                  Printf.sprintf "%s -> %s" (string_of_default a)
                    (string_of_default b)
                )
                x
             )
          )
    | VSet x ->
        Printf.sprintf "{%s}" (String.concat ", " (List.map string_of_default x))
    | VRef x ->
        if x = "" then "Null" else x

  let of_lifecycle lc =
    `Assoc
      [
        ("state", `String Lifecycle.(string_of_state lc.state))
      ; ( "transitions"
        , `List
            (List.map
               (fun (t, r, d) ->
                 `Assoc
                   [
                     ( "transition"
                     , `String
                         (String.lowercase_ascii (Lifecycle.string_of_change t))
                     )
                   ; ("release", `String r)
                   ; ("description", `String d)
                   ]
               )
               lc.transitions
            )
        )
      ]

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
    List.fold_left
      (fun (fields, enums) field ->
        let ty, e = string_of_ty_with_enums field.ty in
        ( `Assoc
            (("name", `String (String.concat "_" field.full_name))
            :: ("description", `String field.field_description)
            :: ("type", `String ty)
            :: ("qualifier", `String (string_of_qualifier field.qualifier))
            :: ( "tag"
               , `String
                   ( match field.field_doc_tags with
                   | [] ->
                       ""
                   | t :: _ ->
                       string_of_doc_tag t
                   )
               )
            :: ("lifecycle", of_lifecycle field.lifecycle)
            ::
            ( match field.default_value with
            | Some d ->
                [("default", `String (string_of_default d))]
            | None ->
                []
            )
            )
          :: fields
        , enums @ e
        )
      )
      ([], []) fields

  let of_result obj msg =
    match msg.msg_result with
    | None ->
        (`List [`String "void"], [])
    | Some (t, d) ->
        if obj.name = "event" && String.lowercase_ascii msg.msg_name = "from"
        then
          (`List [`String "an event batch"; `String d], [])
        else
          let t', enums = string_of_ty_with_enums t in
          (`List [`String t'; `String d], enums)

  let of_params ps =
    let params, enums =
      List.fold_left
        (fun (params, enums) p ->
          let t, e = string_of_ty_with_enums p.param_type in
          ( `Assoc
              [
                ("type", `String t)
              ; ("name", `String p.param_name)
              ; ("doc", `String p.param_doc)
              ; ("required", `Bool (Option.is_none p.param_default))
              ]
            :: params
          , enums @ e
          )
        )
        ([], []) ps
    in
    (`List (List.rev params), enums)

  let of_error e =
    `Assoc [("name", `String e.err_name); ("doc", `String e.err_doc)]

  let of_roles = function
    | None ->
        `List []
    | Some rs ->
        `List (List.map (fun s -> `String s) rs)

  let session_id =
    {
      param_type= Ref Datamodel_common._session
    ; param_name= "session_id"
    ; param_doc= "Reference to a valid session"
    ; param_release= Datamodel_common.rio_release
    ; param_default= None
    }

  let messages_of_obj_with_enums obj =
    List.fold_left
      (fun (msgs, enums) msg ->
        let params =
          if msg.msg_session then
            session_id :: msg.msg_params
          else
            msg.msg_params
        in
        let ctor =
          if msg.msg_tag = FromObject Make then
            let ctor_fields =
              List.filter
                (function {qualifier= StaticRO | RW; _} -> true | _ -> false)
                (fields_of_obj obj)
              |> List.map (fun f ->
                     String.concat "_" f.full_name
                     ^ if f.default_value = None then "*" else ""
                 )
            in
            Printf.sprintf "\nThe constructor args are: %s (* = non-optional)."
              (String.concat ", " ctor_fields)
          else
            ""
        in
        let result, enums1 = of_result obj msg in
        let params, enums2 = of_params params in
        ( `Assoc
            [
              ("name", `String msg.msg_name)
            ; ("description", `String (msg.msg_doc ^ ctor))
            ; ("result", result)
            ; ("params", params)
            ; ("errors", `List (List.map of_error msg.msg_errors))
            ; ("roles", of_roles msg.msg_allowed_roles)
            ; ( "tag"
              , `String
                  ( match msg.msg_doc_tags with
                  | [] ->
                      ""
                  | t :: _ ->
                      string_of_doc_tag t
                  )
              )
            ; ("lifecycle", of_lifecycle msg.msg_lifecycle)
            ; ("implicit", `Bool (msg.msg_tag <> Custom))
            ]
          :: msgs
        , enums @ enums1 @ enums2
        )
      )
      ([], []) obj.messages

  let of_enum (name, vs) =
    let of_value (v, d) = `Assoc [("name", `String v); ("doc", `String d)] in
    `Assoc [("name", `String name); ("values", `List (List.map of_value vs))]

  let xenapi objs =
    `List
      (List.map
         (fun obj ->
           let fields, enums1 = fields_of_obj_with_enums obj in
           let messages, enums2 = messages_of_obj_with_enums obj in
           let enums = Xapi_stdext_std.Listext.List.setify (enums1 @ enums2) in
           let event_snapshot =
             if String.lowercase_ascii obj.name = "event" then
               [
                 `Assoc
                   [
                     ("name", `String "snapshot")
                   ; ( "description"
                     , `String
                         "The record of the database object that was added, \
                          changed or deleted"
                     )
                   ; ("type", `String "<class> record")
                   ; ("qualifier", `String (string_of_qualifier DynamicRO))
                   ; ("tag", `String "")
                   ; ( "lifecycle"
                     , of_lifecycle
                         (Lifecycle.from [(Published, rel_boston, "")])
                     )
                   ]
               ]
             else
               []
           in
           `Assoc
             [
               ("name", `String obj.name)
             ; ("description", `String obj.description)
             ; ("fields", `List (event_snapshot @ fields))
             ; ("messages", `List messages)
             ; ("enums", `List (List.map of_enum enums))
             ; ("lifecycle", of_lifecycle obj.obj_lifecycle)
             ; ( "tag"
               , `String
                   ( match obj.obj_doc_tags with
                   | [] ->
                       ""
                   | t :: _ ->
                       string_of_doc_tag t
                   )
               )
             ]
         )
         objs
      )

  let of_change (t, n, l, s) =
    `Assoc
      [
        ( "transition"
        , `String
            (String.lowercase_ascii (Lifecycle.string_of_change t) ^ " " ^ s)
        )
      ; ("name", `String n)
      ; ("log", `String l)
      ]

  let compare_changes (a_t, a_n, _, a_k) (b_t, b_n, _, b_k) =
    let int_of_transition = function
      | Lifecycle.Prototyped ->
          -10
      | Published ->
          0
      | Extended ->
          10
      | Changed ->
          20
      | Deprecated ->
          30
      | Removed ->
          40
    in
    let int_of_kind = function
      | "class" ->
          0
      | "field" ->
          1
      | "message" ->
          2
      | _ ->
          3
    in
    let cmp =
      compare
        (int_of_transition a_t + int_of_kind a_k)
        (int_of_transition b_t + int_of_kind b_k)
    in
    if cmp = 0 then
      compare a_n b_n
    else
      cmp

  let list_dedup cmp =
    let rec loop acc = function
      | [] ->
          List.rev acc
      | [item] ->
          loop (item :: acc) []
      | a :: b :: rest when cmp a b = 0 ->
          loop (a :: acc) rest
      | a :: rest ->
          loop (a :: acc) rest
    in
    loop []

  let remove_outdated changes =
    (* When several lifecycles transitions for the same entity, keep the most
       latest change one and drop the rest *)
    changes
    |> List.sort (fun ((_, nam_a, _, _) as a) ((_, nam_b, _, _) as b) ->
           let cmp = String.compare nam_a nam_b in
           if cmp <> 0 then
             cmp
           else
             -compare_changes a b
       )
    |> list_dedup (fun (_, a, _, _) (_, b, _, _) -> String.compare a b)

  let release_info releases objs =
    let changes_in_release rel =
      let search_obj obj =
        let changes =
          List.filter
            (fun (_, release, _) -> release = code_name_of_release rel)
            obj.obj_lifecycle.transitions
        in
        let obj_changes =
          List.map
            (fun (transition, _release, doc) ->
              ( transition
              , obj.name
              , ( if doc = "" && transition = Lifecycle.Published then
                    obj.description
                  else
                    doc
                )
              , "class"
              )
            )
            changes
          |> remove_outdated
        in
        let changes_for_msg m =
          let changes =
            List.filter
              (fun (_transition, release, _doc) ->
                release = code_name_of_release rel
              )
              m.msg_lifecycle.transitions
          in
          List.map
            (fun (transition, _release, doc) ->
              ( transition
              , obj.name ^ "." ^ m.msg_name
              , ( if doc = "" && transition = Lifecycle.Published then
                    m.msg_doc
                  else
                    doc
                )
              , "message"
              )
            )
            changes
          |> remove_outdated
        in
        (* Don't include implicit messages *)
        let msgs = List.filter (fun m -> m.msg_tag = Custom) obj.messages in
        let msg_changes =
          List.fold_left (fun l m -> l @ changes_for_msg m) [] msgs
        in
        let changes_for_field f =
          let changes =
            List.filter
              (fun (_transition, release, _doc) ->
                release = code_name_of_release rel
              )
              f.lifecycle.transitions
          in
          let field_name = String.concat "_" f.full_name in
          List.map
            (fun (transition, _release, doc) ->
              ( transition
              , obj.name ^ "." ^ field_name
              , ( if doc = "" && transition = Lifecycle.Published then
                    f.field_description
                  else
                    doc
                )
              , "field"
              )
            )
            changes
          |> remove_outdated
        in
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
        let field_changes =
          List.fold_left (fun l f -> l @ changes_for_field f) [] fields
        in
        let event_snapshot_change =
          if obj.name = "event" && rel.code_name = Some rel_boston then
            [
              ( Lifecycle.Published
              , "event.snapshot"
              , "The record of the database object that was added, changed or \
                 deleted"
              , "field"
              )
            ]
          else
            []
        in
        obj_changes @ event_snapshot_change @ field_changes @ msg_changes
      in
      `List
        (List.concat_map search_obj objs
        |> List.sort compare_changes
        |> List.map of_change
        )
    in
    `Assoc
      (List.filter_map
         (fun rel ->
           if rel.code_name <> None then
             Some (code_name_of_release rel, changes_in_release rel)
           else
             None
         )
         releases
      )
end

module Yaml = struct
  let release = function
    | {code_name= Some x; branding; release_date= Some _; _} ->
        Printf.sprintf "%s: %s\n" x branding
    | _ ->
        ""
end

type file = {filename: string; contents: string}

module Md = struct
  let release = function
    | {release_date= None; _} ->
        None
    | {code_name= Some x; release_date= y; _} ->
        let contents =
          String.concat "\n"
            [
              "---"
            ; "layout: xenapi-release"
            ; Printf.sprintf "release: %s" x
            ; "release_index: true"
            ; "---\n"
            ; ( match y with
              | Some "" ->
                  ""
              | Some z ->
                  Printf.sprintf "Released in %s.\n" z
              | _ ->
                  ""
              )
            ]
        in
        Some {filename= Printf.sprintf "%s.md" x; contents}
    | _ ->
        None

  let cls {name; _} =
    let filename = Printf.sprintf "%s.md" (String.lowercase_ascii name) in
    let contents =
      String.concat "\n"
        [
          "---"
        ; "layout: xenapi-class"
        ; Printf.sprintf "class: %s" name
        ; "class_index: true"
        ; "---\n"
        ]
    in
    Some {filename; contents}
end

let write_to_dir dir maybe_file =
  let write {filename; contents} =
    write_string ~path:(dir // filename) contents
  in
  Option.iter write maybe_file

module Version = struct
  (* versions ending in -next are considered unreleased *)
  type t = int list * string option

  (* orders like this:
     - 22.10.0
     - 22.11.0
     - 22.11.0-next
     - 22.12.0
     From the way the lifecycle binary generates versions there only will be a
     single -next version, with the highest version number
  *)
  let compare (xs, x_next) (ys, y_next) =
    let cmp = List.compare Int.compare xs ys in
    if cmp = 0 then
      Option.compare String.compare x_next y_next
    else
      cmp

  let of_name name =
    let of_chunks mj mn mc tag =
      ([mj; mn; mc], if tag = "" then None else Some tag)
    in
    try Scanf.sscanf name "%d.%d.%d%s" of_chunks
    with _ ->
      failwith
        (Printf.sprintf "Version schema changed, please change this code %s: %s"
           name __LOC__
        )

  let to_name_date (lst, str) =
    ( Fmt.(str "%a" (list ~sep:(Fmt.any ".") int)) lst
    , if Option.is_none str then Some "" else None
    )
end

module NameSet = Set.Make (String)
module VersionSet = Set.Make (Version)

let get_versions_from api =
  let classes = Dm_api.objects_of_api api in
  let rec from_field = function
    | Field fld ->
        fld.lifecycle.transitions
    | Namespace (_, nms) ->
        List.concat_map from_field nms
  in
  let from_message {msg_lifecycle= {transitions; _}; _} = transitions in
  let versions_from_class (cls : obj) =
    let field_lifecycles = List.concat_map from_field cls.contents in
    let message_lifecycles = List.concat_map from_message cls.messages in
    List.concat
      [
        cls.Datamodel_types.obj_lifecycle.transitions
      ; field_lifecycles
      ; message_lifecycles
      ]
  in
  let is_named_release name =
    List.exists
      (fun x ->
        match x.code_name with Some n when n = name -> true | _ -> false
      )
      release_order_full
  in
  (* now gather versions from the lifecycles *)
  let versions =
    List.concat_map versions_from_class classes
    |> List.to_seq
    |> Seq.map (fun (_, version, _) -> version)
    |> NameSet.of_seq
    |> NameSet.filter (fun x -> not (is_named_release x))
    |> NameSet.to_seq
    |> Seq.map Version.of_name
    |> VersionSet.of_seq
    |> VersionSet.elements
  in
  (* now transform the versions to releases, in a free-form way *)
  let release_of_version v =
    let name, release_date = Version.to_name_date v in
    {
      code_name= Some name
    ; version_major= 2
    ; version_minor= 20
    ; release_date
    ; branding= Printf.sprintf "XAPI %s" name
    }
  in
  release_order_full @ List.map release_of_version versions

let () =
  parse_args () ;
  let destdir = !destdir' in
  Xapi_stdext_unix.Unixext.mkdir_rec destdir 0o755 ;
  let data_dir = destdir // "_data" in
  Xapi_stdext_unix.Unixext.mkdir_rec data_dir 0o755 ;
  let api = Datamodel.all_api in
  (* Add all implicit messages *)
  let api = add_implicit_messages api in
  (* Only include messages that are visible to a XenAPI client *)
  let api = filter_by ~message:on_client_side api in
  (* And only messages marked as not hidden from the docs, and non-internal fields *)
  let api =
    filter_by
      ~field:(fun f -> not f.internal_only)
      ~message:(fun m -> not m.msg_hide_from_docs)
      api
  in
  let objs = objects_of_api api in
  let releases = get_versions_from api in
  Yojson.Safe.to_file (data_dir // "xenapi.json") (Json.xenapi objs) ;
  Yojson.Safe.to_file
    (data_dir // "release_info.json")
    (Json.release_info releases objs) ;
  write_string
    ~path:(data_dir // "releases.yml")
    (List.rev_map Yaml.release releases |> String.concat "") ;
  let release_md_dir = destdir // "xen-api/releases" in
  Xapi_stdext_unix.Unixext.mkdir_rec release_md_dir 0o755 ;
  let class_md_dir = destdir // "xen-api/classes" in
  Xapi_stdext_unix.Unixext.mkdir_rec class_md_dir 0o755 ;
  List.iter (fun x -> write_to_dir release_md_dir (Md.release x)) releases ;
  List.iter (fun x -> write_to_dir class_md_dir (Md.cls x)) objs
