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

open Stdext
open Xstringext
open Pervasiveext
open Datamodel
open Datamodel_types

type change_t = lifecycle_change * string * string
and changes_t = change_t list
[@@deriving rpc]

let destdir = ref "."
let templdir = ref ""

let parse_args () =
  Arg.parse [
    "-destdir", Arg.Set_string destdir, "the destination directory for the generated files";
    "-templdir", Arg.Set_string templdir, "the directory with the template (mustache) files";
  ]
    (fun x-> Printf.printf "Ignoring anonymous argument %s" x)
    ("Generates documentation for the datamodel classes. See -help.")


let generate_files api_dir =
  let api = (Datamodel.all_api) in
  let objs = Dm_api.objects_of_api api in
  let create_json obj =
    let name = obj.name in
    let s = Jsonrpc.to_string (rpc_of_obj obj) in
    let fname = name ^ ".json" in
    Stdext.Unixext.write_string_to_file (Filename.concat api_dir fname) ("clsdoc = " ^ s);
    name
  in
  let names = List.map create_json objs in
  let class_list = String.concat ", " (List.map (fun s -> "'" ^ s ^ "'") names) in
  Stdext.Unixext.write_string_to_file (Filename.concat api_dir "index.json") ("classes = [" ^ class_list ^ "]");


  let changes_in_release rel =
    let search_obj obj =
      let changes = List.filter (fun (transition, release, doc) -> release = code_name_of_release rel) obj.obj_lifecycle in
      let obj_changes : changes_t =
        List.map (fun (transition, release, doc) ->
            (transition, obj.name, if doc = "" && transition = Published then obj.description else doc)
          ) changes in

      let changes_for_msg m =
        let changes = List.filter (fun (transition, release, doc) -> release = code_name_of_release rel) m.msg_lifecycle in
        List.map (fun (transition, release, doc) ->
            (transition, m.msg_name, if doc = "" && transition = Published then m.msg_doc else doc)
          ) changes
      in
      let msgs = List.filter (fun m -> not m.msg_hide_from_docs) obj.messages in
      let msg_changes : changes_t = List.fold_left (fun l m -> l @ (changes_for_msg m)) [] msgs in

      let changes_for_field f =
        let changes = List.filter (fun (transition, release, doc) -> release = code_name_of_release rel) f.lifecycle in
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
    let fname = (code_name_of_release rel) ^ ".json" in
    Stdext.Unixext.write_string_to_file (Filename.concat api_dir fname) ("release_info = [" ^ release_info ^ "]")
  in
  List.iter changes_in_release release_order;
  let release_list = String.concat ", " (List.map (fun s -> "'" ^ (code_name_of_release s) ^ "'") release_order) in
  Stdext.Unixext.write_string_to_file (Filename.concat api_dir "releases.json") ("releases = [" ^ release_list ^ "]")

let json_releases =
  let json_of_rel x = `O [
      "code_name", `String (code_name_of_release x);
      "version_major", `Float (float_of_int x.version_major);
      "version_minor", `Float (float_of_int x.version_minor);
      "branding", `String x.branding;
    ]
  in
  `O [ "releases", `A (List.map json_of_rel release_order) ]

let render_template template_file json output_file =
  let templ =  Stdext.Unixext.string_of_file template_file |> Mustache.of_string in
  let rendered = Mustache.render templ json in
  let out_chan = open_out output_file in
  finally (fun () -> output_string out_chan rendered)
    (fun () -> close_out out_chan)


let _ =
  parse_args ();

  let api_dir = Filename.concat !destdir "api" in
  Stdext.Unixext.mkdir_rec api_dir 0o755;

  generate_files api_dir;

  render_template
    (Filename.concat !templdir "branding.mustache")
    json_releases
    (Filename.concat !destdir "branding.js");
