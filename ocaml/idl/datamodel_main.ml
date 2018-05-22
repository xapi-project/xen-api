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
open Datamodel
open Datamodel_types
module DU = Datamodel_utils
open Dm_api

(* right now this file returns all fields if -all set, otherwise it returns
   fields that have z1 set in release.internal *)

let _ =
  let dot_mode = ref false
  and markdown_mode = ref false
  and dtd_mode = ref false
  and closed = ref false (* shows release_closed *)
  and all = ref false (* shows release_impl as well *)
  and dirname = ref "" in

  Arg.parse [ "-dot", Arg.Set dot_mode, "output dot graph";
              "-markdown", Arg.Set markdown_mode, "output markdown document";
              "-dtd", Arg.Set dtd_mode, "output XML DTD";
              "-closed", Arg.Set closed, "output all OSS + closed API functions but not including internal ones";
              "-all", Arg.Set all, "output all API functions, including internal ones";
            ]
    (fun x -> dirname := x)
    "compile XenSource API datamodel specification";
  let all_modes = [ !dot_mode; !markdown_mode; !dtd_mode ] in

  let num_modes_set = List.length (List.filter (fun x->x) all_modes) in

  if num_modes_set = 0 then failwith "No mode set on the commandline";
  if num_modes_set > 1 then failwith "More than one mode on the commandline";

  let oss_filter api =
    filter
      (fun _ -> true) (fun field -> List.mem "3.0.3" field.release.opensource)
      (fun message -> List.mem "3.0.3" message.msg_release.opensource)
      api in
  let closed_filter api =
    filter
      (fun _ -> true) (fun field -> List.mem "closed" field.release.internal)
      (fun message -> List.mem "closed" message.msg_release.internal)
      api in

  let api = match !all, !closed with
    | true, _ -> all_api
    | _, true -> closed_filter all_api
    | _, false -> oss_filter all_api in


  (* Add all implicit messages to the API directly *)
  let api = DU.add_implicit_messages ~document_order:!markdown_mode api in
  (* Only show those visible to the client *)
  let api = filter (fun _ -> true) (fun field -> true) DU.on_client_side api in
  (* And only messages marked as not hidden from the docs, and non-internal fields *)
  let api = filter (fun _ -> true) (fun f -> not f.internal_only) (fun m -> not m.msg_hide_from_docs) api in

  if (!markdown_mode) then
    Markdown_backend.all api !dirname;

  if !dirname <> "" then Unix.chdir !dirname;
  if !dot_mode then begin
    List.iter print_endline (Dot_backend.of_objs api)
  end;

  if !dtd_mode then begin
    let api = filter (fun _ -> true)
        (fun field -> field.qualifier <> DynamicRO)
        (fun _ -> true)
        api in
    List.iter print_endline (Dtd_backend.of_objs api);
  end
