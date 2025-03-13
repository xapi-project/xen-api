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
(* Front end around the API code generator. Central place for filtering *)

open Datamodel_types
open Cmdliner

type filter = Nothing | OpenSource | Closed | Debug

module Field = struct
  let filter_of filter filter_internal field =
    let filter_internal =
      if filter_internal then not field.internal_only else true
    in
    filter_internal
    &&
    match filter with
    | OpenSource ->
        List.mem "3.0.3" field.release.opensource
    | Closed ->
        List.mem "closed" field.release.internal
    | Debug ->
        List.mem "debug" field.release.internal
    | Nothing ->
        true
end

module Message = struct
  let filter_of filter filter_internal msg =
    let filter_internal =
      if filter_internal then
        not msg.msg_db_only
      else
        true
    in
    filter_internal
    &&
    match filter with
    | OpenSource ->
        List.mem "3.0.3" msg.msg_release.opensource
    | Closed ->
        List.mem "closed" msg.msg_release.internal
    | Debug ->
        List.mem "debug" msg.msg_release.internal
    | Nothing ->
        true
end

let filter_api filter filter_internal =
  let api = Datamodel.all_api |> Datamodel_utils.add_implicit_messages in
  let field = Field.filter_of filter filter_internal in
  let message = Message.filter_of filter filter_internal in
  Dm_api.filter_by ~field ~message api

let filter_internal_arg =
  let doc = "Filter out internal messages and internal-only fields." in
  Arg.(value & flag & info ["filter-internal"] ~doc ~docv:"FILTER_INTERNAL")

let gen_debug_arg =
  let doc = "Intersperse debugging code amid output." in
  Arg.(value & flag & info ["gen-debug"] ~doc ~docv:"GEN_DEBUG")

let filter_arg =
  let options =
    [
      ("nothing", Nothing)
    ; ("opensource", OpenSource)
    ; ("closed", Closed)
    ; ("debug", Debug)
    ]
  in
  let doc =
    let keys = List.map fst options |> String.concat ", " in
    Printf.sprintf
      "Specify how the datamodel API should be filtered prior to processing, \
       one of: %s."
      keys
  in
  let filter =
    Arg.(opt (enum options) Nothing & info ["f"; "filter"] ~doc ~docv:"FILTER")
  in
  Arg.(value & filter)

let gen_command_common ~name ~doc
    ~(f : Gen_api_types.config -> Dm_api.api -> unit) =
  let go filter filter_internal debug =
    let api = filter_api filter filter_internal in
    let config = Gen_api_types.{debug} in
    f config api
  in
  let man = [] in
  let info = Cmd.info name ~version:"0.1" ~doc ~man in
  let term =
    Term.(const go $ filter_arg $ filter_internal_arg $ gen_debug_arg)
  in
  Cmd.v info term

module Commands = struct
  open Gen_api

  let server =
    gen_command_common ~name:"server" ~doc:"generate server.ml" ~f:gen_server

  let client =
    gen_command_common ~name:"client" ~doc:"generate client.ml" ~f:gen_client

  let api =
    gen_command_common ~name:"api" ~doc:"generate api.ml" ~f:gen_client_types

  let rbac =
    gen_command_common ~name:"rbac" ~doc:"generate rbac_static.ml" ~f:gen_rbac

  let db =
    gen_command_common ~name:"db" ~doc:"generate db_actions.ml"
      ~f:gen_db_actions

  let utils =
    gen_command_common ~name:"utils" ~doc:"generate generated_record_utils.ml"
      ~f:gen_record_deserialization

  let actions =
    gen_command_common ~name:"actions" ~doc:"generate custom_actions.ml"
      ~f:gen_custom_actions
end

let driver =
  let group = Commands.[server; client; api; rbac; db; utils; actions] in
  let exe = Filename.basename Sys.argv.(0) in
  let info = Cmd.info exe ~version:"0.1" in
  Cmd.group info group

let () = exit (Cmd.eval driver)
