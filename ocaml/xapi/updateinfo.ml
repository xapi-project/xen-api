(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = "updateinfo" end)

open D
open Rpm

module Guidance = struct
  type t = RebootHost | RestartToolstack | EvacuateHost | RestartDeviceModel

  type guidance_kind = Absolute | Recommended

  let compare = Stdlib.compare

  let to_string = function
    | RebootHost ->
        "RebootHost"
    | RestartToolstack ->
        "RestartToolstack"
    | EvacuateHost ->
        "EvacuateHost"
    | RestartDeviceModel ->
        "RestartDeviceModel"

  let of_string = function
    | "RebootHost" ->
        RebootHost
    | "RestartToolstack" ->
        RestartToolstack
    | "EvacuateHost" ->
        EvacuateHost
    | "RestartDeviceModel" ->
        RestartDeviceModel
    | _ ->
        error "Unknown node in <absolute|recommended_guidance>" ;
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
end

module Applicability = struct
  type inequality = Lt | Eq | Gt | Lte | Gte

  type t = {
      name: string
    ; arch: string
    ; inequality: inequality option
    ; epoch: Epoch.t
    ; version: string
    ; release: string
  }

  exception Invalid_inequality

  let string_of_inequality = function
    | Lt ->
        "lt"
    | Eq ->
        "eq"
    | Gt ->
        "gt"
    | Gte ->
        "gte"
    | Lte ->
        "lte"

  let inequality_of_string = function
    | "gte" ->
        Gte
    | "lte" ->
        Lte
    | "gt" ->
        Gt
    | "lt" ->
        Lt
    | "eq" ->
        Eq
    | _ ->
        raise Invalid_inequality

  let default =
    {
      name= ""
    ; arch= ""
    ; inequality= None
    ; epoch= None
    ; version= ""
    ; release= ""
    }

  let assert_valid = function
    | {name= ""; _}
    | {arch= ""; _}
    | {inequality= None; _}
    | {version= ""; _}
    | {release= ""; _} ->
        (* The error should not block update. Ingore it. *)
        warn "Invalid applicability" ;
        None
    | a ->
        Some a

  let of_xml = function
    | Xml.Element ("applicability", _, children) ->
        List.fold_left
          (fun a n ->
            match n with
            | Xml.Element ("inequality", _, [Xml.PCData v]) ->
                {
                  a with
                  inequality=
                    ( try Some (inequality_of_string v)
                      with Invalid_inequality -> None
                    )
                }
            | Xml.Element ("epoch", _, [Xml.PCData v]) -> (
              try {a with epoch= Epoch.of_string v}
              with e ->
                let msg =
                  Printf.sprintf "%s: %s" (ExnHelper.string_of_exn e) v
                in
                (* The error should not block update. Ingore it. *)
                warn "%s" msg ; a
            )
            | Xml.Element ("version", _, [Xml.PCData v]) ->
                {a with version= v}
            | Xml.Element ("release", _, [Xml.PCData v]) ->
                {a with release= v}
            | Xml.Element ("name", _, [Xml.PCData v]) ->
                {a with name= v}
            | Xml.Element ("arch", _, [Xml.PCData v]) ->
                {a with arch= v}
            | _ ->
                (* The error should not block update. Ingore it. *)
                warn "Unknown node in <applicability>" ;
                a
          )
          default children
        |> assert_valid
    | _ ->
        (* The error should not block update. Ingore it. *)
        warn "Unknown node in <guidance_applicabilities>" ;
        None

  let to_string a =
    Printf.sprintf "%s %s %s:%s-%s" a.name
      (Option.value
         (Option.map string_of_inequality a.inequality)
         ~default:"InvalidInequality"
      )
      (Epoch.to_string a.epoch) a.version a.release

  let eval ~epoch ~version ~release ~applicability =
    let epoch' = applicability.epoch in
    let version' = applicability.version in
    let release' = applicability.release in
    match applicability.inequality with
    | Some Lt ->
        Pkg.lt epoch version release epoch' version' release'
    | Some Lte ->
        Pkg.lte epoch version release epoch' version' release'
    | Some Gt ->
        Pkg.gt epoch version release epoch' version' release'
    | Some Gte ->
        Pkg.gte epoch version release epoch' version' release'
    | Some Eq ->
        Pkg.eq epoch version release epoch' version' release'
    | _ ->
        raise Invalid_inequality
end

module RepoMetaData = struct
  type t = {checksum: string; location: string}

  type datatype = UpdateInfo | Group

  let string_of_datatype = function
    | UpdateInfo ->
        "updateinfo"
    | Group ->
        "group"

  let assert_valid = function
    | {checksum= ""; _} | {location= ""; _} ->
        error "Can't find valid 'checksum' or 'location'" ;
        raise Api_errors.(Server_error (invalid_repomd_xml, []))
    | _ ->
        ()

  let of_xml xml data_type =
    let dt = string_of_datatype data_type in
    match xml with
    | Xml.Element ("repomd", _, children) -> (
        let get_node = function
          | Xml.Element ("data", attrs, nodes) -> (
            match List.assoc_opt "type" attrs with
            | Some data_type' when data_type' = dt ->
                Some nodes
            | _ ->
                None
          )
          | _ ->
              None
        in
        match List.filter_map get_node children with
        | [l] ->
            List.fold_left
              (fun md n ->
                match n with
                | Xml.Element ("checksum", _, [Xml.PCData v]) ->
                    {md with checksum= v}
                | Xml.Element ("location", attrs, _) -> (
                  try {md with location= List.assoc "href" attrs}
                  with _ ->
                    error "Failed to get 'href' in 'location' of '%s'" dt ;
                    raise Api_errors.(Server_error (invalid_repomd_xml, []))
                )
                | _ ->
                    md
              )
              {checksum= ""; location= ""}
              l
            |> fun md -> assert_valid md ; md
        | _ ->
            error "Missing or multiple '%s' node(s)" dt ;
            raise Api_errors.(Server_error (invalid_repomd_xml, []))
      )
    | _ ->
        error "Missing 'repomd' node" ;
        raise Api_errors.(Server_error (invalid_repomd_xml, []))

  let of_xml_file xml_path data_type =
    match Sys.file_exists xml_path with
    | false ->
        error "No repomd.xml found: %s" xml_path ;
        raise Api_errors.(Server_error (invalid_repomd_xml, []))
    | true -> (
      match Xml.parse_file xml_path with
      | xml ->
          of_xml xml data_type
      | exception e ->
          error "Failed to parse repomd.xml: %s" (ExnHelper.string_of_exn e) ;
          raise Api_errors.(Server_error (invalid_repomd_xml, []))
    )
end

module UpdateInfo = struct
  type t = {
      id: string
    ; summary: string
    ; description: string
    ; rec_guidance: Guidance.t option
    ; abs_guidance: Guidance.t option
    ; guidance_applicabilities: Applicability.t list
    ; spec_info: string
    ; url: string
    ; update_type: string
  }

  let guidance_to_string o =
    Option.value (Option.map Guidance.to_string o) ~default:""

  let to_json ui =
    `Assoc
      [
        ("id", `String ui.id)
      ; ("summary", `String ui.summary)
      ; ("description", `String ui.description)
      ; ("special-info", `String ui.spec_info)
      ; ("URL", `String ui.url)
      ; ("type", `String ui.update_type)
      ; ("recommended-guidance", `String (guidance_to_string ui.rec_guidance))
      ; ("absolute-guidance", `String (guidance_to_string ui.abs_guidance))
      ]

  let to_string ui =
    Printf.sprintf
      "id=%s rec_guidance=%s abs_guidance=%s guidance_applicabilities=%s" ui.id
      (guidance_to_string ui.rec_guidance)
      (guidance_to_string ui.abs_guidance)
      (String.concat ";"
         (List.map Applicability.to_string ui.guidance_applicabilities)
      )

  let default =
    {
      id= ""
    ; summary= ""
    ; description= ""
    ; rec_guidance= None
    ; abs_guidance= None
    ; guidance_applicabilities= []
    ; spec_info= ""
    ; url= ""
    ; update_type= ""
    }

  let assert_valid_updateinfo = function
    | {id= ""; _} | {summary= ""; _} | {update_type= ""; _} ->
        error "One or more of 'id', 'summary', and 'type' is/are missing" ;
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
    | ui ->
        ui

  let assert_no_dup_update_id l =
    let comp x y = compare x.id y.id in
    if List.length (List.sort_uniq comp l) = List.length (List.sort comp l) then
      l
    else (
      error "Found updates with same upadte ID" ;
      raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
    )

  let of_xml = function
    | Xml.Element ("updates", _, children) ->
        List.filter_map
          (fun n ->
            match n with
            | Xml.Element ("update", attr, update_nodes) ->
                let ty =
                  match List.assoc_opt "type" attr with
                  | Some ty ->
                      ty
                  | None ->
                      ""
                in
                let ui =
                  List.fold_left
                    (fun acc node ->
                      match node with
                      | Xml.Element ("id", _, [Xml.PCData v]) ->
                          {acc with id= v}
                      | Xml.Element ("url", _, [Xml.PCData v]) ->
                          {acc with url= v}
                      | Xml.Element ("special_info", _, [Xml.PCData v]) ->
                          {acc with spec_info= v}
                      | Xml.Element ("summary", _, [Xml.PCData v]) ->
                          {acc with summary= v}
                      | Xml.Element ("description", _, [Xml.PCData v]) ->
                          {acc with description= v}
                      | Xml.Element ("recommended_guidance", _, [Xml.PCData v])
                        -> (
                        try {acc with rec_guidance= Some (Guidance.of_string v)}
                        with e ->
                          (* The error should not block update. Ingore it. *)
                          warn "%s" (ExnHelper.string_of_exn e) ;
                          acc
                      )
                      | Xml.Element ("absolute_guidance", _, [Xml.PCData v])
                        -> (
                        try {acc with abs_guidance= Some (Guidance.of_string v)}
                        with e ->
                          (* The error should not block update. Ingore it. *)
                          warn "%s" (ExnHelper.string_of_exn e) ;
                          acc
                      )
                      | Xml.Element ("guidance_applicabilities", _, apps) ->
                          {
                            acc with
                            guidance_applicabilities=
                              List.filter_map Applicability.of_xml apps
                          }
                      | _ ->
                          acc
                    )
                    {default with update_type= ty}
                    update_nodes
                  |> assert_valid_updateinfo
                in
                debug "updateinfo: %s" (to_string ui) ;
                Some ui
            | _ ->
                None
          )
          children
        |> assert_no_dup_update_id
        |> List.map (fun updateinfo -> (updateinfo.id, updateinfo))
    | _ ->
        error "Failed to parse updateinfo.xml: missing <updates>" ;
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))

  let of_xml_file xml_file_path =
    match Xml.parse_file xml_file_path with
    | xml ->
        of_xml xml
    | exception e ->
        error "Failed to parse updateinfo.xml: %s" (ExnHelper.string_of_exn e) ;
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
end
