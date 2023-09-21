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
  type t =
    | RebootHost
    | RestartToolstack
    | EvacuateHost
    | RestartDeviceModel
    | RebootHostOnLivePatchFailure
    | RebootHostOnXenLivePatchFailure
    | RebootHostOnKernelLivePatchFailure

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
    | RebootHostOnLivePatchFailure ->
        "RebootHostOnLivePatchFailure"
    | RebootHostOnKernelLivePatchFailure ->
        "RebootHostOnKernelLivePatchFailure"
    | RebootHostOnXenLivePatchFailure ->
        "RebootHostOnXenLivePatchFailure"

  let of_string = function
    | "RebootHost" ->
        RebootHost
    | "RestartToolstack" ->
        RestartToolstack
    | "EvacuateHost" ->
        EvacuateHost
    | "RestartDeviceModel" ->
        RestartDeviceModel
    | g ->
        warn
          "Un-recognized guidance in \
           <absolute|recommended|livepatch_guidance>: %s, fallback to \
           RebootHost"
          g ;
        RebootHost

  let of_pending_guidance = function
    | `reboot_host ->
        RebootHost
    | `reboot_host_on_livepatch_failure ->
        RebootHostOnLivePatchFailure
    | `reboot_host_on_kernel_livepatch_failure ->
        RebootHostOnKernelLivePatchFailure
    | `reboot_host_on_xen_livepatch_failure ->
        RebootHostOnXenLivePatchFailure
    | `restart_toolstack ->
        RestartToolstack
    | `restart_device_model ->
        RestartDeviceModel

  let to_pending_guidance = function
    | RebootHost ->
        Some `reboot_host
    | RebootHostOnLivePatchFailure ->
        Some `reboot_host_on_livepatch_failure
    | RebootHostOnKernelLivePatchFailure ->
        Some `reboot_host_on_kernel_livepatch_failure
    | RebootHostOnXenLivePatchFailure ->
        Some `reboot_host_on_xen_livepatch_failure
    | RestartToolstack ->
        Some `restart_toolstack
    | RestartDeviceModel ->
        Some `restart_device_model
    | EvacuateHost ->
        None
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

module LivePatch = struct
  exception Invalid_version_release

  exception Invalid_livepatch

  type t = {
      component: Livepatch.component
    ; base_build_id: string
    ; base_version: string
    ; base_release: string
    ; to_version: string
    ; to_release: string
  }

  let compare = Stdlib.compare

  let version_release_of_string s =
    match Astring.String.cuts ~sep:"-" s with
    | [version; release] ->
        (version, release)
    | _ ->
        raise Invalid_version_release

  let to_json lp =
    `Assoc
      [
        ("component", `String (Livepatch.string_of_component lp.component))
      ; ("base_build_id", `String lp.base_build_id)
      ; ("base_version", `String lp.base_version)
      ; ("base_release", `String lp.base_release)
      ; ("to_version", `String lp.to_version)
      ; ("to_release", `String lp.to_release)
      ]

  let to_string lp = Yojson.Basic.pretty_to_string (to_json lp)

  let assert_valid = function
    | {base_build_id= ""; _}
    | {base_version= ""; _}
    | {base_release= ""; _}
    | {to_version= ""; _}
    | {to_release= ""; _} ->
        (* The error should not block update. Ingore it. *)
        warn "Invalid livepatch metadata" ;
        raise Invalid_livepatch
    | _ ->
        ()

  let initial_record attrs =
    match List.assoc_opt "component" attrs with
    | Some s -> (
      try
        let component = Livepatch.component_of_string s in
        Some
          {
            component
          ; base_build_id= ""
          ; base_version= ""
          ; base_release= ""
          ; to_version= ""
          ; to_release= ""
          }
      with e ->
        let msg = Printf.sprintf "%s: %s" (ExnHelper.string_of_exn e) s in
        warn "%s" msg ; None
    )
    | None ->
        warn "No component in livepatch" ;
        None

  let of_xml livepatches =
    livepatches
    |> List.filter_map (function
         | Xml.Element ("livepatch", attrs, _) -> (
           match initial_record attrs with
           | None ->
               None
           | Some lp -> (
             match
               ( List.assoc_opt "base-buildid" attrs
               , List.assoc_opt "base" attrs
               , List.assoc_opt "to" attrs
               )
             with
             | Some base_build_id, Some base_vr, Some to_vr -> (
                 let open Rresult.R.Infix in
                 ( Ok {lp with base_build_id} >>= fun lp ->
                   ( try
                       let v, r = version_release_of_string base_vr in
                       Ok {lp with base_version= v; base_release= r}
                     with e ->
                       let msg =
                         Printf.sprintf "%s: %s in 'base' of livepatch"
                           (ExnHelper.string_of_exn e)
                           base_vr
                       in
                       Error msg
                   )
                   >>= fun lp ->
                   try
                     let v, r = version_release_of_string to_vr in
                     Ok {lp with to_version= v; to_release= r}
                   with e ->
                     let msg =
                       Printf.sprintf "%s: %s in 'to' of livepatch"
                         (ExnHelper.string_of_exn e)
                         to_vr
                     in
                     Error msg
                 )
                 |> function
                 | Ok lp -> (
                   try assert_valid lp ; Some lp with _ -> None
                 )
                 | Error msg ->
                     warn "Can't parse livepatch: %s" msg ;
                     None
               )
             | _ ->
                 let s =
                   attrs
                   |> List.map (fun (k, v) -> k ^ "=" ^ v)
                   |> Astring.String.concat ~sep:";"
                 in
                 warn "Can't parse livepatch from attributes: %s" s ;
                 None
           )
         )
         | _ ->
             warn "Unexpected child in livepatches" ;
             None
         )
end

module Severity = struct
  type t = None | High

  let to_string = function None -> "None" | High -> "High"

  let of_string = function
    | "None" ->
        None
    | "High" ->
        High
    | _ ->
        error "Unknown severity in updateinfo" ;
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
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
    ; livepatch_guidance: Guidance.t option
    ; livepatches: LivePatch.t list
    ; issued: Xapi_stdext_date.Date.t
    ; severity: Severity.t
  }

  let guidance_to_string o =
    Option.value (Option.map Guidance.to_string o) ~default:""

  let to_json ui =
    let l =
      [
        ("id", `String ui.id)
      ; ("summary", `String ui.summary)
      ; ("description", `String ui.description)
      ; ("special-info", `String ui.spec_info)
      ; ("URL", `String ui.url)
      ; ("type", `String ui.update_type)
      ; ("recommended-guidance", `String (guidance_to_string ui.rec_guidance))
      ; ("absolute-guidance", `String (guidance_to_string ui.abs_guidance))
      ; ("issued", `String (Xapi_stdext_date.Date.to_string ui.issued))
      ; ("severity", `String (Severity.to_string ui.severity))
      ]
    in
    match ui.livepatches with
    | [] ->
        `Assoc l
    | _ as lps ->
        let l' =
          ( "livepatch-guidance"
          , `String (guidance_to_string ui.livepatch_guidance)
          )
          :: ("livepatches", `List (List.map (fun x -> LivePatch.to_json x) lps))
          :: l
        in
        `Assoc l'

  let to_string ui =
    Printf.sprintf
      "id=%s rec_guidance=%s abs_guidance=%s guidance_applicabilities=%s \
       livepatch_guidance=%s livepatches=%s"
      ui.id
      (guidance_to_string ui.rec_guidance)
      (guidance_to_string ui.abs_guidance)
      (String.concat ";"
         (List.map Applicability.to_string ui.guidance_applicabilities)
      )
      (guidance_to_string ui.livepatch_guidance)
      (Astring.String.concat ~sep:";"
         (List.map LivePatch.to_string ui.livepatches)
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
    ; livepatch_guidance= None
    ; livepatches= []
    ; issued= Xapi_stdext_date.Date.epoch
    ; severity= Severity.None
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
                        ->
                          {acc with rec_guidance= Some (Guidance.of_string v)}
                      | Xml.Element ("absolute_guidance", _, [Xml.PCData v]) ->
                          {acc with abs_guidance= Some (Guidance.of_string v)}
                      | Xml.Element ("livepatch_guidance", _, [Xml.PCData v]) ->
                          {
                            acc with
                            livepatch_guidance= Some (Guidance.of_string v)
                          }
                      | Xml.Element ("guidance_applicabilities", _, apps) ->
                          {
                            acc with
                            guidance_applicabilities=
                              List.filter_map Applicability.of_xml apps
                          }
                      | Xml.Element ("livepatches", _, livepatches) ->
                          {acc with livepatches= LivePatch.of_xml livepatches}
                      | Xml.Element ("issued", attr, _) ->
                          let issued =
                            match List.assoc_opt "date" attr with
                            | Some date -> (
                              try
                                Xapi_stdext_date.Date.of_string
                                  (Scanf.sscanf date
                                     "%04d-%02d-%02d %02d:%02d:%02d"
                                     (fun y mon d h m s ->
                                       Printf.sprintf
                                         "%04i%02i%02iT%02i:%02i:%02iZ" y mon d
                                         h m s
                                   )
                                  )
                              with e ->
                                (* The error should not block update. Ingore it
                                   and set "issued" as epoch. *)
                                warn "%s" (ExnHelper.string_of_exn e) ;
                                Xapi_stdext_date.Date.epoch
                            )
                            | None ->
                                Xapi_stdext_date.Date.epoch
                          in
                          {acc with issued}
                      | Xml.Element ("severity", _, [Xml.PCData v]) -> (
                        try {acc with severity= Severity.of_string v}
                        with e ->
                          (* The error should not block update. Ingore it. *)
                          warn "%s" (ExnHelper.string_of_exn e) ;
                          acc
                      )
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

module HostUpdates = struct
  type t = {
      host: string
    ; rec_guidances: Guidance.t list
    ; abs_guidances: Guidance.t list
    ; rpms: Rpm.Pkg.t list
    ; update_ids: string list
    ; livepatches: LivePatch.t list
  }

  let to_json host_updates =
    let g_to_j x = `String (Guidance.to_string x) in
    let p_to_j x = `String (Pkg.to_fullname x) in
    `Assoc
      [
        ("ref", `String host_updates.host)
      ; ( "recommended-guidance"
        , `List (List.map g_to_j host_updates.rec_guidances)
        )
      ; ("absolute-guidance", `List (List.map g_to_j host_updates.abs_guidances))
      ; ("RPMS", `List (List.map p_to_j host_updates.rpms))
      ; ( "updates"
        , `List (List.map (fun upd_id -> `String upd_id) host_updates.update_ids)
        )
      ; ( "livepatches"
        , `List (List.map LivePatch.to_json host_updates.livepatches)
        )
      ]
end
