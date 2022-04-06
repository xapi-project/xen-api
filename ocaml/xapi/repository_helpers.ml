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

module D = Debug.Make (struct let name = "repository_helpers" end)

open D
module Unixext = Xapi_stdext_unix.Unixext
module UpdateIdSet = Set.Make (String)

let exposing_pool_repo_mutex = Mutex.create ()

module Epoch = struct
  type t = int option

  let epoch_none = "(none)"

  exception Invalid_epoch

  let of_string = function
    | s when s = epoch_none || s = "None" ->
        None
    | s -> (
      match int_of_string s with
      | i when i = 0 ->
          None
      | i when i > 0 ->
          Some i
      | _ ->
          raise Invalid_epoch
      | exception _ ->
          raise Invalid_epoch
    )

  let to_string = function Some i -> string_of_int i | None -> epoch_none
end

module Pkg = struct
  type t = {
      name: string
    ; epoch: Epoch.t
    ; version: string
    ; release: string
    ; arch: string
  }

  type order = LT | EQ | GT

  type segment_of_version = Int of int | Str of string

  let string_of_order = function LT -> "<" | EQ -> "=" | GT -> ">"

  let error_msg = Printf.sprintf "Failed to parse '%s'"

  let parse_epoch_version_release epoch_ver_rel =
    (* The epoch_ver_rel likes, I.E.
     *   "10.1.11-34",
     *   "2:1.1.11-34",
     *   "None:1.1.11-34", or
     *   "(none):1.1.11-34".
     *
     * These may come from:
     *   "yum list updates",
     *   "yum updateinfo list updates", or
     *   "rpm -qa" *)
    let open Rresult.R.Infix in
    ( ( match Astring.String.cuts ~sep:":" epoch_ver_rel with
      | [e; vr] -> (
        try Ok (Epoch.of_string e, vr) with _ -> Error "Invalid epoch"
      )
      | [vr] ->
          Ok (None, vr)
      | _ ->
          Error "Invalid epoch:version-release"
      )
    >>= fun (e, vr) ->
      match Astring.String.cuts ~sep:"-" vr with
      | [v; r] ->
          Ok (e, v, r)
      | _ ->
          Error "Invalid version-release"
    )
    |> function
    | Ok (e, v, r) ->
        (e, v, r)
    | Error msg ->
        let msg = error_msg epoch_ver_rel in
        error "%s" msg ;
        raise Api_errors.(Server_error (internal_error, [msg]))

  let of_fullname s =
    (* The s likes, I.E.
     *   "libpath-utils-0.2.1-29.el7.x86_64",
     *   "qemu-dp-2:2.12.0-2.0.11.x86_64", or
     *   "time-(none):1.7-45.el7.x86_64".
     * These may come from "yum updateinfo list updates" and "rpm -qa" *)
    match Astring.String.cut ~rev:true ~sep:"." s with
    | Some (pkg, (("noarch" | "x86_64") as arch)) -> (
      try
        let pos1 = String.rindex pkg '-' in
        let pos2 = String.rindex_from pkg (pos1 - 1) '-' in
        let epoch_ver_rel =
          String.sub pkg (pos2 + 1) (String.length pkg - pos2 - 1)
        in
        let epoch, version, release =
          parse_epoch_version_release epoch_ver_rel
        in
        let name = String.sub pkg 0 pos2 in
        Some {name; epoch; version; release; arch}
      with e ->
        let msg = error_msg s in
        warn "%s: %s" msg (ExnHelper.string_of_exn e) ;
        (* The error should not block update. Ingore it. *)
        None
    )
    | Some _ | None ->
        None

  let to_epoch_ver_rel_json pkg =
    `Assoc
      [
        ("epoch", `String (Epoch.to_string pkg.epoch))
      ; ("version", `String pkg.version)
      ; ("release", `String pkg.release)
      ]

  let to_name_arch_string pkg = pkg.name ^ "." ^ pkg.arch

  let to_fullname pkg =
    match pkg.epoch with
    | Some i ->
        Printf.sprintf "%s-%s:%s-%s.%s.rpm" pkg.name (string_of_int i)
          pkg.version pkg.release pkg.arch
    | None ->
        Printf.sprintf "%s-%s-%s.%s.rpm" pkg.name pkg.version pkg.release
          pkg.arch

  let compare_epoch e1 e2 =
    match (e1, e2) with
    | Some i1, Some i2 ->
        if i1 < i2 then
          LT
        else if i1 = i2 then
          EQ
        else
          GT
    | Some _, None ->
        GT
    | None, Some _ ->
        LT
    | None, None ->
        EQ

  let compare_version_strings s1 s2 =
    (* Compare versions or releases of RPM packages
     * I.E. for "libpath-utils-0.2.1-29.el7.x86_64" and "libpath-utils-0.2.1a-30.el7.x86_64",
     * this function compares:
     * versions between "0.2.1" and "0.2.1a", or
     * releases between "29.el7" and "30.el7".
     * More examples:
     *  "1.2.3" "<" "1.2.4"
     *  "1.2.3" "=" "1.2.3"
     *  "1.2.3" ">" "1.2"
     *  "1.0011" ">" "1.9"
     *  "1.05" "=" "1.5"
     *  "1.0" ">" "1"
     *  "1.0" ">" "1.a"
     *  "2.50" ">" "2.5"
     *  "XS3" "<" "xs2"
     *  "1.2.3" ">" "1.2.3a"
     *  "xs4" "=" "xs.4"
     *  "2a" "<" "2.0"
     *  "2a" "<" "2b"
     *  "1.0" ">" "1.xs2"
     *  "1.0_xs" "=" "1.0.xs"
     *)
    let normalize v =
      let split_letters_and_numbers s =
        let r = Re.Posix.compile_pat {|^([^0-9]+)([0-9]+)$|} in
        match Re.exec_opt r s with
        | Some groups ->
            [Re.Group.get groups 1; Re.Group.get groups 2]
        | None ->
            [s]
      in
      let number = Re.Posix.compile_pat "^[0-9]+$" in
      v
      |> Astring.String.cuts ~sep:"."
      |> List.concat_map (fun s -> Astring.String.cuts ~sep:"_" s)
      |> List.concat_map (fun s -> split_letters_and_numbers s)
      |> List.map (fun s ->
             if Re.execp number s then
               match int_of_string s with i -> Int i | exception _ -> Str s
             else
               Str s
         )
    in
    let rec compare_segments l1 l2 =
      match (l1, l2) with
      | c1 :: t1, c2 :: t2 -> (
        match (c1, c2) with
        | Int s1, Int s2 ->
            if s1 > s2 then
              GT
            else if s1 = s2 then
              compare_segments t1 t2
            else
              LT
        | Int s1, Str s2 ->
            GT
        | Str s1, Int s2 ->
            LT
        | Str s1, Str s2 ->
            let r = String.compare s1 s2 in
            if r < 0 then
              LT
            else if r > 0 then
              GT
            else
              compare_segments t1 t2
      )
      | _ :: _, [] ->
          GT
      | [], _ :: _ ->
          LT
      | [], [] ->
          EQ
    in
    compare_segments (normalize s1) (normalize s2)

  let lt e1 v1 r1 e2 v2 r2 =
    match
      ( compare_epoch e1 e2
      , compare_version_strings v1 v2
      , compare_version_strings r1 r2
      )
    with
    | LT, _, _ | EQ, LT, _ | EQ, EQ, LT ->
        true
    | _ ->
        false

  let gt e1 v1 r1 e2 v2 r2 =
    match
      ( compare_epoch e1 e2
      , compare_version_strings v1 v2
      , compare_version_strings r1 r2
      )
    with
    | GT, _, _ | EQ, GT, _ | EQ, EQ, GT ->
        true
    | _ ->
        false

  let eq e1 v1 r1 e2 v2 r2 =
    match
      ( compare_epoch e1 e2
      , compare_version_strings v1 v2
      , compare_version_strings r1 r2
      )
    with
    | EQ, EQ, EQ ->
        true
    | _ ->
        false

  let lte e1 v1 r1 e2 v2 r2 = lt e1 v1 r1 e2 v2 r2 || eq e1 v1 r1 e2 v2 r2

  let gte e1 v1 r1 e2 v2 r2 = gt e1 v1 r1 e2 v2 r2 || eq e1 v1 r1 e2 v2 r2
end

module Update = struct
  type t = {
      name: string
    ; arch: string
    ; old_epoch: Epoch.t option
    ; old_version: string option
    ; old_release: string option
    ; new_epoch: Epoch.t
    ; new_version: string
    ; new_release: string
    ; update_id: string option
    ; repository: string
  }

  let of_json j =
    try
      let open Yojson.Basic.Util in
      {
        name= member "name" j |> to_string
      ; arch= member "arch" j |> to_string
      ; new_epoch=
          member "newEpochVerRel" j
          |> member "epoch"
          |> to_string
          |> Epoch.of_string
      ; new_version= member "newEpochVerRel" j |> member "version" |> to_string
      ; new_release= member "newEpochVerRel" j |> member "release" |> to_string
      ; old_epoch=
          ( try
              Some
                (member "oldEpochVerRel" j
                |> member "epoch"
                |> to_string
                |> Epoch.of_string
                )
            with _ -> None
          )
      ; old_version=
          ( try Some (member "oldEpochVerRel" j |> member "version" |> to_string)
            with _ -> None
          )
      ; old_release=
          ( try Some (member "oldEpochVerRel" j |> member "release" |> to_string)
            with _ -> None
          )
      ; update_id= member "updateId" j |> to_string_option
      ; repository= member "repository" j |> to_string
      }
    with e ->
      let msg = "Can't construct an update from json" in
      error "%s: %s" msg (ExnHelper.string_of_exn e) ;
      raise Api_errors.(Server_error (internal_error, [msg]))

  let to_string u =
    Printf.sprintf "%s.%s %s:%s-%s -> %s:%s-%s from %s:%s" u.name u.arch
      ( match u.old_epoch with
      | Some e ->
          Epoch.to_string e
      | None ->
          Epoch.epoch_none
      )
      (Option.value u.old_version ~default:"unknown")
      (Option.value u.old_release ~default:"unknown")
      (Epoch.to_string u.new_epoch)
      u.new_version u.new_release
      (Option.value u.update_id ~default:"unknown")
      u.repository
end

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

module GuidanceSet' = Set.Make (Guidance)

module GuidanceSet = struct
  include GuidanceSet'
  open Guidance

  let eq l s = equal (of_list l) (of_list s)

  let eq_set1 = eq [EvacuateHost; RestartToolstack]

  let eq_set2 = eq [RestartDeviceModel; RestartToolstack]

  let error_msg l =
    Printf.sprintf "Found wrong guidance(s): %s"
      (String.concat ";" (List.map to_string l))

  let assert_valid_guidances = function
    | []
    | [RebootHost]
    | [EvacuateHost]
    | [RestartToolstack]
    | [RestartDeviceModel] ->
        ()
    | l when eq_set1 l ->
        (* EvacuateHost and RestartToolstack *)
        ()
    | l when eq_set2 l ->
        (* RestartDeviceModel and RestartToolstack *)
        ()
    | l ->
        let msg = error_msg l in
        raise Api_errors.(Server_error (internal_error, [msg]))

  let precedences =
    [
      (RebootHost, of_list [RestartToolstack; EvacuateHost; RestartDeviceModel])
    ; (EvacuateHost, of_list [RestartDeviceModel])
    ]

  let resort_guidances ~kind gs =
    let gs' =
      List.fold_left
        (fun acc (higher, lowers) ->
          if mem higher acc then
            diff acc lowers
          else
            acc
        )
        gs precedences
    in
    match kind with Recommended -> gs' | Absolute -> remove EvacuateHost gs'
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
    | md ->
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

let create_repository_record ~__context ~name_label ~name_description
    ~binary_url ~source_url ~update ~gpgkey_path =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.Repository.create ~__context ~ref ~uuid ~name_label ~name_description
    ~binary_url ~source_url ~update ~hash:"" ~up_to_date:false ~gpgkey_path ;
  ref

let assert_url_is_valid ~url =
  try
    let uri = Uri.of_string url in
    match Uri.scheme uri with
    | Some "http" | Some "https" -> (
      match Uri.host uri with
      | Some h -> (
        match !Xapi_globs.repository_domain_name_allowlist with
        | [] ->
            ()
        | l -> (
          match
            List.exists (fun d -> Astring.String.is_suffix ("." ^ d) h) l
          with
          | true ->
              ()
          | false ->
              let msg = "host is not in allowlist" in
              raise Api_errors.(Server_error (internal_error, [msg]))
        )
      )
      | None ->
          raise
            Api_errors.(Server_error (internal_error, ["invalid host in url"]))
    )
    | _ ->
        raise
          Api_errors.(Server_error (internal_error, ["invalid scheme in url"]))
  with e ->
    error "Invalid url %s: %s" url (ExnHelper.string_of_exn e) ;
    raise Api_errors.(Server_error (invalid_base_url, [url]))

let is_gpgkey_path_valid = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' ->
      true
  | _ ->
      false

let assert_gpgkey_path_is_valid path =
  (* When Xapi_globs.repository_gpgcheck is:
   * true, an empty gpgkey path will
   *   1) reuslt to use default one which is configured in repository-gpgkey-path, or,
   *   2) raise an error to user if no default one configured;
   * false, an empty gpgkey path will be ignored.
   * The existence and validity of the GPG public key file will be verified before using *)
  if path = "" || Astring.String.for_all is_gpgkey_path_valid path then
    ()
  else (
    error "Invalid gpgkey path %s" path ;
    raise Api_errors.(Server_error (invalid_gpgkey_path, [path]))
  )

let with_pool_repositories f =
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Mutex.lock exposing_pool_repo_mutex ;
      f ()
    )
    (fun () -> Mutex.unlock exposing_pool_repo_mutex)

let is_local_pool_repo_enabled () =
  if Mutex.try_lock exposing_pool_repo_mutex then (
    Mutex.unlock exposing_pool_repo_mutex ;
    false
  ) else
    true

let with_updateinfo_xml gz_path f =
  let tmpfile, tmpch =
    Filename.open_temp_file ~mode:[Open_text] "updateinfo" ".xml"
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () ->
          try
            Unixext.with_file gz_path [Unix.O_RDONLY] 0o0 @@ fun gz_fd_in ->
            Gzip.decompress_passive gz_fd_in @@ fun fd_in ->
            let ic = Unix.in_channel_of_descr fd_in in
            try
              while true do
                let line = input_line ic in
                output_string tmpch (line ^ "\n")
              done
            with End_of_file -> ()
          with e ->
            error "Failed to decompress updateinfo.xml.gz: %s"
              (ExnHelper.string_of_exn e) ;
            raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )
        (fun () -> close_out tmpch) ;
      f tmpfile
    )
    (fun () -> Unixext.unlink_safe tmpfile)

let clean_yum_cache name =
  try
    let params =
      ["--disablerepo=*"; Printf.sprintf "--enablerepo=%s" name; "clean"; "all"]
    in
    ignore (Helpers.call_script !Xapi_globs.yum_cmd params)
  with e ->
    warn "Unable to clean YUM cache for %s: %s" name (ExnHelper.string_of_exn e)

let remove_repo_conf_file repo_name =
  let path =
    Filename.concat !Xapi_globs.yum_repos_config_dir (repo_name ^ ".repo")
  in
  Unixext.unlink_safe path

let write_yum_config ~source_url ~binary_url ~repo_gpgcheck ~gpgkey_path
    ~repo_name =
  let file_path =
    Filename.concat !Xapi_globs.yum_repos_config_dir (repo_name ^ ".repo")
  in
  let opt_repo_gpgcheck, opt_gpgcheck, opt_gpgkey =
    let opt_gpgkey () =
      let gpgkey_abs_path =
        Filename.concat !Xapi_globs.rpm_gpgkey_dir gpgkey_path
      in
      if not (Sys.file_exists gpgkey_abs_path) then
        raise
          Api_errors.(
            Server_error (internal_error, ["gpg key file does not exist"])
          ) ;
      if not ((Unix.lstat gpgkey_abs_path).Unix.st_kind = Unix.S_REG) then
        raise
          Api_errors.(
            Server_error (internal_error, ["gpg key file is not a regular file"])
          ) ;
      Printf.sprintf "gpgkey=file://%s" gpgkey_abs_path
    in
    match (!Xapi_globs.repository_gpgcheck, repo_gpgcheck) with
    | true, true ->
        ("repo_gpgcheck=1", "gpgcheck=1", opt_gpgkey ())
    | true, false ->
        ("repo_gpgcheck=0", "gpgcheck=1", opt_gpgkey ())
    | false, _ ->
        ("repo_gpgcheck=0", "gpgcheck=0", "")
  in
  let content_of_binary =
    [
      Printf.sprintf "[%s]" repo_name
    ; Printf.sprintf "name=%s" repo_name
    ; Printf.sprintf "baseurl=%s" binary_url
    ; "enabled=0"
    ; opt_repo_gpgcheck
    ; opt_gpgcheck
    ; opt_gpgkey
    ]
  in
  let content_of_source =
    match source_url with
    | None ->
        []
    | Some url ->
        [
          ""
        ; Printf.sprintf "[%s-source]" repo_name
        ; Printf.sprintf "name=%s-source" repo_name
        ; Printf.sprintf "baseurl=%s" url
        ; "enabled=0"
        ; opt_repo_gpgcheck
        ; opt_gpgcheck
        ; opt_gpgkey
        ]
  in
  let content = String.concat "\n" (content_of_binary @ content_of_source) in
  Unixext.atomic_write_to_file file_path 0o400 (fun fd ->
      Unixext.really_write_string fd content |> ignore
  )

let get_repo_config repo_name config_name =
  let config_params = [repo_name] in
  Helpers.call_script !Xapi_globs.yum_config_manager_cmd config_params
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter_map (fun kv ->
         let prefix = Printf.sprintf "%s = " config_name in
         match Astring.String.cuts ~sep:prefix kv with
         | [""; v] ->
             Some v
         | _ ->
             None
     )
  |> function
  | [x] ->
      x
  | _ ->
      let msg =
        Printf.sprintf "Not found %s for repository %s" config_name repo_name
      in
      raise Api_errors.(Server_error (internal_error, [msg]))

let get_enabled_repositories ~__context =
  let pool = Helpers.get_pool ~__context in
  Db.Pool.get_repositories ~__context ~self:pool

let get_repository_name ~__context ~self =
  Db.Repository.get_uuid ~__context ~self

let get_remote_repository_name ~__context ~self =
  !Xapi_globs.remote_repository_prefix
  ^ "-"
  ^ get_repository_name ~__context ~self

let get_local_repository_name ~__context ~self =
  !Xapi_globs.local_repository_prefix
  ^ "-"
  ^ get_repository_name ~__context ~self

let with_local_repositories ~__context f =
  let master_addr =
    try Pool_role.get_master_address ()
    with Pool_role.This_host_is_a_master ->
      Option.get (Helpers.get_management_ip_addr ~__context)
  in
  Stunnel.with_client_proxy ~verify_cert:(Stunnel_client.pool ())
    ~remote_host:master_addr ~remote_port:Constants.default_ssl_port
    ~local_host:"127.0.0.1"
    ~local_port:!Xapi_globs.local_yum_repo_port
  @@ fun () ->
  let enabled = get_enabled_repositories ~__context in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      let repositories =
        List.map
          (fun repository ->
            let repo_name =
              get_local_repository_name ~__context ~self:repository
            in
            let binary_url =
              Printf.sprintf "http://127.0.0.1:%s/repository/%s/"
                (string_of_int !Xapi_globs.local_yum_repo_port)
                (get_remote_repository_name ~__context ~self:repository)
            in
            let gpgkey_path =
              match
                Db.Repository.get_gpgkey_path ~__context ~self:repository
              with
              | "" ->
                  !Xapi_globs.repository_gpgkey_name
              | s ->
                  s
            in
            remove_repo_conf_file repo_name ;
            write_yum_config ~source_url:None ~binary_url ~repo_gpgcheck:false
              ~gpgkey_path ~repo_name ;
            clean_yum_cache repo_name ;
            let config_params =
              [
                "--save"
              ; Printf.sprintf "--setopt=%s.sslverify=false" repo_name
                (* certificate verification is handled by the stunnel proxy *)
              ; Printf.sprintf "--setopt=%s.ptoken=true" repo_name
                (* makes yum include the pool secret as a cookie in all requests
                   (only to access the repo mirror in the coordinator!) *)
              ; repo_name
              ]
            in
            ignore
              (Helpers.call_script
                 !Xapi_globs.yum_config_manager_cmd
                 config_params
              ) ;
            repo_name
          )
          enabled
      in
      f repositories
    )
    (fun () ->
      enabled
      |> List.iter (fun repository ->
             let repo_name =
               get_local_repository_name ~__context ~self:repository
             in
             clean_yum_cache repo_name ;
             remove_repo_conf_file repo_name
         )
    )

let assert_yum_error output =
  let errors = ["Updateinfo file is not valid XML"] in
  List.iter
    (fun err ->
      if Astring.String.is_infix ~affix:err output then (
        error "Error from 'yum updateinfo list': %s" err ;
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
      ) else
        ()
    )
    errors ;
  output

let parse_updateinfo_list acc line =
  match Astring.String.fields ~empty:false line with
  | [update_id; _; full_name] -> (
    match Pkg.of_fullname full_name with
    | Some pkg ->
        (pkg, update_id) :: acc
    | None ->
        acc
  )
  | _ ->
      debug "Ignore unrecognized line '%s' in parsing updateinfo list" line ;
      acc

let get_updates_from_updateinfo ~__context repositories =
  let params_of_updateinfo_list =
    [
      "-q"
    ; "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
    ; "updateinfo"
    ; "list"
    ; "updates"
    ]
  in
  Helpers.call_script !Xapi_globs.yum_cmd params_of_updateinfo_list
  |> assert_yum_error
  |> Astring.String.cuts ~sep:"\n"
  |> List.fold_left parse_updateinfo_list []

let eval_guidance_for_one_update ~updates_info ~update ~kind =
  let open Update in
  match update.update_id with
  | Some uid -> (
    match List.assoc_opt uid updates_info with
    | Some updateinfo -> (
        let is_applicable (a : Applicability.t) =
          match
            ( update.name = a.Applicability.name
            , update.arch = a.Applicability.arch
            )
          with
          | true, true -> (
            match
              (update.old_epoch, update.old_version, update.old_release)
            with
            | Some old_epoch, Some old_version, Some old_release ->
                Applicability.eval ~epoch:old_epoch ~version:old_version
                  ~release:old_release ~applicability:a
            | _ ->
                warn "No installed epoch, version or release for package %s.%s"
                  update.name update.arch ;
                false
          )
          | _ ->
              false
        in
        let dbg_msg r =
          Printf.sprintf
            "Evaluating applicability for package %s.%s in update %s returned \
             '%s'"
            update.name update.arch uid (string_of_bool r)
        in
        let apps = updateinfo.UpdateInfo.guidance_applicabilities in
        match (List.exists is_applicable apps, apps) with
        | true, _ | false, [] -> (
            debug "%s" (dbg_msg true) ;
            match kind with
            | Guidance.Absolute ->
                updateinfo.UpdateInfo.abs_guidance
            | Guidance.Recommended ->
                updateinfo.UpdateInfo.rec_guidance
          )
        | _ ->
            debug "%s" (dbg_msg false) ;
            None
      )
    | None ->
        warn "Can't find update ID %s from updateinfo.xml for update %s.%s" uid
          update.name update.arch ;
        None
  )
  | None ->
      warn "Ignore evaluating against package %s.%s as its update ID is missing"
        update.name update.arch ;
      None

let eval_guidances ~updates_info ~updates ~kind =
  List.fold_left
    (fun acc u ->
      match eval_guidance_for_one_update ~updates_info ~update:u ~kind with
      | Some g ->
          GuidanceSet.add g acc
      | None ->
          acc
    )
    GuidanceSet.empty updates
  |> GuidanceSet.resort_guidances ~kind
  |> GuidanceSet.elements

let repoquery_sep = ":|"

let get_repoquery_fmt () =
  ["name"; "epoch"; "version"; "release"; "arch"; "repoid"]
  |> List.map (fun field -> "%{" ^ field ^ "}")
  |> String.concat repoquery_sep

let parse_line_of_repoquery acc line =
  match Astring.String.cuts ~sep:repoquery_sep line with
  | [name; epoch'; version; release; arch; repo] -> (
    try
      let epoch = Epoch.of_string epoch' in
      let pkg = Pkg.{name; epoch; version; release; arch} in
      (pkg, repo) :: acc
    with e ->
      warn "epoch %s from repoquery: %s" epoch' (ExnHelper.string_of_exn e) ;
      (* The error should not block update. Ingore it. *)
      acc
  )
  | _ ->
      warn "Can't parse line of repoquery '%s'" line ;
      acc

let get_installed_pkgs () =
  let fmt = get_repoquery_fmt () in
  let params = ["-a"; "--pkgnarrow=installed"; "--qf"; fmt] in
  Helpers.call_script !Xapi_globs.repoquery_cmd params
  |> Astring.String.cuts ~sep:"\n"
  |> List.fold_left parse_line_of_repoquery []
  |> List.map (fun (pkg, _) -> (Pkg.to_name_arch_string pkg, pkg))

let get_updates_from_repoquery repositories =
  let fmt = get_repoquery_fmt () in
  let params =
    [
      "-a"
    ; "--plugins"
    ; "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
    ; "--pkgnarrow=updates"
    ; "--qf"
    ; fmt
    ]
  in
  List.iter (fun r -> clean_yum_cache r) repositories ;
  Helpers.call_script !Xapi_globs.repoquery_cmd params
  |> Astring.String.cuts ~sep:"\n"
  |> List.fold_left parse_line_of_repoquery []

let validate_latest_updates ~latest_updates ~accumulative_updates =
  List.map
    (fun (pkg, repo) ->
      match List.assoc_opt pkg accumulative_updates with
      | Some uid ->
          (pkg, Some uid, repo)
      | None ->
          warn "Not found update ID for update %s" (Pkg.to_fullname pkg) ;
          (pkg, None, repo)
    )
    latest_updates

let prune_by_latest_updates latest_updates pkg uid =
  let open Pkg in
  let is_same_name_arch pkg1 pkg2 =
    pkg1.name = pkg2.name && pkg1.arch = pkg2.arch
  in
  match
    List.find_opt (fun (pkg', _) -> is_same_name_arch pkg pkg') latest_updates
  with
  | Some (pkg', repo) ->
      if
        Pkg.gt pkg.epoch pkg.version pkg.release pkg'.epoch pkg'.version
          pkg'.release
      then
        let msg =
          Printf.sprintf
            "Found an accumulative update which is even newer than the latest \
             update: %s"
            (Pkg.to_fullname pkg)
        in
        Error (Some msg)
      else
        Ok (pkg, uid, repo)
  | None ->
      let msg =
        Printf.sprintf
          "Found an accumulative update but this package (name.arch) is not in \
           latest updates: %s"
          (Pkg.to_fullname pkg)
      in
      Error (Some msg)

let prune_by_installed_pkgs installed_pkgs pkg uid repo =
  let open Pkg in
  let name_arch = to_name_arch_string pkg in
  match List.assoc_opt name_arch installed_pkgs with
  | Some pkg' ->
      if
        Pkg.gt pkg.epoch pkg.version pkg.release pkg'.epoch pkg'.version
          pkg'.release
      then
        Ok (pkg, uid, repo)
      else (* An out-dated update *)
        Error None
  | None ->
      let msg =
        Printf.sprintf
          "Found an accumulative update but this package (name.arch) is not \
           installed: %s"
          (to_fullname pkg)
      in
      Error (Some msg)

let prune_accumulative_updates ~accumulative_updates ~latest_updates
    ~installed_pkgs =
  List.filter_map
    (fun (pkg, uid) ->
      let open Rresult.R.Infix in
      ( prune_by_latest_updates latest_updates pkg uid
      >>= fun (pkg', uid', repo') ->
        prune_by_installed_pkgs installed_pkgs pkg' uid' repo'
      )
      |> function
      | Ok (pkg', uid', repo') ->
          Some (pkg', Some uid', repo')
      | Error (Some msg) ->
          warn "%s" msg ; None
      | Error None ->
          None
    )
    accumulative_updates

let get_update_in_json ~installed_pkgs (new_pkg, update_id, repo) =
  let remove_prefix prefix s =
    match Astring.String.cut ~sep:prefix s with
    | Some ("", s') when s' <> "" ->
        Some s'
    | _ ->
        None
  in
  match remove_prefix (!Xapi_globs.local_repository_prefix ^ "-") repo with
  | Some repo_name -> (
      let open Pkg in
      let uid_in_json =
        match update_id with Some s -> `String s | None -> `Null
      in
      let l =
        [
          ("name", `String new_pkg.name)
        ; ("arch", `String new_pkg.arch)
        ; ("newEpochVerRel", to_epoch_ver_rel_json new_pkg)
        ; ("updateId", uid_in_json)
        ; ("repository", `String repo_name)
        ]
      in
      let name_arch = Pkg.to_name_arch_string new_pkg in
      match List.assoc_opt name_arch installed_pkgs with
      | Some old_pkg ->
          `Assoc (l @ [("oldEpochVerRel", to_epoch_ver_rel_json old_pkg)])
      | None ->
          `Assoc l
    )
  | None ->
      let msg = "Found update from unmanaged repository" in
      error "%s: %s" msg repo ;
      raise Api_errors.(Server_error (internal_error, [msg]))

let consolidate_updates_of_host ~repository_name ~updates_info host
    updates_of_host =
  let accumulative_updates =
    updates_of_host
    |> Yojson.Basic.Util.member "accumulative_updates"
    |> Yojson.Basic.Util.to_list
    |> List.map Update.of_json
  in
  let latest_updates =
    updates_of_host
    |> Yojson.Basic.Util.member "updates"
    |> Yojson.Basic.Util.to_list
    |> List.map Update.of_json
  in
  (* 'rpms' come from latest updates *)
  let rpms =
    List.map
      (fun u ->
        Pkg.(
          to_fullname
            {
              name= u.Update.name
            ; arch= u.Update.arch
            ; epoch= u.Update.new_epoch
            ; version= u.Update.new_version
            ; release= u.Update.new_release
            }
        )
      )
      latest_updates
  in
  let open Update in
  (* The update IDs and guidances come from accumulative updates *)
  let acc_uids, acc_updates =
    List.fold_left
      (fun (acc_uids', acc_updates') u ->
        match (u.update_id, u.repository = repository_name) with
        | Some id, true ->
            (UpdateIdSet.add id acc_uids', u :: acc_updates')
        | _ ->
            (acc_uids', acc_updates')
      )
      (UpdateIdSet.empty, []) accumulative_updates
  in
  let rec_guidances =
    eval_guidances ~updates_info ~updates:acc_updates ~kind:Recommended
  in
  let abs_guidances_in_json =
    let abs_guidances =
      List.filter
        (fun g -> not (List.mem g rec_guidances))
        (eval_guidances ~updates_info ~updates:acc_updates ~kind:Absolute)
    in
    List.map (fun g -> `String (Guidance.to_string g)) abs_guidances
  in
  let rec_guidances_in_json =
    List.map (fun g -> `String (Guidance.to_string g)) rec_guidances
  in
  let json_of_host =
    `Assoc
      [
        ("ref", `String host)
      ; ("recommended-guidance", `List rec_guidances_in_json)
      ; ("absolute-guidance", `List abs_guidances_in_json)
      ; ("RPMS", `List (List.map (fun r -> `String r) rpms))
      ; ( "updates"
        , `List
            (List.map (fun uid -> `String uid) (UpdateIdSet.elements acc_uids))
        )
      ]
  in
  (json_of_host, acc_uids)

let append_by_key l k v =
  (* Append a (k, v) into a assoc list l [ (k1, [v1]); (k2, [...]); ... ] as
   * [ (k1, [v1; v]); (k2, [...]); ... ] if k1 = k.
   * Otherwise, if no key in l is equal to k, add a new element (k, [v]) into l. *)
  let vals, others =
    List.fold_left
      (fun (acc_vals_of_k, acc_others) (x, y) ->
        if x = k then
          (List.rev_append y acc_vals_of_k, acc_others)
        else
          (acc_vals_of_k, (x, y) :: acc_others)
      )
      ([v], [])
      l
  in
  (k, vals) :: others

let get_singleton = function
  | [s] ->
      s
  | [] ->
      raise Api_errors.(Server_error (no_repository_enabled, []))
  | _ ->
      raise Api_errors.(Server_error (multiple_update_repositories_enabled, []))

let get_single_enabled_update_repository ~__context =
  let enabled_update_repositories =
    List.filter
      (fun r -> Db.Repository.get_update ~__context ~self:r)
      (get_enabled_repositories ~__context)
  in
  get_singleton enabled_update_repositories

let with_access_token ~token ~token_id f =
  match (token, token_id) with
  | t, tid when t <> "" && tid <> "" ->
      info "sync updates with token_id: %s" tid ;
      let json = `Assoc [("token", `String t); ("token_id", `String tid)] in
      let tmpfile, tmpch =
        Filename.open_temp_file ~mode:[Open_text] "accesstoken" ".json"
      in
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () ->
          output_string tmpch (Yojson.Basic.to_string json) ;
          close_out tmpch ;
          f (Some tmpfile)
        )
        (fun () -> Unixext.unlink_safe tmpfile)
  | t, tid when t = "" && tid = "" ->
      f None
  | _ ->
      let msg = Printf.sprintf "%s: The token or token_id is empty" __LOC__ in
      raise Api_errors.(Server_error (internal_error, [msg]))
