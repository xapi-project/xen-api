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

module Pkg = struct
  type t = {name: string; version: string; release: string; arch: string}

  let error_msg = Printf.sprintf "Failed to split RPM full name '%s'"

  let of_fullname s =
    (* s I.E. "libpath-utils-0.2.1-29.el7.x86_64" *)
    let r = Re.Str.regexp "^\\(.+\\)\\.\\(noarch\\|x86_64\\)$" in
    match Re.Str.string_match r s 0 with
    | true -> (
      try
        let pkg = Re.Str.replace_first r "\\1" s in
        (* pkg is, e.g. "libpath-utils-0.2.1-29.el7" *)
        let arch = Re.Str.replace_first r "\\2" s in
        (* arch is "<noarch|x86_64>" *)
        let pos1 = String.rindex pkg '-' in
        let pos2 = String.rindex_from pkg (pos1 - 1) '-' in
        let release =
          String.sub pkg (pos1 + 1) (String.length pkg - pos1 - 1)
        in
        let version = String.sub pkg (pos2 + 1) (pos1 - pos2 - 1) in
        let name = String.sub pkg 0 pos2 in
        Some {name; version; release; arch}
      with e ->
        let msg = error_msg s in
        error "%s: %s" msg (ExnHelper.string_of_exn e) ;
        raise Api_errors.(Server_error (internal_error, [msg]))
    )
    | false ->
        None

  let to_ver_rel_json pkg =
    `Assoc [("version", `String pkg.version); ("release", `String pkg.release)]

  let to_name_arch_string pkg = pkg.name ^ "." ^ pkg.arch
end

module Update = struct
  type t = {
      name: string
    ; arch: string
    ; old_version: string option
    ; old_release: string option
    ; new_version: string
    ; new_release: string
    ; update_id: string option
    ; repository: string
  }

  let of_json j =
    let open Yojson.Basic.Util in
    {
      name= member "name" j |> to_string
    ; arch= member "arch" j |> to_string
    ; new_version= member "newVerRel" j |> member "version" |> to_string
    ; new_release= member "newVerRel" j |> member "release" |> to_string
    ; old_version=
        ( try Some (member "oldVerRel" j |> member "version" |> to_string)
          with _ -> None
        )
    ; old_release=
        ( try Some (member "oldVerRel" j |> member "release" |> to_string)
          with _ -> None
        )
    ; update_id= member "updateId" j |> to_string_option
    ; repository= member "repository" j |> to_string
    }

  let to_string u =
    Printf.sprintf "%s.%s %s-%s -> %s-%s from %s:%s" u.name u.arch
      (Option.value u.old_version ~default:"unknown")
      (Option.value u.old_release ~default:"unknown")
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

  let eq_set3 = eq [RestartDeviceModel; EvacuateHost]

  let eq_set4 = eq [EvacuateHost; RestartToolstack; RestartDeviceModel]

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
    | l when eq_set3 l ->
        (* RestartDeviceModel and EvacuateHost *)
        ()
    | l when eq_set4 l ->
        (* EvacuateHost, RestartToolstack and RestartDeviceModel *)
        ()
    | l ->
        let msg = error_msg l in
        raise Api_errors.(Server_error (internal_error, [msg]))

  let resort_guidances ~kind gs =
    match (find_opt RebootHost gs, kind) with
    | Some _, _ ->
        singleton RebootHost
    | None, Recommended ->
        gs
    | None, Absolute ->
        filter (fun g -> g <> EvacuateHost) gs
end

module Applicability = struct
  type inequality = Lt | Eq | Gt | Lte | Gte

  type order = LT | EQ | GT

  let string_of_order = function LT -> "<" | EQ -> "=" | GT -> ">"

  type t = {
      name: string
    ; arch: string
    ; inequality: inequality option
    ; epoch: string
    ; version: string
    ; release: string
  }

  type segment_of_version = Int of int | Str of string

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
    {name= ""; arch= ""; inequality= None; epoch= ""; version= ""; release= ""}

  let assert_valid = function
    | {name= ""; _}
    | {arch= ""; _}
    | {inequality= None; _}
    | {epoch= ""; _}
    | {version= ""; _}
    | {release= ""; _} ->
        error "Invalid applicability" ;
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
    | a ->
        a

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
            | Xml.Element ("epoch", _, [Xml.PCData v]) ->
                {a with epoch= v}
            | Xml.Element ("version", _, [Xml.PCData v]) ->
                {a with version= v}
            | Xml.Element ("release", _, [Xml.PCData v]) ->
                {a with release= v}
            | Xml.Element ("name", _, [Xml.PCData v]) ->
                {a with name= v}
            | Xml.Element ("arch", _, [Xml.PCData v]) ->
                {a with arch= v}
            | _ ->
                error "Unknown node in <applicability>" ;
                raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
            )
          default children
        |> assert_valid
    | _ ->
        error "Unknown node in <guidance_applicabilities>" ;
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))

  let to_string a =
    Printf.sprintf "%s %s %s-%s (epoch: %s)" a.name
      (Option.value
         (Option.map string_of_inequality a.inequality)
         ~default:"InvalidInequality"
      )
      a.version a.release a.epoch

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
        let r = Re.Str.regexp "^\\([^0-9]+\\)\\([0-9]+\\)$" in
        if Re.Str.string_match r s 0 then
          [Re.Str.replace_first r "\\1" s; Re.Str.replace_first r "\\2" s]
        else
          [s]
      in
      let concat_map f l = List.concat (List.map f l) in
      v
      |> Astring.String.cuts ~sep:"."
      |> concat_map (fun s -> Astring.String.cuts ~sep:"_" s)
      |> concat_map (fun s -> split_letters_and_numbers s)
      |> List.map (fun s ->
             let r = Re.Str.regexp "^[0-9]+$" in
             if Re.Str.string_match r s 0 then
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

  let lt v1 r1 v2 r2 =
    match (compare_version_strings v1 v2, compare_version_strings r1 r2) with
    | LT, _ | EQ, LT ->
        true
    | _ ->
        false

  let gt v1 r1 v2 r2 =
    match (compare_version_strings v1 v2, compare_version_strings r1 r2) with
    | GT, _ | EQ, GT ->
        true
    | _ ->
        false

  let eq v1 r1 v2 r2 =
    match (compare_version_strings v1 v2, compare_version_strings r1 r2) with
    | EQ, EQ ->
        true
    | _ ->
        false

  let lte v1 r1 v2 r2 = lt v1 r1 v2 r2 || eq v1 r1 v2 r2

  let gte v1 r1 v2 r2 = gt v1 r1 v2 r2 || eq v1 r1 v2 r2

  let eval ~version ~release ~applicability =
    let ver = applicability.version in
    let rel = applicability.release in
    match applicability.inequality with
    | Some Lt ->
        lt version release ver rel
    | Some Lte ->
        lte version release ver rel
    | Some Gt ->
        gt version release ver rel
    | Some Gte ->
        gte version release ver rel
    | Some Eq ->
        eq version release ver rel
    | _ ->
        raise Invalid_inequality
end

module UpdateInfoMetaData = struct
  type t = {checksum: string; location: string}

  let assert_valid_repomd = function
    | {checksum= ""; _} | {location= ""; _} ->
        error "Missing 'checksum' or 'location' in updateinfo in repomod.xml" ;
        raise Api_errors.(Server_error (invalid_repomd_xml, []))
    | umd ->
        umd

  let of_xml = function
    | Xml.Element ("repomd", _, children) -> (
        let get_updateinfo_node = function
          | Xml.Element ("data", attrs, nodes) -> (
            match List.assoc_opt "type" attrs with
            | Some "updateinfo" ->
                Some nodes
            | _ ->
                None
          )
          | _ ->
              None
        in
        match List.filter_map get_updateinfo_node children with
        | [l] ->
            List.fold_left
              (fun md n ->
                match n with
                | Xml.Element ("checksum", _, [Xml.PCData v]) ->
                    {md with checksum= v}
                | Xml.Element ("location", attrs, _) -> (
                  try {md with location= List.assoc "href" attrs}
                  with _ ->
                    error "Failed to get location of updateinfo.xml.gz" ;
                    raise Api_errors.(Server_error (invalid_repomd_xml, []))
                )
                | _ ->
                    md
                )
              {checksum= ""; location= ""}
              l
            |> assert_valid_repomd
        | _ ->
            error "Missing or multiple 'updateinfo' node(s)" ;
            raise Api_errors.(Server_error (invalid_repomd_xml, []))
      )
    | _ ->
        error "Missing 'repomd' node" ;
        raise Api_errors.(Server_error (invalid_repomd_xml, []))

  let of_xml_file xml_path =
    match Sys.file_exists xml_path with
    | false ->
        error "No repomd.xml found: %s" xml_path ;
        raise Api_errors.(Server_error (invalid_repomd_xml, []))
    | true -> (
      match Xml.parse_file xml_path with
      | xml ->
          of_xml xml
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
    | {id= ""; _}
    | {summary= ""; _}
    | {description= ""; _}
    | {update_type= ""; _} ->
        error
          "One or more of 'id', 'summary', 'description' and 'update_type' \
           is/are missing" ;
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
                      | Xml.Element ("guidance_applicabilities", _, apps) ->
                          {
                            acc with
                            guidance_applicabilities=
                              List.map Applicability.of_xml apps
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
    ~binary_url ~source_url ~update =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.Repository.create ~__context ~ref ~uuid ~name_label ~name_description
    ~binary_url ~source_url ~update ~hash:"" ~up_to_date:false ;
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

let write_yum_config ?(source_url = None) binary_url repo_name =
  let file_path =
    Filename.concat !Xapi_globs.yum_repos_config_dir (repo_name ^ ".repo")
  in
  let gpgkey_path =
    Filename.concat !Xapi_globs.rpm_gpgkey_dir
      !Xapi_globs.repository_gpgkey_name
  in
  if !Xapi_globs.repository_gpgcheck then (
    if not (Sys.file_exists gpgkey_path) then
      raise
        Api_errors.(
          Server_error (internal_error, ["gpg key file does not exist"])
        ) ;
    if not ((Unix.lstat gpgkey_path).Unix.st_kind = Unix.S_REG) then
      raise
        Api_errors.(
          Server_error (internal_error, ["gpg key file is not a regular file"])
        )
  ) ;
  let content_of_binary =
    [
      Printf.sprintf "[%s]" repo_name
    ; Printf.sprintf "name=%s" repo_name
    ; Printf.sprintf "baseurl=%s" binary_url
    ; "enabled=0"
    ; (if !Xapi_globs.repository_gpgcheck then "gpgcheck=1" else "gpgcheck=0")
    ; ( if !Xapi_globs.repository_gpgcheck then
          Printf.sprintf "gpgkey=file://%s" gpgkey_path
      else
        ""
      )
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
        ; ( if !Xapi_globs.repository_gpgcheck then
              "gpgcheck=1"
          else
            "gpgcheck=0"
          )
        ; ( if !Xapi_globs.repository_gpgcheck then
              Printf.sprintf "gpgkey=file://%s" gpgkey_path
          else
            ""
          )
        ]
  in
  let content = String.concat "\n" (content_of_binary @ content_of_source) in
  Unixext.atomic_write_to_file file_path 0o400 (fun fd ->
      Unixext.really_write_string fd content |> ignore
  )

let get_cachedir repo_name =
  let config_params = [repo_name] in
  Helpers.call_script !Xapi_globs.yum_config_manager_cmd config_params
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter_map (fun kv ->
         match Astring.String.is_prefix ~affix:"cachedir =" kv with
         | true ->
             Some (Scanf.sscanf kv "cachedir = %s" (fun x -> x))
         | false ->
             None
     )
  |> function
  | [x] ->
      x
  | _ ->
      let msg =
        Printf.sprintf "Not found cachedir for repository %s" repo_name
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
  let enabled = get_enabled_repositories ~__context in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      let repositories =
        List.map
          (fun repository ->
            let repo_name =
              get_local_repository_name ~__context ~self:repository
            in
            let url =
              Printf.sprintf "https://%s/repository/%s/" master_addr
                (get_remote_repository_name ~__context ~self:repository)
            in
            remove_repo_conf_file repo_name ;
            write_yum_config url repo_name ;
            clean_yum_cache repo_name ;
            let config_params =
              [
                "--save"
              ; Printf.sprintf "--setopt=%s.sslverify=false" repo_name
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

let get_installed_pkgs () =
  Helpers.call_script !Xapi_globs.rpm_cmd ["-qa"]
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter_map Pkg.of_fullname
  |> List.map (fun pkg -> (Pkg.to_name_arch_string pkg, pkg))

let parse_updateinfo_list acc line =
  let sep = Re.Str.regexp " +" in
  match Re.Str.split sep line with
  | [update_id; _; full_name] -> (
    match Pkg.of_fullname full_name with
    | Some pkg -> (
        (* Found same package in more than 1 update *)
        let name_arch = Pkg.to_name_arch_string pkg in
        match List.assoc_opt name_arch acc with
        | Some (v, r, _) -> (
            let open Applicability in
            (* Select the latest update by comparing version and release  *)
            match
              ( compare_version_strings v pkg.Pkg.version
              , compare_version_strings r pkg.Pkg.release
              )
            with
            | LT, _ | EQ, LT | EQ, EQ ->
                let latest_so_far =
                  (name_arch, (pkg.Pkg.version, pkg.Pkg.release, update_id))
                in
                latest_so_far :: List.remove_assoc name_arch acc
            | _ ->
                acc
          )
        | None ->
            (name_arch, (pkg.Pkg.version, pkg.Pkg.release, update_id)) :: acc
      )
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
  |> List.map (fun (name_arch, (_, _, update_id)) -> (name_arch, update_id))

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
            match (update.old_version, update.old_release) with
            | Some old_version, Some old_release ->
                Applicability.eval ~version:old_version ~release:old_release
                  ~applicability:a
            | _ ->
                warn "No installed version or release for package %s.%s"
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

let get_rpm_update_in_json ~rpm2updates ~installed_pkgs line =
  let sep = Re.Str.regexp " +" in
  match Re.Str.split sep line with
  | ["Updated"; "Packages"] ->
      None
  | [name_arch; ver_rel; repo] -> (
      (* xsconsole.x86_64  10.1.11-34  local-normal *)
      let remove_prefix prefix s =
        match Astring.String.cut ~sep:prefix s with
        | Some ("", s') when s' <> "" ->
            Some s'
        | _ ->
            None
      in
      match remove_prefix (!Xapi_globs.local_repository_prefix ^ "-") repo with
      | Some repo_name -> (
        match
          ( Astring.String.cuts ~sep:"." name_arch
          , Astring.String.cuts ~sep:"-" ver_rel
          )
        with
        | [name; arch], [version; release] -> (
            let open Pkg in
            let new_pkg = {name; version; release; arch} in
            let uid_in_json =
              match List.assoc_opt name_arch rpm2updates with
              | Some s ->
                  `String s
              | None ->
                  warn "No update ID found for %s" name_arch ;
                  `Null
            in
            let l =
              [
                ("name", `String name)
              ; ("arch", `String arch)
              ; ("newVerRel", to_ver_rel_json new_pkg)
              ; ("updateId", uid_in_json)
              ; ("repository", `String repo_name)
              ]
            in
            match List.assoc_opt name_arch installed_pkgs with
            | Some old_pkg ->
                Some (`Assoc (l @ [("oldVerRel", to_ver_rel_json old_pkg)]))
            | None ->
                Some (`Assoc l)
          )
        | _ ->
            warn "Can't parse %s and %s" name_arch ver_rel ;
            None
      )
      | None ->
          let msg = "Found update from unmanaged repository" in
          error "%s: %s" msg line ;
          raise Api_errors.(Server_error (internal_error, [msg]))
    )
  | _ ->
      debug "Ignore unrecognized line '%s' in parsing updates list" line ;
      None

let consolidate_updates_of_host ~repository_name ~updates_info host
    updates_of_host =
  let all_updates =
    updates_of_host
    |> Yojson.Basic.Util.member "updates"
    |> Yojson.Basic.Util.to_list
    |> List.map Update.of_json
  in
  let open Update in
  let rpms, uids, updates =
    List.fold_left
      (fun (acc_rpms, acc_uids, acc_updates) u ->
        let rpms =
          Printf.sprintf "%s-%s-%s.%s.rpm" u.name u.new_version u.new_release
            u.arch
          :: acc_rpms
        in
        match (u.update_id, u.repository = repository_name) with
        | Some id, true ->
            (rpms, UpdateIdSet.add id acc_uids, u :: acc_updates)
        | _ ->
            (rpms, acc_uids, acc_updates)
        )
      ([], UpdateIdSet.empty, [])
      all_updates
  in
  let rec_guidances = eval_guidances ~updates_info ~updates ~kind:Recommended in
  let abs_guidances_in_json =
    let abs_guidances =
      List.filter
        (fun g -> not (List.mem g rec_guidances))
        (eval_guidances ~updates_info ~updates ~kind:Absolute)
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
        , `List (List.map (fun uid -> `String uid) (UpdateIdSet.elements uids))
        )
      ]
  in
  (json_of_host, uids)

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
