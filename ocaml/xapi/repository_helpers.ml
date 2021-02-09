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

module GuidanceStrSet = Set.Make (String)

let exposing_pool_repo_mutex = Mutex.create ()

module Pkg = struct
  type t = {
      name: string
    ; version: string
    ; release: string
    ; arch: string
  }

  let of_fullname s =
    (* s I.E. "libpath-utils-0.2.1-29.el7.x86_64" *)
    let r = Re.Str.regexp "^\\(.+\\)\\.\\(noarch\\|x86_64\\)$" in
    match Re.Str.string_match r s 0 with
    | true ->
      (try
         let pkg = Re.Str.replace_first r "\\1" s in (* pkg is, e.g. "libpath-utils-0.2.1-29.el7" *)
         let arch = Re.Str.replace_first r "\\2" s in (* arch is "<noarch|x86_64>" *)
         let pos1 = String.rindex pkg '-' in
         let pos2 = String.rindex_from pkg (pos1 - 1) '-' in
         let release = String.sub pkg (pos1 + 1) ((String.length pkg) - pos1 - 1) in
         let version = String.sub pkg (pos2 + 1) (pos1 - pos2 - 1) in
         let name = String.sub pkg 0 pos2 in
         Some { name; version; release; arch }
       with e ->
         let msg =
           Printf.sprintf "Failed to split rpm name '%s': %s" s (ExnHelper.string_of_exn e) in
         raise Api_errors.(Server_error (internal_error, [msg])))
    | false -> None

  let to_ver_rel_json pkg =
    `Assoc [
      ("version", `String pkg.version);
      ("release", `String pkg.release);
    ]

  let to_name_arch_string pkg =
    pkg.name ^ "." ^ pkg.arch
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
  }

  let of_json j =
    let open Yojson.Basic.Util in
    {
      name = (member "name" j |> to_string);
      arch = (member "arch" j |> to_string);
      new_version = (member "newVerRel" j |> member "version" |> to_string);
      new_release = (member "newVerRel" j |> member "release" |> to_string);
      old_version = (try Some (member "oldVerRel" j |> member "version" |> to_string)
                     with _ -> None);
      old_release = (try Some (member "oldVerRel" j |> member "release" |> to_string)
                     with _ -> None);
      update_id = (member "updateId" j |> to_string_option)
    }
end

module Guidance = struct
  type t =
    | RebootHost
    | RestartToolstack
    | EvacuateHost
    | RestartDeviceModel

  type guidance_kind = Absolute | Recommended

  let to_string = function
    | RebootHost -> "RebootHost"
    | RestartToolstack -> "RestartToolstack"
    | EvacuateHost -> "EvacuateHost"
    | RestartDeviceModel -> "RestartDeviceModel"

  exception Unknown_guidance

  let of_string = function
    | "RebootHost" -> RebootHost
    | "RestartToolstack" -> RestartToolstack
    | "EvacuateHost" -> EvacuateHost
    | "RestartDeviceModel" -> RestartDeviceModel
    | _ -> raise Unknown_guidance

  let of_xml = function
    | Xml.Element ("guidance", _, [Xml.PCData v]) ->
      of_string v
    | _ ->
      error "Unknown node in <absolute|recommended_guidances>";
      raise Api_errors.(Server_error (invalid_updateinfo_xml, []))

  let set_of_list l =
    l
    |> List.map to_string
    |> GuidanceStrSet.of_list

  let eq l s = GuidanceStrSet.equal (set_of_list l) (set_of_list s)

  let eq_set1 = eq [EvacuateHost; RestartToolstack]

  let eq_set2 = eq [RestartDeviceModel; RestartToolstack]

  let eq_set3 = eq [RestartDeviceModel; EvacuateHost]

  let eq_set4 = eq [EvacuateHost; RestartToolstack; RestartDeviceModel]

  let assert_valid_guidances = function
    | [RebootHost]
    | [EvacuateHost]
    | [RestartToolstack]
    | [RestartDeviceModel] -> ()
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
      let msg =
        Printf.sprintf "Found wrong guidance(s) before applying updates: %s"
          (String.concat ";" (List.map to_string l))
      in
      raise Api_errors.(Server_error (internal_error, [msg]))

  let resort_guidances gs=
    match GuidanceStrSet.find_opt "RebootHost" gs with
    | Some _ -> GuidanceStrSet.singleton "RebootHost"
    | None -> gs
end

module Applicability = struct
  type inequality = Lt | Eq | Gt | Lte | Gte | Invalid

  type order = Lt_ | Eq_ | Gt_

  let string_of_order = function
    | Lt_ -> "<"
    | Eq_ -> "="
    | Gt_ -> ">"

  type t = {
      name: string
    ; arch: string
    ; inequality: inequality
    ; epoch: string
    ; version: string
    ; release: string
  }

  type segment_of_version =
    | Int of int
    | Str of string

  exception Invalid_inequality

  let string_of_inequality = function
    | Lt -> "<"
    | Eq -> "="
    | Gt -> ">"
    | Gte -> ">="
    | Lte -> "<="
    | _ -> raise Invalid_inequality

  let inequality_of_string = function
    | "gte" -> Gte
    | "lte" -> Lte
    | "gt"  -> Gt
    | "lt"  -> Lt
    | "eq"  -> Eq
    | _ -> raise Invalid_inequality

  let default = {
      name = ""
    ; arch = ""
    ; inequality = Invalid
    ; epoch = ""
    ; version = ""
    ; release = ""
    }

  let of_xml = function
    | Xml.Element ("applicability", _, children) ->
      List.fold_left (fun a n ->
          match n with
          | Xml.Element ("inequality", _, [Xml.PCData v]) ->
            { a with inequality = (inequality_of_string v) }
          | Xml.Element ("epoch", _, [Xml.PCData v]) ->
            { a with epoch = v }
          | Xml.Element ("version", _, [Xml.PCData v]) ->
            { a with version = v }
          | Xml.Element ("release", _, [Xml.PCData v]) ->
            { a with release = v }
          | Xml.Element ("name", _, [Xml.PCData v]) ->
            { a with name = v }
          | Xml.Element ("arch", _, [Xml.PCData v]) ->
            { a with arch = v }
          | _ ->
            error "Unknown node in <applicability>";
            raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
      ) default children
    | _ ->
      error "Unknown node in <guidance_applicabilities>";
      raise Api_errors.(Server_error (invalid_updateinfo_xml, []))

  let to_string a =
    Printf.sprintf "%s%s%s-%s"
      a.name (string_of_inequality a.inequality) a.version a.release

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
          [(Re.Str.replace_first r "\\1" s); (Re.Str.replace_first r "\\2" s)]
        else [s]
      in
      let concat_map f l = List.concat (List.map f l) in
      v
      |> Astring.String.cuts ~sep:"."
      |> concat_map (fun s -> Astring.String.cuts ~sep:"_" s)
      |> concat_map (fun s -> split_letters_and_numbers s)
      |> List.map (fun s ->
          match int_of_string s with
          | i ->
            let startswith x =
              Astring.String.is_prefix ~affix:x (String.lowercase_ascii s)
            in
            if List.exists startswith ["0x"; "0b"; "0o"] then Str s else Int i
          | exception _  ->
            Str s)
    in
    let rec compare_segments l1 l2 =
      match l1, l2 with
      | c1 :: t1, c2 :: t2 ->
        begin match c1, c2 with
          | Int s1, Int s2 ->
            if s1 > s2 then Gt_
            else if s1 = s2 then compare_segments t1 t2
            else Lt_
          | Int s1, Str s2 -> Gt_
          | Str s1, Int s2 -> Lt_
          | Str s1, Str s2 ->
            let r = String.compare s1 s2 in
            if r < 0 then Lt_
            else if r > 0 then Gt_
            else compare_segments t1 t2
        end
      | _ :: _, [] -> Gt_
      | [], _ :: _ -> Lt_
      | [], [] -> Eq_
    in
    compare_segments (normalize s1) (normalize s2)

  let lt v1 r1 v2 r2 =
    match (compare_version_strings v1 v2), (compare_version_strings r1 r2) with
    | Lt_, _ | Eq_, Lt_ -> true
    | _ -> false

  let gt v1 r1 v2 r2 =
    match (compare_version_strings v1 v2), (compare_version_strings r1 r2) with
    | Gt_, _ | Eq_, Gt_ -> true
    | _ -> false

  let eq v1 r1 v2 r2 =
    match (compare_version_strings v1 v2), (compare_version_strings r1 r2) with
    | Eq_, Eq_ -> true
    | _ -> false

  let lte v1 r1 v2 r2 = lt v1 r1 v2 r2 || eq v1 r1 v2 r2

  let gte v1 r1 v2 r2 = gt v1 r1 v2 r2 || eq v1 r1 v2 r2

  let eval_applicability ~version ~release ~applicability =
    let ver = applicability.version in
    let rel = applicability.release in
    match applicability.inequality with
    | Lt ->
      lt version release ver rel
    | Lte ->
      lte version release ver rel
    | Gt ->
      gt version release ver rel
    | Gte ->
      gte version release ver rel
    | Eq ->
      eq version release ver rel
    | _ ->
      raise Invalid_inequality
end

module UpdateInfoMetaData = struct
  type t = {
      checksum: string
    ; location: string
  }

  let of_xml_file xml_path =
    let assert_valid_repomd = function
      | { checksum = ""; _ } | { location = ""; _ } ->
        error "Missing 'checksum' or 'location' in updateinfo in repomod.xml";
        raise Api_errors.(Server_error (invalid_repomd_xml, []))
      | umd -> umd
    in
    match Sys.file_exists xml_path with
    | false ->
      error "No repomd.xml found: %s" xml_path;
      raise Api_errors.(Server_error (invalid_repomd_xml, []))
    | true ->
      begin match Xml.parse_file xml_path with
       | Xml.Element ("repomd", _, children) ->
         let get_updateinfo_node = function
           | Xml.Element ("data", attrs, nodes) ->
             (match List.assoc_opt "type" attrs with
              | Some "updateinfo" -> Some nodes
              | _ -> None)
           | _ -> None
         in
         begin match List.filter_map get_updateinfo_node children with
          | [l] ->
            List.fold_left (fun md n ->
                begin match n with
                  | Xml.Element ("checksum", _, [Xml.PCData v]) -> {md with checksum = v}
                  | Xml.Element ("location", attrs, _) ->
                    (try {md with location = (List.assoc "href" attrs)}
                     with _ ->
                       error "Failed to get location of updateinfo.xml.gz";
                       raise Api_errors.(Server_error (invalid_repomd_xml, [])))
                  | _ -> md
                end
              ) { checksum = ""; location = "" } l
            |> assert_valid_repomd
          | _ ->
            error "Missing or multiple 'updateinfo' node(s)";
            raise Api_errors.(Server_error (invalid_repomd_xml, []))
         end
       | _ ->
         error "Missing 'repomd' node";
         raise Api_errors.(Server_error (invalid_repomd_xml, []))
       | exception e ->
         error "Failed to parse repomd.xml: %s" (ExnHelper.string_of_exn e);
         raise Api_errors.(Server_error (invalid_repomd_xml, []))
      end
end

module UpdateInfo = struct
  type t = {
      id: string
    ; summary: string
    ; description: string
    ; rec_guidances: Guidance.t list
    ; abs_guidances: Guidance.t list
    ; guidance_applicabilities: Applicability.t list
    ; spec_info: string
    ; url: string
    ; update_type: string
  }

  let to_json ui =
    `Assoc [
      ("id", `String ui.id);
      ("summary", `String ui.summary);
      ("description", `String ui.description);
      ("special-info", `String ui.spec_info);
      ("URL", `String ui.url);
      ("type", `String ui.update_type);
      ("recommended-guidance", `List (List.map (fun g ->
           `String (Guidance.to_string g)) ui.rec_guidances));
      ("absolute-guidance", `List (List.map (fun g ->
           `String (Guidance.to_string g)) ui.abs_guidances))
    ]

  let to_string ui =
    Printf.sprintf "id=%s rec_guidances=%s abs_guidances=%s guidance_applicabilities=%s"
      ui.id
      (String.concat ";" (List.map Guidance.to_string ui.rec_guidances))
      (String.concat ";" (List.map Guidance.to_string ui.abs_guidances))
      (String.concat ";" (List.map Applicability.to_string ui.guidance_applicabilities))

  let default = {
      id = ""
    ; summary = ""
    ; description = ""
    ; rec_guidances = []
    ; abs_guidances = []
    ; guidance_applicabilities = []
    ; spec_info = ""
    ; url = ""
    ; update_type = ""
    }

  let assert_valid_updateinfo = function
    | { id = ""; _}
    | { summary = ""; _}
    | { description = ""; _}
    | { update_type = ""; _} ->
      error "One or more of 'id', 'summary', 'description' and 'update_type' is/are missing";
      raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
    | ui -> ui

  let of_xml_file xml_file_path =
    match Xml.parse_file xml_file_path with
    | Xml.Element  ("updates", _, children) ->
      List.filter_map (fun n ->
          match n with
          | Xml.Element ("update", attr, update_nodes) ->
            let ty =
              match List.assoc_opt "type" attr with
              | Some ty -> ty
              | None -> ""
            in
            let ui = List.fold_left (fun acc node ->
                match node with
                | Xml.Element ("id", _, [Xml.PCData v]) ->
                  { acc with id = v }
                | Xml.Element ("url", _, [Xml.PCData v]) ->
                  { acc with url = v }
                | Xml.Element ("special_info", _, [Xml.PCData v]) ->
                  { acc with spec_info = v }
                | Xml.Element ("summary", _, [Xml.PCData v]) ->
                  { acc with summary = v }
                | Xml.Element ("description", _, [Xml.PCData v]) ->
                  { acc with description = v }
                | Xml.Element ("recommended_guidances", _, gs) ->
                  { acc with rec_guidances = (List.map Guidance.of_xml gs) }
                | Xml.Element ("absolute_guidances", _, gs) ->
                  { acc with abs_guidances = (List.map Guidance.of_xml gs) }
                | Xml.Element ("guidance_applicabilities", _, apps) ->
                  { acc with guidance_applicabilities = (List.map Applicability.of_xml apps) }
                | _ -> acc
              ) { default with update_type = ty } update_nodes
              |> assert_valid_updateinfo
            in
            debug "updateinfo: %s" (to_string ui);
            Some ui
          | _ -> None
      ) children
      |> List.map (fun updateinfo -> (updateinfo.id, updateinfo))
    | _ ->
      error "Failed to parse updateinfo.xml: missing <updates>";
      raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
    | exception e ->
      error "Failed to parse updateinfo.xml: %s" (ExnHelper.string_of_exn e);
      raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
end

let create_repository_record ~__context ~name_label ~name_description ~binary_url ~source_url =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.Repository.create
    ~__context
    ~ref
    ~uuid
    ~name_label
    ~name_description
    ~binary_url
    ~source_url
    ~hash:""
    ~up_to_date:false;
  ref

let assert_url_is_valid ~url =
  try
    let uri = Uri.of_string url in
    match Uri.scheme uri with
    | Some "http" | Some "https" ->
      begin match Uri.host uri with
        | Some h ->
          begin match !Xapi_globs.repository_domain_name_allowlist with
            | [] -> ()
            | l ->
              begin match List.exists (fun d -> Astring.String.is_suffix ("." ^ d) h) l with
                | true -> ()
                | false ->
                  let msg = "host is not in allowlist" in
                  raise Api_errors.(Server_error (internal_error, [msg]))
              end
          end
        | None -> raise Api_errors.(Server_error (internal_error, ["invalid host in url"]))
      end
    | _ -> raise Api_errors.(Server_error (internal_error, ["invalid scheme in url"]))
  with e ->
    error "Invalid url %s: %s" url (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (invalid_base_url, [url]))

let with_pool_repository f =
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
        Mutex.lock exposing_pool_repo_mutex;
        f ())
    (fun () -> Mutex.unlock exposing_pool_repo_mutex)

let is_local_pool_repo_enabled () =
  if Mutex.try_lock exposing_pool_repo_mutex then
    (Mutex.unlock exposing_pool_repo_mutex;
     false)
  else true

let with_updateinfo_xml gz_path f =
  let tmpfile, tmpch = Filename.open_temp_file ~mode:[Open_text] "updateinfo" ".xml" in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () ->
           try
             (Unixext.with_file gz_path [Unix.O_RDONLY] 0o0 @@ fun gz_fd_in ->
              Gzip.decompress_passive gz_fd_in @@ fun fd_in ->
              let ic = Unix.in_channel_of_descr fd_in in
              try
                while true do
                  let line = input_line ic in
                  output_string tmpch (line ^ "\n")
                done
              with End_of_file -> ())
           with e ->
             error "Failed to decompress updateinfo.xml.gz: %s" (ExnHelper.string_of_exn e);
             raise Api_errors.(Server_error (invalid_updateinfo_xml, [])))
        (fun () -> close_out tmpch);
      f tmpfile)
    (fun () -> Unixext.unlink_safe tmpfile)

let clean_yum_cache name =
  try
    let params =
        [
          "--disablerepo=*";
          Printf.sprintf "--enablerepo=%s" name;
          "clean"; "all"
        ]
    in
    ignore (Helpers.call_script !Xapi_globs.yum_cmd params)
  with e ->
    warn "Unable to clean YUM cache for %s: %s" name (ExnHelper.string_of_exn e)

let remove_repo_conf_file repo_name =
  let path = Filename.concat !Xapi_globs.yum_repos_config_dir (repo_name ^ ".repo") in
  Unixext.unlink_safe path

let write_yum_config ?(source_url=None) binary_url repo_name =
  let file_path = Filename.concat !Xapi_globs.yum_repos_config_dir (repo_name ^ ".repo") in
  let gpgkey_path =
    Filename.concat !Xapi_globs.rpm_gpgkey_dir !Xapi_globs.repository_gpgkey_name
  in
  if !Xapi_globs.repository_gpgcheck then
    (if not (Sys.file_exists gpgkey_path) then
       raise Api_errors.(Server_error (internal_error, ["gpg key file does not exist"]));
     if not ( (Unix.lstat gpgkey_path).Unix.st_kind = Unix.S_REG ) then
       raise Api_errors.(Server_error (internal_error,
                                       ["gpg key file is not a regular file"])));
  let content_of_binary =
    [
      Printf.sprintf "[%s]" repo_name;
      Printf.sprintf "name=%s" repo_name;
      Printf.sprintf "baseurl=%s" binary_url;
      "enabled=0";
      if !Xapi_globs.repository_gpgcheck then "gpgcheck=1" else "gpgcheck=0";
      if !Xapi_globs.repository_gpgcheck then
        Printf.sprintf "gpgkey=file://%s" gpgkey_path
      else ""
    ]
  in
  let content_of_source =
    match source_url with
    | None -> []
    | Some url ->
      [
        "";
        Printf.sprintf "[%s-source]" repo_name;
        Printf.sprintf "name=%s-source" repo_name;
        Printf.sprintf "baseurl=%s" url;
        "enabled=0";
        if !Xapi_globs.repository_gpgcheck then "gpgcheck=1" else "gpgcheck=0";
        if !Xapi_globs.repository_gpgcheck then
          Printf.sprintf "gpgkey=file://%s" gpgkey_path
        else ""
      ]
  in
  let content = String.concat "\n" (content_of_binary @ content_of_source) in
  Unixext.atomic_write_to_file file_path 0o400 (fun fd ->
    Unixext.really_write_string fd content |> ignore)

let get_cachedir repo_name =
  let config_params = [repo_name] in
  Helpers.call_script !Xapi_globs.yum_config_manager_cmd config_params
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter_map (fun kv ->
      match Astring.String.is_prefix ~affix:"cachedir =" kv with
      | true -> Some (Scanf.sscanf kv "cachedir = %s" (fun x -> x))
      | false -> None)
  |> (function
      | [x] -> x
      | _ ->
        let msg = Printf.sprintf "Not found cachedir for repository %s" repo_name in
        raise Api_errors.(Server_error (internal_error, [msg])))

let with_local_repository ~__context f =
  let master_addr =
    try Pool_role.get_master_address ()
    with Pool_role.This_host_is_a_master ->
      Option.get (Helpers.get_management_ip_addr ~__context)
  in
  let url = Printf.sprintf "https://%s/repository/" master_addr in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      remove_repo_conf_file !Xapi_globs.local_repo_name;
      write_yum_config url !Xapi_globs.local_repo_name;
      let config_params =
          [
            "--save";
            Printf.sprintf "--setopt=%s.sslverify=false" !Xapi_globs.local_repo_name;
            !Xapi_globs.local_repo_name
          ]
      in
      ignore (Helpers.call_script !Xapi_globs.yum_config_manager_cmd config_params);
      f ())
    (fun () -> remove_repo_conf_file !Xapi_globs.local_repo_name)

let assert_yum_error output =
  let errors = ["Updateinfo file is not valid XML"] in
  List.iter (fun err ->
    if Astring.String.is_infix ~affix:err output then (
      error "Error from 'yum updateinfo list': %s" err;
      raise Api_errors.(Server_error (invalid_updateinfo_xml, [])))
    else ()) errors;
  output

let get_installed_pkgs () =
  Helpers.call_script !Xapi_globs.rpm_cmd ["-qa"]
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter_map Pkg.of_fullname
  |> List.map (fun pkg -> ((Pkg.to_name_arch_string pkg), pkg))

let get_updates_from_updateinfo () =
  let params_of_updateinfo_list =
    [
      "-q"; "--disablerepo=*"; Printf.sprintf "--enablerepo=%s" !Xapi_globs.local_repo_name;
      "updateinfo"; "list"; "updates";
    ]
  in
  let sep = Re.Str.regexp " +" in
  Helpers.call_script !Xapi_globs.yum_cmd params_of_updateinfo_list
  |> assert_yum_error
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter_map (fun line ->
      match Re.Str.split sep line with
      | update_id :: _ :: full_name :: []  ->
        begin match Pkg.of_fullname full_name with
          | Some pkg -> Some ((Pkg.to_name_arch_string pkg), update_id)
          | None -> None
        end
      | _ ->
        debug "Ignore unrecognized line '%s' in parsing updateinfo list" line;
        None)

let eval_guidance_for_one_update ~updates_info ~update ~kind =
  let open Update in
  match update.update_id with
  | Some uid ->
    begin match List.assoc_opt uid updates_info with
      | Some updateinfo ->
        let is_applicable (a : Applicability.t) =
          match update.name = a.Applicability.name with
          | true ->
            begin match update.old_version, update.old_release with
              | Some old_version, Some old_release ->
                Applicability.eval_applicability
                  ~version:old_version ~release:old_release ~applicability:a
              | _ ->
                warn "No installed version or release for package %s.%s"
                  update.name update.arch;
                false
            end
          | _ -> false
        in
        let dbg_msg r =
            Printf.sprintf
              "Evaluating applicability for package %s.%s in update %s returned '%s'"
              update.name update.arch uid (string_of_bool r)
        in
        let apps = updateinfo.UpdateInfo.guidance_applicabilities in
        begin match (List.exists is_applicable apps), apps with
          | true, _ | false, [] ->
            debug "%s" (dbg_msg true);
            begin match kind with
              | Guidance.Absolute ->
                List.map Guidance.to_string updateinfo.UpdateInfo.abs_guidances
              | Guidance.Recommended ->
                List.map Guidance.to_string updateinfo.UpdateInfo.rec_guidances
            end
            |> GuidanceStrSet.of_list
          | _ ->
            debug "%s" (dbg_msg false);
            GuidanceStrSet.empty
        end
      | None ->
        let msg = Printf.sprintf "Can't find update ID %s from updateinfo.xml" uid in
        raise Api_errors.(Server_error (internal_error, [msg]))
    end
  | None ->
    warn "Ignore evaluating against package %s.%s as its update ID is missing"
      update.name update.arch;
    GuidanceStrSet.empty

let eval_guidances ~updates_info ~updates ~kind =
  List.fold_left (fun acc u ->
      GuidanceStrSet.union acc
        (eval_guidance_for_one_update ~updates_info ~update:u ~kind)
  ) GuidanceStrSet.empty updates
  |> Guidance.resort_guidances
  |> GuidanceStrSet.elements
  |> List.map Guidance.of_string
