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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "repository" end)

open D

module UpdateIdSet = Set.Make (String)

module GuidanceStrSet = Set.Make (String)

let owner_ro = 0o400
let capacity_in_parallel = 16
let yum_repos_config_dir  = "/etc/yum.repos.d"
let cached_repomd_xml_path = "/var/cache/yum/x86_64/7/citrix-normal/repomd.xml"
let cmd_yum_config_manager = "/usr/bin/yum-config-manager"
let cmd_reposync = "/usr/bin/reposync"
let cmd_createrepo = "/usr/bin/createrepo_c"
let cmd_modifyrepo = "/usr/bin/modifyrepo_c"
let cmd_yum = "/usr/bin/yum"
let cmd_rm = "/usr/bin/rm"
let cmd_rpm = "/usr/bin/rpm"

let reposync_mutex = Mutex.create ()
let exposing_pool_repo_mutex = Mutex.create ()

(* TODO: determine the format of updateinfo.xml *)

type guidance =
  | RebootHost
  | RestartToolstack
  | EvacuateHost
  | RestartDeviceModel

type guidance_kind = Absolute | Recommended

let string_of_guidance = function
  | RebootHost -> "RebootHost"
  | RestartToolstack -> "RestartToolstack"
  | EvacuateHost -> "EvacuateHost"
  | RestartDeviceModel -> "RestartDeviceModel"

exception Unknown_guidance

let guidance_of_string = function
  | "RebootHost" -> RebootHost
  | "RestartToolstack" -> RestartToolstack
  | "EvacuateHost" -> EvacuateHost
  | "RestartDeviceModel" -> RestartDeviceModel
  | _ -> raise Unknown_guidance

type inequality = Lt | Eq | Gt

let string_of_inequality = function
  | Lt -> "<"
  | Eq -> "="
  | Gt -> ">"

exception Invalid_inequalities

let inequalities_of_string = function
  | "gte" -> [Gt; Eq]
  | "lte" -> [Lt; Eq]
  | "gt"  -> [Gt]
  | "lt"  -> [Lt]
  | "eq"  -> [Eq]
  | _ -> raise Invalid_inequalities

let string_of_inequalities = function
  | [Gt; Eq] | [Eq; Gt] -> ">="
  | [Lt; Eq] | [Eq; Lt] -> "<="
  | [Gt] -> ">"
  | [Lt] -> "<"
  | [Eq] -> "="
  | _ -> raise Invalid_inequalities

type segment_of_version =
  | Int of int
  | Str of string

type applicability = {
    name: string
  ; inequalities: inequality list
  ; epoch: string
  ; version: string
  ; release: string
}

let string_of_applicability a =
  Printf.sprintf "%s%s%s-%s"
    a.name (string_of_inequalities a.inequalities) a.version a.release

type updateinfo = {
    id: string
  ; summary: string
  ; description: string
  ; rec_guidances: guidance list
  ; abs_guidances: guidance list
  ; guidance_applicabilities: applicability list
  ; spec_info: string
  ; url: string
}

let string_of_updateinfo ui =
  Printf.sprintf "id=%s rec_guidances=%s abs_guidances=%s guidance_applicabilities=%s"
    ui.id
    (String.concat ";" (List.map string_of_guidance ui.rec_guidances))
    (String.concat ";" (List.map string_of_guidance ui.abs_guidances))
    (String.concat ";" (List.map string_of_applicability ui.guidance_applicabilities))

type updateinfo_md = {
    checksum: string
  ; open_checksum: string
  ; location: string
  ; timestamp: int
  ; size: int
  ; open_size: int
}

type update = {
    name: string
  ; arch: string
  ; old_ver_rel: string option
  ; new_ver_rel: string
  ; update_id: string
}

let compare_ver v1 v2 =
  let normalize v =
    v
    |> Str.split (Str.regexp "\\.")
    |> List.map (fun s -> try Int (int_of_string s) with _ -> Str s)
  in
  let compare_str v1 v2 f =
    let r = String.compare v1 v2 in
    if r < 0 then Lt
    else if r > 0 then Gt
    else f ()
  in
  let rec compare_segments l1 l2 =
    match l1, l2 with
    | c1 :: t1, c2 :: t2 ->
      let f () = compare_segments t1 t2 in
      (match c1, c2 with
       | Int v1, Int v2 ->
         if v1 > v2 then Gt
         else if v1 = v2 then f ()
         else Lt
       | Int v1, Str v2 -> compare_str (string_of_int v1) v2 f
       | Str v1, Int v2 -> compare_str v1 (string_of_int v2) f
       | Str v1, Str v2 -> compare_str v1 v2 f)
    | c1 :: t1, [] -> Gt
    | [], c2 :: t2 -> Lt
    | [], [] -> Eq
  in
  let r = compare_segments (normalize v1) (normalize v2) in
  debug "version/release comparison between  %s and %s: %s" v1 v2 (string_of_inequality r);
  r

let eval_applicability ~version ~release ~applicability =
  let ver = applicability.version in
  let rel = applicability.release in
  let eval_applicability' inequality =
    match inequality, (compare_ver version ver), (compare_ver release rel) with
    | Lt, Lt, _ | Lt, Eq, Lt | Eq, Eq, Eq | Gt, Gt, _ | Gt, Eq, Gt -> true
    | _ -> false
  in
  List.exists eval_applicability' applicability.inequalities

let eval_guidance_for_one_update ~updates_info ~update ~kind =
  let ver, rel =
    match update.old_ver_rel with
    | Some v_r ->
      begin match Str.split (Str.regexp "-") v_r with
        | v :: r :: [] -> v, r
        | _ ->
          raise Api_errors.(
              Server_error (internal_error,
                            ["Invalid version and release in evaluating guidances"]))
      end
    | None ->
      raise Api_errors.(
          Server_error (internal_error,
                        ["Missing old version and release in evaluating guidances"]))
  in 
  let is_applicable (a : applicability) =
    if update.name = a.name then
      eval_applicability ~version:ver ~release:rel ~applicability:a
    else false
  in
  let updateinfo = List.assoc update.update_id updates_info in
  let applicabilities = updateinfo.guidance_applicabilities in
  match (List.exists is_applicable applicabilities), applicabilities with
  | true, _ | false, [] -> 
    debug "Evaluating applicability for package %s in update %s returns true."
      update.name updateinfo.id;
    begin match kind with
      | Absolute -> List.map string_of_guidance updateinfo.abs_guidances
      | Recommended -> List.map string_of_guidance updateinfo.rec_guidances
    end
    |> GuidanceStrSet.of_list
  | false, _ :: _ ->
    debug "Evaluating applicability for package %s in update %s returns false."
      update.name updateinfo.id;
    GuidanceStrSet.empty

let resort_guidances gs=
  match GuidanceStrSet.find_opt "RebootHost" gs with
  | Some _ -> GuidanceStrSet.singleton "RebootHost"
  | None -> gs

let eval_guidances ~updates_info ~updates ~kind =
  List.fold_left (fun acc u ->
      GuidanceStrSet.union acc (eval_guidance_for_one_update ~updates_info ~update:u ~kind)
  ) GuidanceStrSet.empty updates
  |> resort_guidances
  |> GuidanceStrSet.elements

let create_repository_record ~__context ~name_label ~binary_url ~source_url =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.Repository.create
    ~__context
    ~ref
    ~uuid
    ~name_label
    ~name_description:""
    ~binary_url
    ~source_url
    ~hash:""
    ~up_to_date:false;
  ref

let assert_name_is_valid ~name_label =
    match Str.string_match (Str.regexp "^[A-Za-z0-9\\-]+$") name_label 0 with
    | true -> ()
    | false -> raise Api_errors.(Server_error (invalid_repository_name, [name_label]))

let assert_url_is_valid ~url =
  match List.exists (fun domain_name ->
      let r = Str.regexp
          (Printf.sprintf
             "^https?://[A-Za-z0-9\\-]+\\.%s(:[0-9]+)?/[A-Za-z0-9\\-/\\$]+$"
             domain_name)
      in
      Str.string_match r url 0
    ) !Xapi_globs.repository_domain_name_whitelist with
  | true -> ()
  | false -> raise Api_errors.(Server_error (invalid_base_url, [url]))

let introduce ~__context ~name_label ~binary_url ~source_url =
  assert_name_is_valid ~name_label;
  assert_url_is_valid ~url:binary_url;
  assert_url_is_valid ~url:source_url;
  match Db.Repository.get_by_name_label ~__context ~label:name_label with
  | [] -> create_repository_record  ~__context ~name_label ~binary_url ~source_url
  | _ ->
    error "A repository with same name_label '%s' already exists" name_label;
    raise Api_errors.(Server_error (repository_already_exists, []))

let forget ~__context ~self =
  let pool = Helpers.get_pool ~__context in
  let enabled = Db.Pool.get_repository ~__context ~self:pool in
  if enabled = self then
    raise Api_errors.(Server_error (repository_is_in_use, []))
  else
    Db.Repository.destroy ~__context ~self

let get_enabled_repository ~__context =
  let pool = Helpers.get_pool ~__context in
  match Db.Pool.get_repository ~__context ~self:pool with
  | ref when ref <> Ref.null -> ref
  | _ ->
      raise Api_errors.(Server_error (no_repository_enabled, []))

let remove_local_repo_conf_files () =
  List.iter (fun n ->
    let path = yum_repos_config_dir ^ "/" ^ n in
    let stat = Unix.lstat path in
    let matched = Str.string_match (Str.regexp "^local-.+\\.repo$") n 0 in
    match stat.Unix.st_kind = Unix.S_REG, matched with
    | true, true ->
        debug "removing local YUM repository config file: %s" path;
        Unixext.unlink_safe path
    | _ -> ()
  ) (Array.to_list (Sys.readdir yum_repos_config_dir))

let clean_yum_cache ~prefix ~name =
  try
    let params =
        [
          "--disablerepo=*";
          Printf.sprintf "--enablerepo=%s-%s" prefix name;
          "clean";
          "expire-cache";
        ]
    in
    ignore (Helpers.call_script cmd_yum params);
    ()
  with _ -> warn "Unable to clean YUM cache of %s-%s" prefix name

let with_local_repository ~__context f =
  let ref = get_enabled_repository ~__context in
  let name = Db.Repository.get_name_label ~__context ~self:ref in
  let master_addr =
    try Pool_role.get_master_address ()
    with Pool_role.This_host_is_a_master ->
      Option.get (Helpers.get_management_ip_addr ~__context)
  in
  let url = Printf.sprintf "https://%s/repository/" master_addr in
  let file_path = Printf.sprintf "%s/local-%s.repo" yum_repos_config_dir name in
  let content = String.concat "\n"
      [
        Printf.sprintf "[local-%s]" name;
        Printf.sprintf "name=local-%s" name;
        Printf.sprintf "baseurl=%s" url;
        "enabled=1";
        "gpgcheck=1";
        "gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-Citrix";
      ]
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      remove_local_repo_conf_files ();
      Unixext.atomic_write_to_file file_path owner_ro (fun fd ->
          Unixext.really_write_string fd content |> ignore);
      let config_params =
          [
            "--save";
            Printf.sprintf "--setopt=local-%s.sslverify=false" name;
            Printf.sprintf "local-%s" name;
          ]
      in
      ignore (Helpers.call_script cmd_yum_config_manager config_params);
      f name)
    (fun () ->
       clean_yum_cache ~prefix:"local" ~name;
       remove_local_repo_conf_files ())

let split_rpm_name s =
  (* s I.E. "libpath-utils-0.2.1-29.el7.x86_64" *)
  let r = Str.regexp "^\\(.+\\)\\.\\(noarch\\|x86_64\\)$" in
  match Str.string_match r s 0 with
  | true ->
    (try
       let pkg = Str.replace_first r "\\1" s in (* pkg is, I.E. "libpath-utils-0.2.1-29.el7" *)
       let arch = Str.replace_first r "\\2" s in
       let pos1 = String.rindex pkg '-' in
       let pos2 = String.rindex_from pkg (pos1 - 1) '-' in
       let rel = String.sub pkg (pos1 + 1) ((String.length pkg) - pos1 - 1) in
       let ver = String.sub pkg (pos2 + 1) (pos1 - pos2 - 1) in
       let name = String.sub pkg 0 pos2 in
       debug "%s -> name=%s version=%s release=%s arch=%s" s name ver rel arch;
       Some (name, ver, rel, arch)
     with e ->
       let msg =
         Printf.sprintf "Failed to split rpm name '%s': %s" s (ExnHelper.string_of_exn e) in
       raise Api_errors.(Server_error (internal_error, [msg])))
  | false -> None

let get_installed_pkgs () =
  Helpers.call_script cmd_rpm ["-qa"]
  |> Str.split (Str.regexp "\n")
  |> List.filter_map split_rpm_name
  |> List.map (fun (name, ver, rel, arch) -> ((name ^ "." ^ arch), (ver ^ "-" ^ rel)))

let get_host_updates_in_json ~__context ~installed ~uuid =
  with_local_repository ~__context (fun name ->
      clean_yum_cache ~prefix:"local" ~name;
      let repo_name = Printf.sprintf "local-%s" name in
      let params_of_updateinfo_list =
        [
          "-q"; "--disablerepo=*"; Printf.sprintf "--enablerepo=%s" repo_name;
          "updateinfo"; "list"; "updates";
        ]
      in
      let assert_error output =
        let errors = ["Updateinfo file is not valid XML"] in
        let open Xapi_stdext_std.Xstringext in
        List.iter (fun err ->
          if String.has_substr output err then (
            error "Error from 'yum updateinfo list': %s" err;
            raise Api_errors.(Server_error (invalid_updateinfo_xml, [])))
          else ()) errors;
        output
      in
      let rpm2updates =
        Helpers.call_script cmd_yum params_of_updateinfo_list
        |> assert_error
        |> Str.split (Str.regexp "\n")
        |> List.filter_map (fun l ->
            match Str.split(Str.regexp " +") l with
            | update_id :: _ :: rpm_name :: []  -> Some (rpm_name, update_id)
            | _ -> None)
      in
      let params_of_list =
        [
          "-q"; "--disablerepo=*"; Printf.sprintf "--enablerepo=%s" repo_name;
          "list"; "updates";
        ]
      in
      let installed_pkgs = match installed with
        | true -> get_installed_pkgs ()
        | false -> []
      in
      let updates =
        Helpers.call_script cmd_yum params_of_list
        |> Str.split (Str.regexp "\n")
        |> List.filter_map (fun l ->
            match Str.split(Str.regexp " +") l with
            | "Updated" :: "Packages" :: [] -> None
            | name_arch :: ver_rel :: repo :: [] when repo = repo_name ->
              (* xsconsole.x86_64  10.1.11-34  local-normal *)
              begin match Str.split(Str.regexp "\\.") name_arch with
                | name :: arch :: [] ->
                  let l2 =
                    begin match installed with
                      | true ->
                        (try
                           [("oldVerRel", `String (List.assoc name_arch installed_pkgs))]
                         with _ -> 
                           error "no installed package found for %s" name_arch;
                           raise Api_errors.(Server_error (get_host_updates_failed, [uuid])))
                      | false -> []
                    end
                  in
                  let rpm_name = Printf.sprintf "%s-%s.%s" name ver_rel arch in
                  (try
                     let l = [("name", `String name);
                              ("arch", `String arch);
                              ("newVerRel", `String ver_rel);
                              ("updateId", `String (List.assoc rpm_name rpm2updates))]
                     in
                     Some (`Assoc (l @ l2))
                   with _ -> 
                     error "no update found for %s" rpm_name;
                     raise Api_errors.(Server_error (invalid_updateinfo_xml, [])))
                | _ -> None
              end
            | _ -> None)
      in
      (* TODO: live patches *)
      `Assoc [("updates", `List updates)])

let cleanup ~__context ~self ~prefix =
  if self <> Ref.null then
    let name = Db.Repository.get_name_label ~__context ~self in
    try
      clean_yum_cache ~prefix ~name;
      ignore (Helpers.call_script
                cmd_rm
                ["-f"; (Printf.sprintf "/etc/yum.repos.d/%s-%s.repo" prefix name)]);
      ignore (Helpers.call_script cmd_rm ["-rf"; !Xapi_globs.local_pool_repo_dir]);
      ()
    with e ->
      error "Failed to cleanup local pool repository: %s" (ExnHelper.string_of_exn e);
      raise Api_errors.(Server_error (local_pool_repo_cleanup_failed, []))

let parse_repomd xml_path =
  let empty_updateinfo_md = {
      checksum = ""
    ; open_checksum = ""
    ; location = ""
    ; timestamp = 0
    ; size = 0
    ; open_size = 0
    }
  in
  match Sys.file_exists xml_path with
  | false ->
    error "No repomd.xml found";
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
              try
                match n with
                | Xml.Element ("checksum", _, [Xml.PCData v]) ->
                  {md with checksum = v}
                | Xml.Element ("open-checksum", _, [Xml.PCData v]) ->
                  {md with open_checksum = v}
                | Xml.Element ("location", attrs, _) ->
                  {md with location = (List.assoc "href" attrs)}
                | Xml.Element ("timestamp", _, [Xml.PCData v]) ->
                  {md with timestamp = int_of_string v}
                | Xml.Element ("size", _, [Xml.PCData v]) ->
                  {md with size = int_of_string v}
                | Xml.Element ("open-size", _, [Xml.PCData v]) ->
                  {md with open_size = int_of_string v}
                | _ ->
                  debug "Unknown node in <updateinfo>, ignore it";
                  md
              with _ ->
                error "Unexpected 'updateinfo' node";
                raise Api_errors.(Server_error (invalid_repomd_xml, []))
          ) empty_updateinfo_md l
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

let parse_guidances ~gs =
  List.map (fun g ->
      match g with
      | Xml.Element ("guidance", _, [Xml.PCData v]) ->
        guidance_of_string v
      | _ -> 
        error "Unknown node in <absolute|recommended_guidances>";
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
  ) gs

let parse_applicabilities ~apps =
  let empty_applicability = {
      name = ""
    ; inequalities = []
    ; epoch = ""
    ; version = ""
    ; release = ""
    }
  in
  List.map (fun a ->
      match a with
      | Xml.Element ("applicability", _, children) ->
        List.fold_left (fun a n ->
            match n with
            | Xml.Element ("inequality", _, [Xml.PCData v]) ->
              { a with inequalities = (inequalities_of_string v) }
            | Xml.Element ("epoch", _, [Xml.PCData v]) ->
              { a with epoch = v }
            | Xml.Element ("version", _, [Xml.PCData v]) ->
              { a with version = v }
            | Xml.Element ("release", _, [Xml.PCData v]) ->
              { a with release = v }
            | Xml.Element ("name", _, [Xml.PCData v]) ->
              { a with name = v }
            | _ ->
              error "Unknown node in <applicability>";
              raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
        ) empty_applicability children
      | _ ->
        error "Unknown node in <guidance_applicabilities>";
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
  ) apps

let parse_updateinfo_xml_file xml_file_path =
  let empty_updateinfo = {
      id = ""
    ; summary = ""
    ; description = ""
    ; rec_guidances = []
    ; abs_guidances = []
    ; guidance_applicabilities = []
    ; spec_info = ""
    ; url = ""
    }
  in
  match Xml.parse_file xml_file_path with
  | Xml.Element  ("updates", _, children) ->
    List.filter_map (fun n ->
        match n with
        | Xml.Element ("update", _, update_nodes) ->
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
                { acc with rec_guidances = (parse_guidances gs) }
              | Xml.Element ("absolute_guidances", _, gs) ->
                { acc with abs_guidances = (parse_guidances gs) }
              | Xml.Element ("guidance_applicabilities", _, apps) ->
                { acc with guidance_applicabilities = (parse_applicabilities apps) }
              | _ -> acc
            ) empty_updateinfo update_nodes
          in
          debug "updateinfo: %s" (string_of_updateinfo ui);
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

let with_updateinfo_xml gz_path f =
  let tmpfile, tmpch = Filename.open_temp_file ~mode:[Open_text] "updateinfo" ".xml" in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () ->
           try
             Unixext.with_file gz_path [Unix.O_RDONLY] 0o0
               (fun gz_fd_in ->
                  Gzip.decompress_passive gz_fd_in (fun fd_in ->
                      let ic = Unix.in_channel_of_descr fd_in in
                      try
                        while true do
                          let line = input_line ic in
                          output_string tmpch (line ^ "\n")
                        done
                      with End_of_file -> ()))
           with e ->
             error "Failed to decompress updateinfo.xml.gz: %s" (ExnHelper.string_of_exn e);
             raise Api_errors.(Server_error (invalid_updateinfo_xml, [])))
        (fun () -> close_out tmpch);
      f tmpfile)
    (fun () -> Sys.remove tmpfile)

let parse_updateinfo ~prefix ~name ~hash =
  let repo_dir = Printf.sprintf "%s/%s-%s" !Xapi_globs.local_pool_repo_dir prefix name in
  let repodata_dir = repo_dir ^ "/repodata" in
  let repomd_xml_path = repodata_dir ^ "/repomd.xml" in
  let md = parse_repomd repomd_xml_path in
  let updateinfo_xml_gz_path = repo_dir ^ "/" ^ md.location in
  if hash <> md.checksum then error "unexpected mismatch between XAPI DB and YUM DB";
  match Sys.file_exists updateinfo_xml_gz_path with
  | false -> raise Api_errors.(Server_error (no_updateinfo_xml, []))
  | true ->
    with_updateinfo_xml updateinfo_xml_gz_path parse_updateinfo_xml_file

let http_get_host_updates_in_json ~__context ~host ~installed =
  let host_session_id =
    Xapi_session.login_no_password ~__context ~uname:None ~host ~pool:true
      ~is_local_superuser:true ~subject:Ref.null ~auth_user_sid:""
      ~auth_user_name:"" ~rbac_permissions:[]
  in
  let request = Xapi_http.http_request
      ~cookie:[("session_id", Ref.string_of host_session_id)]
      ~query:[("installed", (string_of_bool installed))]
      Http.Get Constants.get_host_updates_uri
  in
  let host_name = (Db.Host.get_hostname ~__context ~self:host) in
  let host_addr = Db.Host.get_address ~__context ~self:host in
  let open Xmlrpc_client in
  let transport = SSL (SSL.make () ~verify_cert:false, host_addr, !Constants.https_port) in
  debug "getting host updates on %s (addr %s) by HTTP GET" host_name host_addr;
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      try
        let json_str =
          with_transport transport
            (with_http request (fun (response, fd) ->
                 Xapi_stdext_unix.Unixext.string_of_fd fd))
        in
        debug "host %s returned updates: %s" host_name json_str;
        Yojson.Basic.from_string json_str
      with e ->
        let uuid = Db.Host.get_uuid ~__context ~self:host in
        error "Failed to get updates from host uuid='%s': %s" uuid (ExnHelper.string_of_exn e);
        raise Api_errors.(Server_error (get_host_updates_failed, [uuid])))
    (fun () -> Xapi_session.destroy_db_session ~__context ~self:host_session_id)

let run_in_parallel ~funs ~capacity =
  let rec run_in_parallel' acc funs capacity =
    let rec split_for_first_n acc n l =
      match n, l with
      | n, h::t when n > 0 -> split_for_first_n (h :: acc) (n - 1) t
      | _ -> acc, l
    in
    let run f =
      let result = ref `Not_started in
      let wrapper r = try r := `Succ (f ()) with e -> r := `Fail e in
      let th = Thread.create wrapper result in
      th, result
    in
    let get_result (th, result) =
      Thread.join th;
      match !result with
      | `Not_started -> `Error (Failure "The thread in run_in_parallel is not started")
      | `Succ s -> `Ok s
      | `Fail e -> `Error e
    in
    let to_be_run, remaining = split_for_first_n [] capacity funs in
    match to_be_run with
    | [] -> acc
    | _ -> 
      let finished =
        List.map run to_be_run
        |> List.map get_result
        |> List.map (function `Ok s -> s | `Error e -> raise e)
      in
      run_in_parallel' (List.rev_append finished acc) remaining capacity
  in
  run_in_parallel' [] funs capacity

let write_yum_config ~__context ~self ~prefix =
  let name = Db.Repository.get_name_label ~__context ~self in
  let file_path = Printf.sprintf "/etc/yum.repos.d/%s-%s.repo" prefix name in
  let binary_url = Db.Repository.get_binary_url  ~__context ~self in
  let source_url = Db.Repository.get_source_url  ~__context ~self in
  let content = String.concat "\n"
      [
        Printf.sprintf "[%s-%s]" prefix name;
        Printf.sprintf "name=%s-%s" prefix name;
        Printf.sprintf "baseurl=%s" binary_url;
        "enabled=0";
        if !Xapi_globs.repository_gpgcheck then "gpgcheck=1" else "gpgcheck=0";
        "gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-Citrix";
        "";
        Printf.sprintf "[%s-%s-source]" prefix name;
        Printf.sprintf "name=%s-%s-source" prefix name;
        Printf.sprintf "baseurl=%s" source_url;
        "enabled=0";
        if !Xapi_globs.repository_gpgcheck then "gpgcheck=1" else "gpgcheck=0";
        "gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-Citrix";
       ]
  in
  Unixext.atomic_write_to_file file_path owner_ro (fun fd ->
    Unixext.really_write_string fd content |> ignore);
  name

let sync ~__context ~self ~prefix =
  try
    remove_local_repo_conf_files ();
    let name = write_yum_config ~__context ~self ~prefix in
    let config_params =
        [
          "--save";
          "repo_gpgcheck=1";
          Printf.sprintf "%s-%s" prefix name;
        ]
    in
    ignore (Helpers.call_script cmd_yum_config_manager config_params);
    (* sync with remote repository *)
    let sync_params =
        [
          "-p"; !Xapi_globs.local_pool_repo_dir;
          "--downloadcomps";
          "--download-metadata";
          if !Xapi_globs.repository_gpgcheck then "--gpgcheck" else "";
          "--delete";
          Printf.sprintf "--repoid=%s-%s" prefix name;
        ]
    in
    Unixext.mkdir_rec !Xapi_globs.local_pool_repo_dir 0o700;
    clean_yum_cache ~prefix ~name;
    ignore (Helpers.call_script cmd_reposync sync_params)
  with e ->
    error "Failed to sync with remote YUM repository: %s" (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (reposync_failed, []))

let create_pool_repository ~__context ~self ~prefix =
  let name = Db.Repository.get_name_label ~__context ~self in
  match Sys.file_exists !Xapi_globs.local_pool_repo_dir with
  | true ->
    let repo_dir = Printf.sprintf "%s/%s-%s"
        !Xapi_globs.local_pool_repo_dir prefix name
    in
    ignore (Helpers.call_script cmd_createrepo [repo_dir]);
    let md = parse_repomd cached_repomd_xml_path in
    let updateinfo_xml_gz_path =
      Printf.sprintf "%s/%s-updateinfo.xml.gz" repo_dir md.checksum
    in
    begin match Sys.file_exists updateinfo_xml_gz_path with
      | true ->
        with_updateinfo_xml updateinfo_xml_gz_path (fun xml_file_path ->
            ignore (Helpers.call_script cmd_modifyrepo
                      ["--remove"; "updateinfo"; repo_dir ^ "/repodata"]);
            ignore (Helpers.call_script cmd_modifyrepo
                      ["--mdtype"; "updateinfo"; xml_file_path; repo_dir ^ "/repodata"]))
      | false -> raise Api_errors.(Server_error (no_updateinfo_xml, []))
    end
  | false ->
    error "local pool repository directory %s does not exists" !Xapi_globs.local_pool_repo_dir;
    raise Api_errors.(Server_error (no_local_pool_repository, []))

let with_pool_repository ~__context ~self ~prefix f =
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
        Mutex.lock exposing_pool_repo_mutex;
        f ())
    (fun () -> Mutex.unlock exposing_pool_repo_mutex)

let is_local_pool_repo_enabled () =
  match Mutex.try_lock exposing_pool_repo_mutex with
  | true ->
    Mutex.unlock exposing_pool_repo_mutex;
    false
  | false -> true

let reposync_try_lock () =
  Mutex.try_lock reposync_mutex

let reposync_unlock () =
  Mutex.unlock reposync_mutex

let with_reposync_lock f =
  if Mutex.try_lock reposync_mutex then
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () -> f ())
      (fun () -> Mutex.unlock reposync_mutex)
  else
    raise Api_errors.(Server_error (reposync_in_progress, []))

let get_local_repomd_xml_path ~__context ~self ~prefix =
  let name = Db.Repository.get_name_label ~__context ~self in
  Printf.sprintf "%s/%s-%s/repodata/repomd.xml" !Xapi_globs.local_pool_repo_dir prefix name

let try_set_available_updates ~__context ~self ~prefix =
  let hosts = Db.Host.get_all ~__context in
  let xml_path = get_local_repomd_xml_path ~__context ~self ~prefix in
  let md = parse_repomd xml_path in
  let hash = md.checksum in
  let not_up_to_date host () =
    let json = http_get_host_updates_in_json ~__context ~host ~installed:false in
    (* TODO: live patches *)
    match Yojson.Basic.Util.member "updates" json with
    | `List [] -> false
    | _ -> true
    | exception e ->
      let uuid = Db.Host.get_uuid ~__context ~self:host in
      error "Invalid updates from host uuid='%s': %s" uuid (ExnHelper.string_of_exn e);
      raise Api_errors.(Server_error (get_host_updates_failed, [uuid]))
  in
  let funs = List.map (fun h -> not_up_to_date h) hosts in
  let rets_of_hosts = run_in_parallel funs capacity_in_parallel in
  let is_all_up_to_date = not (List.exists (fun b -> b) rets_of_hosts) in
  Db.Repository.set_up_to_date ~__context ~self  ~value:is_all_up_to_date;
  Db.Repository.set_hash ~__context ~self ~value:hash;
  hash

let update_of_json j =
  let open Yojson.Basic.Util in
  let u = {
    name = (member "name" j |> to_string);
    arch = (member "arch" j |> to_string);
    new_ver_rel = (member "newVerRel" j |> to_string);
    old_ver_rel = (try Some (member "oldVerRel" j |> to_string) with _ -> None);
    update_id = (member "updateId" j |> to_string)
    }
  in
  let old_ver_rel = match u.old_ver_rel with
    | Some vr -> vr
    | None -> "<Unknown>"
  in
  debug "in update %s: %s.%s will be updated as %s -> %s"
    u.update_id u.name u.arch old_ver_rel u.new_ver_rel;
  u

let get_pool_updates_in_json ~__context ~prefix ~hosts =
  let ref = get_enabled_repository ~__context in
  let hash = Db.Repository.get_hash ~__context ~self:ref in
  let name = Db.Repository.get_name_label ~__context ~self:ref in
  let updates_info = parse_updateinfo ~prefix ~name ~hash in
  let updates_of_hosts, ids_of_updates = List.fold_left (fun (acc1, acc2) host ->
      let updates =
        http_get_host_updates_in_json ~__context ~host ~installed:true
        |> Yojson.Basic.Util.member "updates"
        |> Yojson.Basic.Util.to_list
        |> List.map update_of_json
      in
      let rpms, uids = List.fold_left (fun (acc_rpms, acc_uids) u ->
          ( ((Printf.sprintf "%s-%s.%s.rpm" u.name u.new_ver_rel u.arch) :: acc_rpms),
            (UpdateIdSet.add u.update_id acc_uids) )
        ) ([], UpdateIdSet.empty) updates
      in
      let rec_guidances = List.map (fun g -> `String g)
          (eval_guidances ~updates_info ~updates ~kind:Recommended)
      in
      let abs_guidances = List.map (fun g -> `String g)
          (eval_guidances ~updates_info ~updates ~kind:Absolute)
      in
      let json_of_host = `Assoc [
          ("ref", `String (Ref.string_of host));
          ("recommended-guidance", `List rec_guidances);
          ("absolute-guidance", `List abs_guidances);
          ("RPMS", `List (List.map (fun r -> `String r) rpms));
          ("updates", `List (List.map (fun uid -> `String uid) (UpdateIdSet.elements uids)))]
      in
      ( (json_of_host :: acc1), (UpdateIdSet.union uids acc2) )
    ) ([], UpdateIdSet.empty) hosts
  in
  `Assoc [
    ("hosts", `List updates_of_hosts);
    ("updates", `List (UpdateIdSet.elements ids_of_updates |> List.map (fun x -> `String x)));
    ("hash", `String hash)]

let get_repository_handler (req : Http.Request.t) s _ =
  let open Http in
  let open Xapi_stdext_std.Xstringext in
  debug "Repository.get_repository_handler URL %s" req.Request.uri ;
  req.Request.close <- true ;
  match is_local_pool_repo_enabled () with
  | true ->
    (try
      let repo_name =
        let l = List.filter_map (fun n ->
            let path = !Xapi_globs.local_pool_repo_dir ^ "/" ^ n in
            let stat = Unix.lstat path in
            begin match stat.Unix.st_kind = Unix.S_DIR with
              | true -> Some n
              | false -> None
            end
          ) (Array.to_list (Sys.readdir !Xapi_globs.local_pool_repo_dir))
        in
        begin match l with
          | [d] -> d
          | _ ->
            let msg = "Multiple or no repositories are under local pool repository directory" in
            raise Api_errors.(Server_error (internal_error, [msg]))
        end
      in
      let resolved_path =
        let len = String.length Constants.get_repository_uri in
        begin match String.sub_to_end req.Request.uri len with
          | untrusted_path ->
            untrusted_path
            |> Filename.concat repo_name
            |> Filename.concat !Xapi_globs.local_pool_repo_dir
            |> Uri.pct_decode
            |> Xapi_stdext_unix.Unixext.resolve_dot_and_dotdot
          | exception e ->
            let msg =
              Printf.sprintf "Failed to get path from uri': %s" (ExnHelper.string_of_exn e)
            in
            raise Api_errors.(Server_error (internal_error, [msg]))
        end
      in
      let p = !Xapi_globs.local_pool_repo_dir ^ "/" ^ repo_name ^ "/" in
      begin match (String.startswith p resolved_path), (Sys.file_exists resolved_path) with
        | true, true ->
          Fileserver.response_file s resolved_path
        | _ ->
          error
            "Rejecting request for file: %s (outside of or not existed in directory %s)"
            resolved_path !Xapi_globs.local_pool_repo_dir;
          Http_svr.response_forbidden ~req s
      end
    with e ->
      error
        "Failed to serve for request on uri %s: %s" req.Request.uri (ExnHelper.string_of_exn e);
      Http_svr.response_forbidden ~req s)
  | false ->
    error "Rejecting request: local pool repository is not enabled";
    Http_svr.response_forbidden ~req s
