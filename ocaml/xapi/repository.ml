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

let capacity_in_parallel = 16

let reposync_mutex = Mutex.create ()
let exposing_pool_repo_mutex = Mutex.create ()

type updateinfo_md = {
    checksum: string
  ; location: string
}

type pkg = {
    name: string
  ; version: string
  ; release: string
  ; arch: string
}

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
  ; arch: string
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
  ; update_type: string
}

let json_of_updateinfo ui =
  `Assoc [
    ("id", `String ui.id);
    ("summary", `String ui.summary);
    ("description", `String ui.description);
    ("special-info", `String ui.spec_info);
    ("URL", `String ui.url);
    ("type", `String ui.update_type);
    ("recommended-guidance", `List (List.map (fun g ->
         `String (string_of_guidance g)) ui.rec_guidances));
    ("absolute-guidance", `List (List.map (fun g ->
         `String (string_of_guidance g)) ui.abs_guidances))
  ]

type update = {
    name: string
  ; arch: string
  ; old_version: string option
  ; old_release: string option
  ; new_version: string
  ; new_release: string
  ; update_id: string option
}

let update_of_json j =
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

let introduce ~__context ~name_label ~name_description ~binary_url ~source_url =
  assert_url_is_valid ~url:binary_url;
  assert_url_is_valid ~url:source_url;
  Db.Repository.get_all ~__context
  |> List.iter (fun ref ->
      if name_label = Db.Repository.get_name_label ~__context ~self:ref
      || binary_url = Db.Repository.get_binary_url ~__context ~self:ref then
        raise Api_errors.( Server_error (repository_already_exists, [(Ref.string_of ref)]) ));
  create_repository_record  ~__context ~name_label ~name_description ~binary_url ~source_url

let forget ~__context ~self =
  let pool = Helpers.get_pool ~__context in
  let enabled = Db.Pool.get_repository ~__context ~self:pool in
  if enabled = self then
    raise Api_errors.(Server_error (repository_is_in_use, []))
  else
    Db.Repository.destroy ~__context ~self

let with_reposync_lock f =
  if Mutex.try_lock reposync_mutex then
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () -> f ())
      (fun () -> Mutex.unlock reposync_mutex)
  else
    raise Api_errors.(Server_error (reposync_in_progress, []))

let get_enabled_repository ~__context =
  let pool = Helpers.get_pool ~__context in
  match Db.Pool.get_repository ~__context ~self:pool with
  | ref when ref <> Ref.null -> ref
  | _ ->
    raise Api_errors.(Server_error (no_repository_enabled, []))

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

let cleanup_pool_repo () =
  try
    clean_yum_cache !Xapi_globs.pool_repo_name;
    Unixext.unlink_safe (Filename.concat !Xapi_globs.yum_repos_config_dir
                           !Xapi_globs.pool_repo_name);
    Helpers.rmtree !Xapi_globs.local_pool_repo_dir
  with e ->
    error "Failed to cleanup pool repository: %s" (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (repository_cleanup_failed, []))

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

let sync ~__context ~self =
  try
    remove_repo_conf_file !Xapi_globs.pool_repo_name;
    let binary_url = Db.Repository.get_binary_url  ~__context ~self in
    let source_url = Db.Repository.get_source_url  ~__context ~self in
    write_yum_config ~source_url:(Some source_url) binary_url !Xapi_globs.pool_repo_name;
    let config_params =
        [
          "--save";
          if !Xapi_globs.repository_gpgcheck then "--setopt=repo_gpgcheck=1"
          else "--setopt=repo_gpgcheck=0";
          Printf.sprintf "%s" !Xapi_globs.pool_repo_name;
        ]
    in
    ignore (Helpers.call_script !Xapi_globs.yum_config_manager_cmd config_params);
    (* sync with remote repository *)
    let sync_params =
        [
          "-p"; !Xapi_globs.local_pool_repo_dir;
          "--downloadcomps";
          "--download-metadata";
          if !Xapi_globs.repository_gpgcheck then "--gpgcheck" else "";
          "--delete";
          Printf.sprintf "--repoid=%s" !Xapi_globs.pool_repo_name;
        ]
    in
    Unixext.mkdir_rec !Xapi_globs.local_pool_repo_dir 0o700;
    clean_yum_cache !Xapi_globs.pool_repo_name;
    ignore (Helpers.call_script !Xapi_globs.reposync_cmd sync_params)
  with e ->
    error "Failed to sync with remote YUM repository: %s" (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (reposync_failed, []))

let with_pool_repository f =
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
        Mutex.lock exposing_pool_repo_mutex;
        f ())
    (fun () -> Mutex.unlock exposing_pool_repo_mutex)

let parse_repomd xml_path =
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
        let ref = Ref.string_of host in
        error "Failed to get updates from host ref='%s': %s" ref (ExnHelper.string_of_exn e);
        raise Api_errors.(Server_error (get_host_updates_failed, [ref])))
    (fun () -> Xapi_session.destroy_db_session ~__context ~self:host_session_id)

let set_available_updates ~__context ~self =
  let hosts = Db.Host.get_all ~__context in
  let xml_path =
    "repodata/repomd.xml"
    |> Filename.concat !Xapi_globs.pool_repo_name
    |> Filename.concat !Xapi_globs.local_pool_repo_dir
  in
  let md = parse_repomd xml_path in
  let are_updates_available host () =
    let json = http_get_host_updates_in_json ~__context ~host ~installed:false in
    (* TODO: live patches *)
    match Yojson.Basic.Util.member "updates" json with
    | `List [] -> false
    | _ -> true
    | exception e ->
      let ref = Ref.string_of host in
      error "Invalid updates from host ref='%s': %s" ref (ExnHelper.string_of_exn e);
      raise Api_errors.(Server_error (get_host_updates_failed, [ref]))
  in
  let funs = List.map (fun h -> are_updates_available h) hosts in
  let rets_of_hosts = Helpers.run_in_parallel funs capacity_in_parallel in
  let is_all_up_to_date = not (List.exists (fun b -> b) rets_of_hosts) in
  Db.Repository.set_up_to_date ~__context ~self ~value:is_all_up_to_date;
  Db.Repository.set_hash ~__context ~self ~value:(md.checksum);
  md.checksum

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
    (fun () -> Unixext.unlink_safe tmpfile)

let create_pool_repository ~__context ~self =
  let repo_dir = Filename.concat !Xapi_globs.local_pool_repo_dir !Xapi_globs.pool_repo_name in
  match Sys.file_exists repo_dir with
  | true ->
    (try
       let cachedir = get_cachedir !Xapi_globs.pool_repo_name in
       let md = parse_repomd (Filename.concat cachedir "repomd.xml") in
       let updateinfo_xml_gz_path =
         Filename.concat repo_dir (md.checksum ^ "-updateinfo.xml.gz")
       in
       ignore (Helpers.call_script !Xapi_globs.createrepo_cmd [repo_dir]);
       begin match Sys.file_exists updateinfo_xml_gz_path with
         | true ->
           with_updateinfo_xml updateinfo_xml_gz_path (fun xml_file_path ->
               let repodata_dir = Filename.concat repo_dir "repodata" in
               ignore (Helpers.call_script !Xapi_globs.modifyrepo_cmd
                         ["--remove"; "updateinfo"; repodata_dir]);
               ignore (Helpers.call_script !Xapi_globs.modifyrepo_cmd
                         ["--mdtype"; "updateinfo"; xml_file_path; repodata_dir]));
           with_pool_repository (fun () ->
               set_available_updates ~__context ~self)
         | false ->
           error "No updateinfo.xml.gz found: %s" updateinfo_xml_gz_path;
           raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
       end
     with
     | Api_errors.(Server_error (code, _)) as e when code <> Api_errors.internal_error ->
       raise e
     | e ->
       error "Creating local pool repository failed: %s" (ExnHelper.string_of_exn e);
       raise Api_errors.(Server_error (createrepo_failed, [])))
  | false ->
    error "local pool repository directory '%s' does not exist" repo_dir;
    raise Api_errors.(Server_error (reposync_failed, []))

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

let pkg_of_fullname s =
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

let json_of_pkg (pkg: pkg) =
  `Assoc [
    ("version", `String pkg.version);
    ("release", `String pkg.release);
  ]

let name_arch_of_pkg (pkg: pkg) =
  pkg.name ^ "." ^ pkg.arch

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
        begin match pkg_of_fullname full_name with
          | Some pkg -> Some ((name_arch_of_pkg pkg), update_id)
          | None -> None
        end
      | _ ->
        debug "Ignore unrecognized line '%s' in parsing updateinfo list" line;
        None)

let get_installed_pkgs () =
  Helpers.call_script !Xapi_globs.rpm_cmd ["-qa"]
  |> Astring.String.cuts ~sep:"\n"
  |> List.filter_map pkg_of_fullname
  |> List.map (fun pkg -> ((name_arch_of_pkg pkg), pkg))

let get_host_updates_in_json ~__context ~installed =
  try
    with_local_repository ~__context (fun () ->
      let rpm2updates = get_updates_from_updateinfo () in
      let installed_pkgs = match installed with
        | true -> get_installed_pkgs ()
        | false -> []
      in
      let params_of_list =
        [
          "-q"; "--disablerepo=*"; Printf.sprintf "--enablerepo=%s" !Xapi_globs.local_repo_name;
          "list"; "updates";
        ]
      in
      let sep = Re.Str.regexp " +" in
      let updates =
        clean_yum_cache !Xapi_globs.local_repo_name;
        Helpers.call_script !Xapi_globs.yum_cmd params_of_list
        |> Astring.String.cuts ~sep:"\n"
        |> List.filter_map (fun line ->
            match Re.Str.split sep line with
            | "Updated" :: "Packages" :: [] -> None
            | name_arch :: ver_rel :: repo :: [] when repo = !Xapi_globs.local_repo_name ->
              (* xsconsole.x86_64  10.1.11-34  local-normal *)
              begin match Astring.String.cuts ~sep:"." name_arch,
                          Astring.String.cuts ~sep:"-" ver_rel with
                | name :: arch :: [], version :: release :: [] ->
                  let new_pkg = { name; version; release; arch } in
                  let uid_in_json = match List.assoc_opt name_arch rpm2updates with
                    | Some s -> `String s
                    | None ->
                      warn "No update ID found for %s" name_arch;
                      `Null
                  in
                  let l1 = [("name", `String name);
                            ("arch", `String arch);
                            ("newVerRel", (json_of_pkg new_pkg));
                            ("updateId", uid_in_json)]
                  in
                  let l2 = match List.assoc_opt name_arch installed_pkgs with
                    | Some old_pkg ->
                      [("oldVerRel", (json_of_pkg old_pkg))]
                    | None -> []
                  in
                  Some (`Assoc (l1 @ l2))
                | _ ->
                  warn "Can't parse %s and %s" name_arch ver_rel;
                  None
              end
            | _ ->
              debug "Ignore unrecognized line '%s' in parsing updates list" line;
              None)
      in
      (* TODO: live patches *)
      `Assoc [("updates", `List updates)])
  with
  | Api_errors.(Server_error (code, _)) as e when code <> Api_errors.internal_error ->
    raise e
  | e ->
    let ref = Ref.string_of (Helpers.get_localhost ~__context) in
    error "Failed to get host updates on host ref=%s: %s" ref (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (get_host_updates_failed, [ref]))

let is_local_pool_repo_enabled () =
  if Mutex.try_lock exposing_pool_repo_mutex then
    (Mutex.unlock exposing_pool_repo_mutex;
     false)
  else true

(* This handler hosts HTTP endpoint '/repository' which will be available iif
 * 'is_local_pool_repo_enabled' returns true with 'with_pool_repository' being called by
 * others.
 *)
let get_repository_handler (req : Http.Request.t) s _ =
  let open Http in
  let open Xapi_stdext_std.Xstringext in
  debug "Repository.get_repository_handler URL %s" req.Request.uri ;
  req.Request.close <- true ;
  if is_local_pool_repo_enabled () then
    (try
        let len = String.length Constants.get_repository_uri in
        begin match String.sub_to_end req.Request.uri len with
          | uri_path ->
            let root = Filename.concat
                !Xapi_globs.local_pool_repo_dir !Xapi_globs.pool_repo_name
            in
            Fileserver.response_file s (Helpers.resolve_uri_path ~root ~uri_path)
          | exception e ->
            let msg =
              Printf.sprintf "Failed to get path from uri': %s" (ExnHelper.string_of_exn e)
            in
            raise Api_errors.(Server_error (internal_error, [msg]))
        end
    with e ->
      error
        "Failed to serve for request on uri %s: %s" req.Request.uri (ExnHelper.string_of_exn e);
      Http_svr.response_forbidden ~req s)
  else
    (error "Rejecting request: local pool repository is not enabled";
     Http_svr.response_forbidden ~req s)

let string_of_updateinfo ui =
  Printf.sprintf "id=%s rec_guidances=%s abs_guidances=%s guidance_applicabilities=%s"
    ui.id
    (String.concat ";" (List.map string_of_guidance ui.rec_guidances))
    (String.concat ";" (List.map string_of_guidance ui.abs_guidances))
    (String.concat ";" (List.map string_of_applicability ui.guidance_applicabilities))

let parse_guidances gs =
  List.map (function
      | Xml.Element ("guidance", _, [Xml.PCData v]) ->
        guidance_of_string v
      | _ ->
        error "Unknown node in <absolute|recommended_guidances>";
        raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
  ) gs

let parse_applicabilities apps =
  let empty_applicability = {
      name = ""
    ; arch = ""
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
            | Xml.Element ("arch", _, [Xml.PCData v]) ->
              { a with arch = v }
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
    ; update_type = ""
    }
  in
  let assert_valid_updateinfo = function
    | { id = ""; _}
    | { summary = ""; _}
    | { description = ""; _}
    | { update_type = ""; _} ->
      error "One or more of 'id', 'summary', 'description' and 'update_type' is/are missing";
      raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
    | ui -> ui
  in
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
                { acc with rec_guidances = (parse_guidances gs) }
              | Xml.Element ("absolute_guidances", _, gs) ->
                { acc with abs_guidances = (parse_guidances gs) }
              | Xml.Element ("guidance_applicabilities", _, apps) ->
                { acc with guidance_applicabilities = (parse_applicabilities apps) }
              | _ -> acc
            ) { empty_updateinfo with update_type = ty } update_nodes
            |> assert_valid_updateinfo
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

let parse_updateinfo ~hash =
  let repo_dir = Filename.concat !Xapi_globs.local_pool_repo_dir !Xapi_globs.pool_repo_name in
  let repodata_dir = Filename.concat repo_dir "repodata" in
  let repomd_xml_path = Filename.concat repodata_dir "repomd.xml" in
  let md = parse_repomd repomd_xml_path in
  if hash <> md.checksum then
    (error "Unexpected mismatch between XAPI DB and YUM DB. Need to do pool.sync-updates again.";
     raise Api_errors.(Server_error (createrepo_failed, [])));
  let updateinfo_xml_gz_path = Filename.concat repo_dir md.location in
  match Sys.file_exists updateinfo_xml_gz_path with
  | false ->
    error "File %s doesn't exist" updateinfo_xml_gz_path;
    raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
  | true ->
    with_updateinfo_xml updateinfo_xml_gz_path parse_updateinfo_xml_file

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
    |> List.map (fun s -> try Int (int_of_string s) with _ -> Str s)
  in
  let rec compare_segments l1 l2 =
    match l1, l2 with
    | c1 :: t1, c2 :: t2 ->
      begin match c1, c2 with
        | Int s1, Int s2 ->
          if s1 > s2 then Gt
          else if s1 = s2 then compare_segments t1 t2
          else Lt
        | Int s1, Str s2 -> Gt
        | Str s1, Int s2 -> Lt
        | Str s1, Str s2 ->
          let r = String.compare s1 s2 in
          if r < 0 then Lt
          else if r > 0 then Gt
          else compare_segments t1 t2
      end
    | _ :: _, [] -> Gt
    | [], _ :: _ -> Lt
    | [], [] -> Eq
  in
  compare_segments (normalize s1) (normalize s2)

let eval_applicability ~version ~release ~applicability =
  let ver = applicability.version in
  let rel = applicability.release in
  let eval_applicability' inequality =
    match inequality,
          (compare_version_strings version ver),
          (compare_version_strings release rel) with
    | Lt, Lt, _ | Lt, Eq, Lt | Eq, Eq, Eq | Gt, Gt, _ | Gt, Eq, Gt -> true
    | _ -> false
  in
  List.exists eval_applicability' applicability.inequalities

let eval_guidance_for_one_update ~updates_info ~update ~kind =
  match update.update_id with
  | Some uid ->
    begin match List.assoc_opt uid updates_info with
      | Some updateinfo ->
        let is_applicable (a : applicability) =
          match update.name = a.name with
          | true ->
            begin match update.old_version, update.old_release with
              | Some old_version, Some old_release ->
                eval_applicability ~version:old_version ~release:old_release ~applicability:a
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
        let apps = updateinfo.guidance_applicabilities in
        begin match (List.exists is_applicable apps), apps with
          | true, _ | false, [] ->
            debug "%s" (dbg_msg true);
            begin match kind with
              | Absolute -> List.map string_of_guidance updateinfo.abs_guidances
              | Recommended -> List.map string_of_guidance updateinfo.rec_guidances
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

let resort_guidances gs=
  match GuidanceStrSet.find_opt "RebootHost" gs with
  | Some _ -> GuidanceStrSet.singleton "RebootHost"
  | None -> gs

let eval_guidances ~updates_info ~updates ~kind =
  List.fold_left (fun acc u ->
      GuidanceStrSet.union acc
        (eval_guidance_for_one_update ~updates_info ~update:u ~kind)
  ) GuidanceStrSet.empty updates
  |> resort_guidances
  |> GuidanceStrSet.elements

let get_pool_updates_in_json ~__context ~hosts =
  let ref = get_enabled_repository ~__context in
  let hash = Db.Repository.get_hash ~__context ~self:ref in
  try
    let funs = List.map (fun host ->
        fun () ->
          ( host, (http_get_host_updates_in_json ~__context ~host ~installed:true) )
      ) hosts
    in
    let rets =
      with_pool_repository (fun () ->
          Helpers.run_in_parallel funs capacity_in_parallel)
    in
    let updates_info = parse_updateinfo ~hash in
    let updates_of_hosts, ids_of_updates =
      rets
      |> List.fold_left (fun (acc1, acc2) (host, ret_of_host) ->
          let updates =
            ret_of_host
            |> Yojson.Basic.Util.member "updates"
            |> Yojson.Basic.Util.to_list
            |> List.map update_of_json
          in
          let rpms, uids = List.fold_left (fun (acc_rpms, acc_uids) u ->
              ( ((Printf.sprintf "%s-%s-%s.%s.rpm"
                    u.name u.new_version u.new_release u.arch) :: acc_rpms),
                match u.update_id with
                | Some id -> (UpdateIdSet.add id acc_uids)
                | None -> acc_uids )
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
      ) ([], UpdateIdSet.empty)
    in
    `Assoc [
      ("hosts", `List updates_of_hosts);
      ("updates", `List (UpdateIdSet.elements ids_of_updates |> List.map (fun uid  ->
           json_of_updateinfo (List.assoc uid updates_info))));
      ("hash", `String hash)]
  with
  | Api_errors.(Server_error (code, _)) as e when code <> Api_errors.internal_error ->
    raise e
  | e ->
    error "getting updates for pool failed: %s" (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (get_updates_failed, []))
