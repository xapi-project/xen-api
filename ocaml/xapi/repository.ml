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

open Repository_helpers

module UpdateIdSet = Set.Make (String)

let capacity_in_parallel = 16
let reposync_mutex = Mutex.create ()
(* The cache below is protected by pool's current_operations locking mechanism *)
let updates_in_cache : (API.ref_host, Yojson.Basic.t) Hashtbl.t =
  Hashtbl.create 64

let introduce ~__context ~name_label ~name_description ~binary_url ~source_url ~update =
  assert_url_is_valid ~url:binary_url;
  assert_url_is_valid ~url:source_url;
  Db.Repository.get_all ~__context
  |> List.iter (fun ref ->
      if name_label = Db.Repository.get_name_label ~__context ~self:ref
      || binary_url = Db.Repository.get_binary_url ~__context ~self:ref then
        raise Api_errors.( Server_error (repository_already_exists, [(Ref.string_of ref)]) ));
  create_repository_record
    ~__context ~name_label ~name_description ~binary_url ~source_url ~update

let forget ~__context ~self =
  let pool = Helpers.get_pool ~__context in
  let enabled = Db.Pool.get_repositories ~__context ~self:pool in
  if List.mem self enabled then
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

let cleanup_all_pool_repositories () =
  try
    clean_yum_cache "*" ;
    let prefix = !Xapi_globs.remote_repository_prefix ^ "-" in
    Sys.readdir !Xapi_globs.yum_repos_config_dir
    |> Array.iter (fun file ->
        let open Astring.String in
        if ( (is_prefix ~affix:prefix file) && (is_suffix ~affix:".repo" file) ) then (
          let path = Filename.concat !Xapi_globs.yum_repos_config_dir file in
          Unixext.unlink_safe path)) ;
    Helpers.rmtree !Xapi_globs.local_pool_repo_dir
  with e ->
    error "Failed to clean up all pool repositories: %s" (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (repository_cleanup_failed, []))

let cleanup_pool_repo ~__context ~self =
  let repo_name = get_remote_repository_name ~__context ~self in
  try
    clean_yum_cache repo_name;
    Unixext.unlink_safe (Filename.concat
                           !Xapi_globs.yum_repos_config_dir
                           (repo_name ^ ".repo"));
    Helpers.rmtree (Filename.concat !Xapi_globs.local_pool_repo_dir repo_name)
  with e ->
    error "Failed to clean up pool repository %s: %s" repo_name (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (repository_cleanup_failed, []))

let sync ~__context ~self =
  try
    let repo_name = get_remote_repository_name ~__context ~self in
    remove_repo_conf_file repo_name;
    let binary_url = Db.Repository.get_binary_url  ~__context ~self in
    let source_url = Db.Repository.get_source_url  ~__context ~self in
    write_yum_config ~source_url:(Some source_url) binary_url repo_name;
    let config_params =
      [
        "--save"
      ; if !Xapi_globs.repository_gpgcheck then "--setopt=repo_gpgcheck=1"
        else "--setopt=repo_gpgcheck=0"
      ; repo_name
      ]
    in
    ignore (Helpers.call_script !Xapi_globs.yum_config_manager_cmd config_params);
    (* sync with remote repository *)
    let sync_params =
      [
        "-p"
      ; !Xapi_globs.local_pool_repo_dir
      ; "--downloadcomps"
      ; "--download-metadata"
      ; if !Xapi_globs.repository_gpgcheck then "--gpgcheck" else ""
      ; "--delete"
      ; Printf.sprintf "--repoid=%s" repo_name
      ]
    in
    Unixext.mkdir_rec !Xapi_globs.local_pool_repo_dir 0o700;
    clean_yum_cache repo_name;
    ignore (Helpers.call_script !Xapi_globs.reposync_cmd sync_params)
  with e ->
    error "Failed to sync with remote YUM repository: %s" (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (reposync_failed, []))

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
  let transport = SSL (SSL.make () ~verify_cert:(Stunnel_client.pool ()), host_addr, !Constants.https_port) in
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

let group_host_updates_by_repository ~__context enabled host updates_of_host =
  (* Return updates grouped by repository. Example:
   * [ ("repo-00", [u0, u1]);
   *   ("repo-01", [u3, u4]);
   *   ... ...
   * ]
   *)
  (* TODO: live patches *)
  match Yojson.Basic.Util.member "updates" updates_of_host with
  | `List updates ->
    List.fold_left
      (fun acc update ->
         let upd = Update.of_json update in
         begin match
             Db.Repository.get_by_uuid ~__context ~uuid:(upd.Update.repository)
           with
           | repository when (List.mem repository enabled) ->
             append_by_key acc repository upd
           | repository when not (List.mem repository enabled) ->
             let msg =
               Printf.sprintf "Found update (%s) from a disabled repository"
                 (Update.to_string upd)
             in
             raise Api_errors.(Server_error (internal_error, [msg]))
           | _
           | exception _  ->
             let msg =
               Printf.sprintf "Found update (%s) from an unmanaged repository"
                 (Update.to_string upd)
             in
             raise Api_errors.(Server_error (internal_error, [msg]))
         end
      )
      [] updates
  | _ ->
    let ref = Ref.string_of host in
    error "Invalid updates from host ref='%s': No 'updates'" ref ;
    raise Api_errors.(Server_error (get_host_updates_failed, [ref]))
  | exception e ->
    let ref = Ref.string_of host in
    error "Invalid updates from host ref='%s': %s" ref (ExnHelper.string_of_exn e) ;
    raise Api_errors.(Server_error (get_host_updates_failed, [ref]))

let set_available_updates ~__context =
  ignore (get_single_enabled_update_repository ~__context) ;
  let hosts = Db.Host.get_all ~__context in
  let funs =
    List.map (fun host ->
        fun () ->
          ( host, (http_get_host_updates_in_json ~__context ~host ~installed:true) )
      ) hosts
  in
  let rets = with_pool_repositories (fun () ->
      Helpers.run_in_parallel funs capacity_in_parallel)
  in
  let enabled = get_enabled_repositories ~__context in
  (* Group updates by repository for each host *)
  let updates_of_hosts =
    List.map (fun (h, updates_of_host) ->
        group_host_updates_by_repository ~__context enabled h updates_of_host) rets
  in
  (* Group updates by repository for all hosts *)
  let updates_by_repository =
    List.fold_left
      (fun acc l -> List.fold_left (fun acc' (x, y) -> append_by_key acc' x y) acc l)
      [] updates_of_hosts
  in
  let checksums =
    List.filter_map
      (fun repository ->
         let up_to_date =
           match List.assoc_opt repository updates_by_repository with
           | Some (_::_) -> false
           | _ -> true
         in
         Db.Repository.set_up_to_date ~__context ~self:repository ~value:up_to_date ;
         if (Db.Repository.get_update ~__context ~self:repository) then (
           let repo_name = get_remote_repository_name ~__context ~self:repository in
           let xml_path =
             "repodata/repomd.xml"
             |> Filename.concat repo_name
             |> Filename.concat !Xapi_globs.local_pool_repo_dir
           in
           let md = UpdateInfoMetaData.of_xml_file xml_path in
           Db.Repository.set_hash ~__context ~self:repository
             ~value:(md.UpdateInfoMetaData.checksum) ;
           Some md.UpdateInfoMetaData.checksum)
         else None)
      enabled
  in
  Hashtbl.clear updates_in_cache ;
  Hashtbl.add_seq updates_in_cache (List.to_seq rets) ;
  get_singleton checksums

let create_pool_repository ~__context ~self =
  let repo_name = get_remote_repository_name ~__context ~self in
  let repo_dir = Filename.concat !Xapi_globs.local_pool_repo_dir repo_name in
  match Sys.file_exists repo_dir with
  | true ->
    (try
       ignore (Helpers.call_script !Xapi_globs.createrepo_cmd [repo_dir]);
       if (Db.Repository.get_update ~__context ~self) then (
         let cachedir = get_cachedir repo_name in
         let md = UpdateInfoMetaData.of_xml_file (Filename.concat cachedir "repomd.xml") in
         let updateinfo_xml_gz_path =
           Filename.concat repo_dir (md.UpdateInfoMetaData.checksum ^ "-updateinfo.xml.gz")
         in
         begin match Sys.file_exists updateinfo_xml_gz_path with
           | true ->
             with_updateinfo_xml updateinfo_xml_gz_path (fun xml_file_path ->
                 let repodata_dir = Filename.concat repo_dir "repodata" in
                 ignore (Helpers.call_script !Xapi_globs.modifyrepo_cmd
                           ["--remove"; "updateinfo"; repodata_dir]);
                 ignore (Helpers.call_script !Xapi_globs.modifyrepo_cmd
                           ["--mdtype"; "updateinfo"; xml_file_path; repodata_dir]))
           | false ->
             error "No updateinfo.xml.gz found: %s" updateinfo_xml_gz_path;
             raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
         end
       )
     with
     | Api_errors.(Server_error (code, _)) as e when code <> Api_errors.internal_error ->
       raise e
     | e ->
       error "Creating local pool repository failed: %s" (ExnHelper.string_of_exn e);
       raise Api_errors.(Server_error (createrepo_failed, [])))
  | false ->
    error "local pool repository directory '%s' does not exist" repo_dir;
    raise Api_errors.(Server_error (reposync_failed, []))

let get_host_updates_in_json ~__context ~installed =
  try
    with_local_repositories ~__context (fun repositories ->
      let rpm2updates = get_updates_from_updateinfo ~__context repositories in
      let installed_pkgs = match installed with
        | true -> get_installed_pkgs ()
        | false -> []
      in
      let params_of_list =
        [
          "-q"
        ; "--disablerepo=*"
        ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
        ; "list"
        ; "updates"
        ]
      in
      List.iter (fun r -> clean_yum_cache r) repositories ;
      let updates =
        Helpers.call_script !Xapi_globs.yum_cmd params_of_list
        |> Astring.String.cuts ~sep:"\n"
        |> List.filter_map
          (get_rpm_update_in_json ~rpm2updates ~installed_pkgs)
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

(* This handler hosts HTTP endpoint '/repository' which will be available iif
 * 'is_local_pool_repo_enabled' returns true with 'with_pool_repositories' being called by
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
            let root = !Xapi_globs.local_pool_repo_dir in
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

let parse_updateinfo ~__context ~self =
  let hash = Db.Repository.get_hash ~__context ~self in
  let repo_name = get_remote_repository_name ~__context ~self in
  let repo_dir = Filename.concat !Xapi_globs.local_pool_repo_dir repo_name in
  let repodata_dir = Filename.concat repo_dir "repodata" in
  let repomd_xml_path = Filename.concat repodata_dir "repomd.xml" in
  let md = UpdateInfoMetaData.of_xml_file repomd_xml_path in
  if hash <> md.UpdateInfoMetaData.checksum then (
    let msg =
      Printf.sprintf
        "Unexpected mismatch between XAPI DB (%s) and YUM DB (%s). \
         Need to do pool.sync-updates again."
        hash md.UpdateInfoMetaData.checksum
    in
    error "%s: %s" repo_name msg;
    raise Api_errors.(Server_error (internal_error, [msg]))
  );
  let updateinfo_xml_gz_path = Filename.concat repo_dir md.UpdateInfoMetaData.location in
  match Sys.file_exists updateinfo_xml_gz_path with
  | false ->
    error "File %s doesn't exist" updateinfo_xml_gz_path;
    raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
  | true ->
    with_updateinfo_xml updateinfo_xml_gz_path UpdateInfo.of_xml_file

let get_pool_updates_in_json ~__context ~hosts =
  try
    let repository = get_single_enabled_update_repository ~__context in
    if Hashtbl.length updates_in_cache > 0 then (
      let repository_name = get_repository_name  ~__context ~self:repository in
      let updates_info = parse_updateinfo ~__context ~self:repository in
      let updates_of_hosts, ids_of_updates =
        Hashtbl.fold
          (fun host updates_of_host (acc1, acc2) ->
            if List.mem host hosts then (
              let json_of_host, uids =
                consolidate_updates_of_host ~repository_name ~updates_info
                  (Ref.string_of host) updates_of_host
              in
              ( (json_of_host :: acc1), (UpdateIdSet.union uids acc2) ))
            else
              (acc1, acc2))
          updates_in_cache
          ([], UpdateIdSet.empty)
      in
      `Assoc [
        ("hosts", `List updates_of_hosts);
        ("updates", `List (UpdateIdSet.elements ids_of_updates
                           |> List.map (fun uid  ->
                               UpdateInfo.to_json (List.assoc uid updates_info))));
        ("hash", `String (Db.Repository.get_hash ~__context ~self:repository))])
    else
      raise Api_errors.(Server_error (updates_require_sync, []))
  with
  | Api_errors.(Server_error (code, _)) as e when code <> Api_errors.internal_error ->
    raise e
  | e ->
    error "getting updates for pool failed: %s" (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (get_updates_failed, []))

let apply ~__context ~host =
  (* This function runs on slave host *)
  with_local_repositories ~__context (fun repositories ->
      let params =
        [
          "-y"
        ; "--disablerepo=*"
        ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
        ; "upgrade"
        ]
      in
      try ignore (Helpers.call_script !Xapi_globs.yum_cmd params)
      with e ->
        let ref = Ref.string_of host in
        error "Failed to apply updates on host ref='%s': %s" ref (ExnHelper.string_of_exn e);
        raise Api_errors.(Server_error (apply_updates_failed, [ref])))

let restart_device_models ~__context host =
  (* Restart device models of all running HVM VMs on the host by doing
   * local migrations. *)
  Db.Host.get_resident_VMs ~__context ~self:host
  |> List.map (fun self -> (self, Db.VM.get_record ~__context ~self))
  |> List.filter (fun (_, record) -> not record.API.vM_is_control_domain)
  |> List.filter_map (fun (ref, record) ->
      match record.API.vM_power_state,
            Helpers.has_qemu_currently ~__context ~self:ref with
      | `Running, true ->
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            Client.Client.VM.pool_migrate rpc session_id ref host [("live", "true")]);
        None
      | `Paused, true ->
        error "VM 'ref=%s' is paused, can't restart its device models" (Ref.string_of ref);
        Some ref
      | _ ->
        (* No device models are running for this VM *)
        None)
  |> function
  | [] -> ()
  | _ :: _ ->
    let msg = "Can't restart device models for some VMs" in
    raise Api_errors.(Server_error (internal_error, [msg]))

let apply_immediate_guidances ~__context ~host ~guidances =
  (* This function runs on master host *)
  try
    let num_of_hosts = List.length (Db.Host.get_all ~__context) in
    let open Client in
    Helpers.call_api_functions ~__context (fun rpc session_id ->
        let open Guidance in
        match guidances with
        | [] ->
          ()
        | [RebootHost] ->
          Client.Host.reboot ~rpc ~session_id ~host
        | [EvacuateHost] ->
          (* EvacuatHost should be done before applying updates by XAPI users.
           * Here only the guidances to be applied after applying updates are handled.
           *)
          ()
        | [RestartDeviceModel] ->
          restart_device_models ~__context host
        | [RestartToolstack] ->
          Client.Host.restart_agent ~rpc ~session_id ~host
        | l when eq_set1 l ->
          (* EvacuateHost and RestartToolstack *)
          Client.Host.restart_agent ~rpc ~session_id ~host
        | l when eq_set2 l ->
          (* RestartDeviceModel and RestartToolstack *)
          restart_device_models ~__context host;
          Client.Host.restart_agent ~rpc ~session_id ~host
        | l when eq_set3 l ->
          (* RestartDeviceModel and EvacuateHost *)
          (* Evacuating host restarted device models already *)
          if num_of_hosts = 1 then restart_device_models ~__context host;
          ()
        | l when eq_set4 l ->
          (* EvacuateHost, RestartToolstack and RestartDeviceModel *)
          (* Evacuating host restarted device models already *)
          if num_of_hosts = 1 then restart_device_models ~__context host;
          Client.Host.restart_agent ~rpc ~session_id ~host
        | l ->
          let ref = Ref.string_of host in
          error "Found wrong guidance(s) after applying updates on host ref='%s': %s"
            ref (String.concat ";" (List.map Guidance.to_string l));
          raise Api_errors.(Server_error (apply_guidance_failed, [ref])))
  with e ->
    let ref = Ref.string_of host in
    error "applying immediate guidances on host ref='%s' failed: %s"
      ref (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (apply_guidance_failed, [ref]))

let apply_updates ~__context ~host ~hash =
  (* This function runs on master host *)
  try
    let repository = get_single_enabled_update_repository ~__context in
    if hash = "" || hash <> Db.Repository.get_hash ~__context ~self:repository then
      raise Api_errors.(Server_error (updateinfo_hash_mismatch, []));
    with_pool_repositories (fun () ->
        let all_updates =
          http_get_host_updates_in_json ~__context ~host ~installed:true
          |> Yojson.Basic.Util.member "updates"
          |> Yojson.Basic.Util.to_list
          |> List.map Update.of_json
        in
        match all_updates with
        | [] ->
          let ref = Ref.string_of host in
          info "Host ref='%s' is already up to date." ref;
          []
        | l ->
          let repository_name = get_repository_name  ~__context ~self:repository in
          let updates =
            List.filter (fun u -> u.Update.repository = repository_name) l
          in
          let updates_info = parse_updateinfo ~__context ~self:repository in
          let immediate_guidances =
            eval_guidances ~updates_info ~updates ~kind:Recommended
          in
          Guidance.assert_valid_guidances immediate_guidances;
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              Client.Client.Repository.apply ~rpc ~session_id ~host);
          (* TODO: absolute guidances *)

          Hashtbl.replace updates_in_cache host (`Assoc [("updates", `List [])]) ;
          immediate_guidances)
  with
  | Api_errors.(Server_error (code, _)) as e when code <> Api_errors.internal_error ->
    raise e
  | e ->
    let ref = Ref.string_of host in
    error "applying updates on host ref='%s' failed: %s" ref (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (apply_updates_failed, [ref]))

let reset_updates_in_cache () =
  Hashtbl.clear updates_in_cache
