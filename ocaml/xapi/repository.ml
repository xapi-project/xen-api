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
open Updateinfo
open Repository_helpers
module UpdateIdSet = Set.Make (String)

module Pkgs = (val Pkg_mgr.get_pkg_mgr)

let capacity_in_parallel = 16

let ( // ) = Filename.concat

(* The cache below is protected by pool's current_operations locking mechanism *)
let updates_in_cache : (API.ref_host, Yojson.Basic.t) Hashtbl.t =
  Hashtbl.create 64

let introduce ~__context ~name_label ~name_description ~binary_url ~source_url
    ~update ~gpgkey_path =
  assert_url_is_not_blocked ~url:binary_url ;
  assert_url_is_not_blocked ~url:source_url ;
  assert_url_is_valid ~url:binary_url ;
  assert_url_is_valid ~url:source_url ;
  assert_gpgkey_path_is_valid gpgkey_path ;
  Db.Repository.get_all ~__context
  |> List.iter (fun ref ->
         if
           name_label = Db.Repository.get_name_label ~__context ~self:ref
           || binary_url = Db.Repository.get_binary_url ~__context ~self:ref
         then
           raise
             Api_errors.(
               Server_error (repository_already_exists, [Ref.string_of ref])
             )
     ) ;
  create_repository_record ~__context ~name_label ~name_description ~binary_url
    ~source_url ~update ~gpgkey_path ~origin:`remote ~certificate:""

let introduce_bundle ~__context ~name_label ~name_description =
  Db.Repository.get_all ~__context
  |> List.iter (fun ref ->
         if name_label = Db.Repository.get_name_label ~__context ~self:ref then
           raise
             Api_errors.(
               Server_error (repository_already_exists, [Ref.string_of ref])
             ) ;
         if Db.Repository.get_origin ~__context ~self:ref = `bundle then
           raise
             Api_errors.(
               Server_error
                 (bundle_repository_already_exists, [Ref.string_of ref])
             )
     ) ;
  create_repository_record ~__context ~name_label ~name_description
    ~binary_url:"" ~source_url:"" ~update:true ~gpgkey_path:"" ~origin:`bundle
    ~certificate:""

let introduce_remote_pool ~__context ~name_label ~name_description ~binary_url
    ~certificate =
  assert_remote_pool_url_is_valid ~url:binary_url ;
  Db.Repository.get_all ~__context
  |> List.iter (fun ref ->
         if
           name_label = Db.Repository.get_name_label ~__context ~self:ref
           || binary_url = Db.Repository.get_binary_url ~__context ~self:ref
         then
           raise
             Api_errors.(
               Server_error (repository_already_exists, [Ref.string_of ref])
             )
     ) ;
  create_repository_record ~__context ~name_label ~name_description ~binary_url
    ~source_url:"" ~update:true ~gpgkey_path:"" ~origin:`remote_pool
    ~certificate

let forget ~__context ~self =
  let pool = Helpers.get_pool ~__context in
  let enabled = Db.Pool.get_repositories ~__context ~self:pool in
  if List.mem self enabled then
    raise Api_errors.(Server_error (repository_is_in_use, []))
  else
    Db.Repository.destroy ~__context ~self

let set_gpgkey_path ~__context ~self ~value =
  assert_gpgkey_path_is_valid value ;
  Db.Repository.set_gpgkey_path ~__context ~self ~value

let cleanup_all_pool_repositories () =
  try
    clean_yum_cache "*" ;
    let prefix = !Xapi_globs.remote_repository_prefix ^ "-" in
    Sys.readdir !Xapi_globs.yum_repos_config_dir
    |> Array.iter (fun file ->
           let open Astring.String in
           if is_prefix ~affix:prefix file && is_suffix ~affix:".repo" file then
             let path = Filename.concat !Xapi_globs.yum_repos_config_dir file in
             Unixext.unlink_safe path
       ) ;
    Xapi_stdext_unix.Unixext.rm_rec !Xapi_globs.local_pool_repo_dir
  with e ->
    error "Failed to clean up all pool repositories: %s"
      (ExnHelper.string_of_exn e) ;
    raise Api_errors.(Server_error (repository_cleanup_failed, []))

let cleanup_pool_repo ~__context ~self =
  let repo_name = get_remote_repository_name ~__context ~self in
  try
    clean_yum_cache repo_name ;
    Unixext.unlink_safe
      (Filename.concat !Xapi_globs.yum_repos_config_dir (repo_name ^ ".repo")) ;
    Xapi_stdext_unix.Unixext.rm_rec
      (Filename.concat !Xapi_globs.local_pool_repo_dir repo_name)
  with e ->
    error "Failed to clean up pool repository %s: %s" repo_name
      (ExnHelper.string_of_exn e) ;
    raise Api_errors.(Server_error (repository_cleanup_failed, []))

let get_proxy_params ~__context =
  let pool = Helpers.get_pool ~__context in
  let url = Db.Pool.get_repository_proxy_url ~__context ~self:pool in
  let username = Db.Pool.get_repository_proxy_username ~__context ~self:pool in
  let password_ref =
    Db.Pool.get_repository_proxy_password ~__context ~self:pool
  in
  match (url, username, password_ref) with
  | url', username', password_ref'
    when url' <> "" && username' <> "" && password_ref' <> Ref.null ->
      [
        Printf.sprintf "proxy=%s" url'
      ; Printf.sprintf "proxy_username=%s" username'
      ; Printf.sprintf "proxy_password=%s"
          (Db.Secret.get_value ~__context ~self:password_ref')
      ]
  | url', "", _ when url' <> "" ->
      [Printf.sprintf "proxy=%s" url']
  | _ ->
      []

let sync ~__context ~self ~token ~token_id ~username ~password =
  try
    let repo_name = get_remote_repository_name ~__context ~self in
    remove_repo_conf_file repo_name ;
    let origin = Db.Repository.get_origin ~__context ~self in

    let binary_url, source_url, use_proxy, client_auth, server_auth =
      match origin with
      | `remote ->
          let plugin = "accesstoken" in
          ( Db.Repository.get_binary_url ~__context ~self
          , Some (Db.Repository.get_source_url ~__context ~self)
          , true
          , CdnTokenAuth {token_id; token; plugin}
          , DefaultAuth
          )
      | `bundle ->
          let uri =
            Uri.make ~scheme:"file" ~path:!Xapi_globs.bundle_repository_dir ()
          in
          (Uri.to_string uri, None, false, NoAuth, NoAuth)
      | `remote_pool ->
          let cert = Db.Repository.get_certificate ~__context ~self in
          let repo_binary_url = Db.Repository.get_binary_url ~__context ~self in
          let remote_addr =
            repo_binary_url |> Repository_helpers.get_remote_pool_coordinator_ip
          in
          let verified_rpc =
            try
              Helpers.make_external_host_verified_rpc ~__context remote_addr
                cert
            with Xmlrpc_client.Connection_reset ->
              raise
                (Api_errors.Server_error
                   ( Api_errors
                     .update_syncing_remote_pool_coordinator_connection_failed
                   , []
                   )
                )
          in
          let session_id =
            try
              Client.Client.Session.login_with_password ~rpc:verified_rpc
                ~uname:username ~pwd:password
                ~version:Datamodel_common.api_version_string
                ~originator:Xapi_version.xapi_user_agent
            with
            | Http_client.Http_request_rejected _ | Http_client.Http_error _ ->
              raise
                (Api_errors.Server_error
                   ( Api_errors
                     .update_syncing_remote_pool_coordinator_service_failed
                   , []
                   )
                )
          in
          let xapi_token = session_id |> Ref.string_of in
          let plugin = "xapitoken" in
          ( repo_binary_url
          , None
          , true
          , PoolExtHostAuth {xapi_token; plugin}
          , StunnelClientProxyAuth
              {cert; remote_addr; remote_port= Constants.default_ssl_port}
          )
    in
    let gpgkey_path =
      match Db.Repository.get_gpgkey_path ~__context ~self with
      | "" ->
          !Xapi_globs.repository_gpgkey_name
      | s ->
          s
    in
    let write_initial_yum_config ~binary_url =
      write_yum_config ~source_url ~binary_url ~repo_gpgcheck:true ~gpgkey_path
        ~repo_name
    in
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () ->
        let config_repo config =
          if config <> [] then (* Set params to yum/dnf *)
            let Pkg_mgr.{cmd; params} = Pkgs.config_repo ~repo_name ~config in
            ignore
              (Helpers.call_script ~log_output:Helpers.On_failure cmd params)
          else
            debug "%s: Skip configure repo as no valid params to configure"
              __FUNCTION__
        in
        let make_cache () =
          (* Import YUM repository GPG key to check metadata in reposync *)
          let Pkg_mgr.{cmd; params} = Pkgs.make_cache ~repo_name in
          ignore (Helpers.call_script cmd params)
        in

        (* Sync with remote repository *)
        let sync_repo () =
          let Pkg_mgr.{cmd; params} = Pkgs.sync_repo ~repo_name in
          Unixext.mkdir_rec !Xapi_globs.local_pool_repo_dir 0o700 ;
          clean_yum_cache repo_name ;
          ignore (Helpers.call_script cmd params)
        in

        with_sync_client_auth client_auth @@ fun client_auth ->
        with_sync_server_auth server_auth @@ fun binary_url' ->
        write_initial_yum_config
          ~binary_url:(Option.value binary_url' ~default:binary_url) ;
        clean_yum_cache repo_name ;
        (* Remove imported YUM repository GPG key *)
        if Pkgs.manager = Yum then
          Xapi_stdext_unix.Unixext.rm_rec (get_repo_config repo_name "gpgdir") ;
        let auth_params =
          match client_auth with
          | Some (auth_file, plugin) ->
              let token_param =
                Printf.sprintf "%s=%s" plugin
                  (Uri.make ~scheme:"file" ~path:auth_file () |> Uri.to_string)
              in
              [token_param]
          | None ->
              []
        in
        let proxy_params =
          match use_proxy with
          | true ->
              get_proxy_params ~__context
          | false ->
              []
        in
        auth_params @ proxy_params |> fun x ->
        config_repo x ; make_cache () ; sync_repo ()
      )
      (fun () ->
        (* Rewrite repo conf file as initial content to remove credential
         * related info, I.E. proxy username/password and temporary token file
         * path.
         * One thing to note: for remote_repo, the binary_url used to
         * re-initial yum repo is the url configed in the remote_pool repo,
         * which is not the correct one for stunnel client proxy, while as we
         * will always write_initial_yum_config every time before syncing repo,
         * this should be ok.
         *)
        write_initial_yum_config ~binary_url
      ) ;
    (* The custom yum-utils will fully download repository metadata including
     * the repo gpg signature.
     *)
    let repo_gpg_signature =
      !Xapi_globs.local_pool_repo_dir
      // repo_name
      // "repodata"
      // "repomd.xml.asc"
    in
    Sys.file_exists repo_gpg_signature
  with
  | Api_errors.Server_error (_, _) as e ->
      raise e
  | Stunnel.Stunnel_verify_error reason ->
      raise (Api_errors.Server_error (Api_errors.ssl_verify_error, [reason]))
  | e ->
      error "Failed to sync with remote YUM repository: %s"
        (ExnHelper.string_of_exn e) ;
      raise Api_errors.(Server_error (reposync_failed, []))

let http_get_host_updates_in_json ~__context ~host ~installed =
  let host_session_id =
    Xapi_session.login_no_password ~__context ~uname:None ~host ~pool:true
      ~is_local_superuser:true ~subject:Ref.null ~auth_user_sid:""
      ~auth_user_name:"" ~rbac_permissions:[]
  in
  let request =
    Xapi_http.http_request
      ~cookie:[("session_id", Ref.string_of host_session_id)]
      ~query:[("installed", string_of_bool installed)]
      Http.Get Constants.get_host_updates_uri
  in
  let host_name = Db.Host.get_hostname ~__context ~self:host in
  let host_addr = Db.Host.get_address ~__context ~self:host in
  let open Xmlrpc_client in
  let transport =
    SSL
      ( SSL.make () ~verify_cert:(Stunnel_client.pool ())
      , host_addr
      , !Constants.https_port
      )
  in
  debug "getting host updates on %s (addr %s) by HTTP GET" host_name host_addr ;
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      try
        let json_str =
          with_transport transport
            (with_http request (fun (_, fd) ->
                 Xapi_stdext_unix.Unixext.string_of_fd fd
             )
            )
        in
        debug "host %s returned updates: %s" host_name json_str ;
        Yojson.Basic.from_string json_str
      with e ->
        let host' = Ref.string_of host in
        error "Failed to get updates from host ref='%s': %s" host'
          (ExnHelper.string_of_exn e) ;
        raise Api_errors.(Server_error (get_host_updates_failed, [host']))
    )
    (fun () -> Xapi_session.destroy_db_session ~__context ~self:host_session_id)

let parse_updateinfo ~__context ~self ~check =
  let repo_name = get_remote_repository_name ~__context ~self in
  let repo_dir = Filename.concat !Xapi_globs.local_pool_repo_dir repo_name in
  let repodata_dir = Filename.concat repo_dir "repodata" in
  let repomd_xml_path = Filename.concat repodata_dir "repomd.xml" in
  let md = RepoMetaData.(of_xml_file repomd_xml_path UpdateInfo) in
  ( if check then
      let hash = Db.Repository.get_hash ~__context ~self in
      if hash <> md.RepoMetaData.checksum then (
        let msg =
          Printf.sprintf
            "Unexpected mismatch between XAPI DB (%s) and YUM DB (%s). Need to \
             do pool.sync-updates again."
            hash md.RepoMetaData.checksum
        in
        error "%s: %s" repo_name msg ;
        Helpers.internal_error "%s" msg
      )
  ) ;
  let updateinfo_xml_gz_path =
    Filename.concat repo_dir md.RepoMetaData.location
  in
  match Sys.file_exists updateinfo_xml_gz_path with
  | false ->
      error "File %s doesn't exist" updateinfo_xml_gz_path ;
      raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
  | true ->
      with_updateinfo_xml updateinfo_xml_gz_path UpdateInfo.of_xml_file

let get_hosts_updates ~__context =
  let hosts = Db.Host.get_all ~__context in
  let funs =
    List.map
      (fun host () ->
        (host, http_get_host_updates_in_json ~__context ~host ~installed:true)
      )
      hosts
  in
  with_pool_repositories (fun () ->
      Helpers.run_in_parallel ~funs ~capacity:capacity_in_parallel
  )

let get_applied_livepatches_of_host updates_of_host =
  get_list_from_updates_of_host "applied_livepatches" updates_of_host
  |> List.map Livepatch.of_json

let is_livepatchable ~__context repository applied_livepatches_of_host =
  let updates_info =
    parse_updateinfo ~__context ~self:repository ~check:false |> snd
  in
  List.exists
    (fun lp ->
      get_accumulative_livepatches ~since:lp ~updates_info |> function
      | [] ->
          false
      | _ ->
          true
    )
    applied_livepatches_of_host

let set_available_updates ~__context =
  ignore (get_single_enabled_update_repository ~__context) ;
  let enabled = get_enabled_repositories ~__context in
  let hosts_updates = get_hosts_updates ~__context in
  List.iter
    (fun (h, updates_of_host) ->
      let latest_synced_updates_applied =
        match get_list_from_updates_of_host "updates" updates_of_host with
        | [] -> (
            (* No RPM packages to be updated.
             * Find out if there are available livepatches from a update repo
             *)
            let update_repo =
              get_singleton
                (List.filter
                   (fun repo -> Db.Repository.get_update ~__context ~self:repo)
                   enabled
                )
            in
            let livepatchable =
              is_livepatchable ~__context update_repo
                (get_applied_livepatches_of_host updates_of_host)
            in
            match livepatchable with true -> `no | false -> `yes
          )
        | _ ->
            `no
      in
      Db.Host.set_latest_synced_updates_applied ~__context ~self:h
        ~value:latest_synced_updates_applied
    )
    hosts_updates ;
  let checksums =
    List.filter_map
      (fun repository ->
        let is_update_repo =
          Db.Repository.get_update ~__context ~self:repository
        in
        if is_update_repo then (
          let repo_name =
            get_remote_repository_name ~__context ~self:repository
          in
          let xml_path =
            "repodata/repomd.xml"
            |> Filename.concat repo_name
            |> Filename.concat !Xapi_globs.local_pool_repo_dir
          in
          let md = RepoMetaData.(of_xml_file xml_path UpdateInfo) in
          Db.Repository.set_hash ~__context ~self:repository
            ~value:md.RepoMetaData.checksum ;
          Some md.RepoMetaData.checksum
        ) else
          None
      )
      enabled
  in
  Hashtbl.clear updates_in_cache ;
  Hashtbl.add_seq updates_in_cache (List.to_seq hosts_updates) ;
  get_singleton checksums

let create_pool_repository ~__context ~self =
  let repo_name = get_remote_repository_name ~__context ~self in
  let repo_dir = Filename.concat !Xapi_globs.local_pool_repo_dir repo_name in
  match Sys.file_exists repo_dir with
  | true -> (
    try
      let cachedir = get_repo_config repo_name "cachedir" in
      let group_params =
        match
          RepoMetaData.(
            of_xml_file (Filename.concat cachedir "repomd.xml") Group
          )
        with
        | RepoMetaData.{checksum= _; location} ->
            ["-g"; Filename.concat cachedir (Filename.basename location)]
        | exception _ ->
            []
      in
      ignore
        (Helpers.call_script !Xapi_globs.createrepo_cmd
           (group_params @ [repo_dir])
        ) ;
      if Db.Repository.get_update ~__context ~self then
        let md =
          RepoMetaData.(
            of_xml_file (Filename.concat cachedir "repomd.xml") UpdateInfo
          )
        in
        let updateinfo_xml_gz_path =
          Filename.concat repo_dir (Filename.basename md.RepoMetaData.location)
        in
        match Sys.file_exists updateinfo_xml_gz_path with
        | true ->
            with_updateinfo_xml updateinfo_xml_gz_path (fun xml_file_path ->
                let repodata_dir = Filename.concat repo_dir "repodata" in
                ignore
                  (Helpers.call_script !Xapi_globs.modifyrepo_cmd
                     ["--remove"; "updateinfo"; repodata_dir]
                  ) ;
                ignore
                  (Helpers.call_script !Xapi_globs.modifyrepo_cmd
                     ["--mdtype"; "updateinfo"; xml_file_path; repodata_dir]
                  )
            )
        | false ->
            error "No updateinfo.xml.gz found: %s" updateinfo_xml_gz_path ;
            raise Api_errors.(Server_error (invalid_updateinfo_xml, []))
    with
    | Api_errors.(Server_error (code, _)) as e
      when code <> Api_errors.internal_error ->
        raise e
    | e ->
        error "Creating local pool repository failed: %s"
          (ExnHelper.string_of_exn e) ;
        raise Api_errors.(Server_error (createrepo_failed, []))
  )
  | false ->
      error "local pool repository directory '%s' does not exist" repo_dir ;
      raise Api_errors.(Server_error (reposync_failed, []))

let get_host_updates_in_json ~__context ~installed =
  try
    with_local_repositories ~__context (fun repositories ->
        (* (pkg, update_id) list *)
        let accumulative_updates =
          get_updates_from_updateinfo ~__context repositories
        in
        (* (name_arch, pkg) list *)
        let installed_pkgs =
          match installed with true -> get_installed_pkgs () | false -> []
        in
        let applied_livepatches_in_json =
          Livepatch.get_applied_livepatches () |> List.map Livepatch.to_json
        in
        (* (pkg: Rpm.Pkg.t, repo: string) list *)
        let latest_updates' =
          get_updates_from_yum_upgrade_dry_run repositories
        in
        let latest_updates'' = get_updates_from_repoquery repositories in
        (* To ensure the updating function will not strand, use redundant
         * functions to get the update/installation list.
         * Falling back to use "repoquery" when
         * 1. parsing output of "yum upgrade (dry run)" fails, or
         * 2. the list from "yum upgrade (dry run)" is not a subset of the
         *    one from "repoquery".
         *)
        let fail_on_error = Xapi_fist.fail_on_error_in_yum_upgrade_dry_run () in
        let latest_updates =
          get_latest_updates_from_redundancy ~fail_on_error
            ~pkgs:latest_updates' ~fallback_pkgs:latest_updates''
        in
        List.iter (fun r -> clean_yum_cache r) repositories ;
        let latest_updates_in_json =
          validate_latest_updates ~latest_updates ~accumulative_updates
          |> List.map (get_update_in_json ~installed_pkgs:[])
        in
        let accumulative_updates_in_json =
          prune_accumulative_updates ~accumulative_updates ~latest_updates
            ~installed_pkgs
          |> List.map (get_update_in_json ~installed_pkgs)
        in
        `Assoc
          [
            ("updates", `List latest_updates_in_json)
          ; ("accumulative_updates", `List accumulative_updates_in_json)
          ; ("applied_livepatches", `List applied_livepatches_in_json)
          ]
    )
  with
  | Api_errors.(Server_error (code, _)) as e
    when code <> Api_errors.internal_error ->
      raise e
  | e ->
      let ref = Ref.string_of (Helpers.get_localhost ~__context) in
      error "Failed to get host updates on host ref=%s: %s" ref
        (ExnHelper.string_of_exn e) ;
      raise Api_errors.(Server_error (get_host_updates_failed, [ref]))

let get_repository_handler (req : Http.Request.t) s _ =
  let open Http in
  debug "%s URL: %s" __FUNCTION__ req.Request.path ;
  req.Request.close <- true ;
  Fileserver.send_file Constants.get_repository_uri
    !Xapi_globs.local_pool_repo_dir
    req s ()

let get_enabled_repository_handler (req : Http.Request.t) s _ =
  let open Http in
  debug "%s URL: %s" __FUNCTION__ req.Request.path ;
  req.Request.close <- true ;
  Xapi_http.with_context __FUNCTION__ req s (fun __context ->
      let enabled_repo = get_single_enabled_update_repository ~__context in
      let repo_name =
        get_remote_repository_name ~__context ~self:enabled_repo
      in
      Fileserver.send_file Constants.get_enabled_repository_uri
        (Filename.concat !Xapi_globs.local_pool_repo_dir repo_name)
        req s ()
  )

let consolidate_updates_of_hosts ~repository_name ~updates_info ~hosts =
  Hashtbl.fold
    (fun host updates_of_host (acc1, acc2) ->
      if List.mem host hosts then
        let updates_of_host, upd_ids =
          consolidate_updates_of_host ~repository_name ~updates_info
            (Ref.string_of host) updates_of_host
        in
        (updates_of_host :: acc1, UpdateIdSet.union upd_ids acc2)
      else
        (acc1, acc2)
    )
    updates_in_cache ([], UpdateIdSet.empty)

let get_pool_updates_in_json ~__context ~hosts =
  try
    let repository = get_single_enabled_update_repository ~__context in
    if Hashtbl.length updates_in_cache = 0 then
      set_available_updates ~__context |> ignore ;

    let repository_name = get_repository_name ~__context ~self:repository in
    let api_ver, updates_info =
      parse_updateinfo ~__context ~self:repository ~check:true
    in
    let updates_of_hosts, ids_of_updates =
      consolidate_updates_of_hosts ~repository_name ~updates_info ~hosts
    in
    let lps =
      updates_of_hosts
      |> List.concat_map (fun x -> x.HostUpdates.livepatches)
      |> LivePatchSet.of_list
    in
    let updateinfo_list =
      UpdateIdSet.elements ids_of_updates
      |> List.map (fun upd_id -> List.assoc upd_id updates_info)
      |> List.map (prune_updateinfo_for_livepatches lps)
    in
    let f x =
      Option.fold ~none:x
        ~some:(fun api_ver -> ("xapi-api-version", `String api_ver) :: x)
        api_ver
    in
    [
      ("hosts", `List (List.map HostUpdates.to_json updates_of_hosts))
    ; ("updates", `List (List.map UpdateInfo.to_json updateinfo_list))
    ; ("hash", `String (Db.Repository.get_hash ~__context ~self:repository))
    ]
    |> fun x -> `Assoc (f x)
  with
  | Api_errors.(Server_error (code, _)) as e
    when code <> Api_errors.internal_error ->
      raise e
  | e ->
      error "getting updates for pool failed: %s" (ExnHelper.string_of_exn e) ;
      raise Api_errors.(Server_error (get_updates_failed, []))

let apply ~__context ~host =
  (* This function runs on member host *)
  with_local_repositories ~__context (fun repositories ->
      let Pkg_mgr.{cmd; params} = Pkgs.apply_upgrade ~repositories in
      try ignore (Helpers.call_script cmd params)
      with e ->
        let host' = Ref.string_of host in
        error "Failed to apply updates on host ref='%s': %s" host'
          (ExnHelper.string_of_exn e) ;
        raise Api_errors.(Server_error (apply_updates_failed, [host']))
  )

let apply_livepatch ~__context ~host:_ ~component ~base_build_id ~base_version
    ~base_release ~to_version ~to_release =
  (* This function runs on member host *)
  let open Livepatch in
  let component' =
    try component_of_string component
    with _ ->
      Helpers.internal_error ~log_err:true "Invalid component name '%s'"
        component
  in
  match
    Livepatch.get_livepatch_file_path ~component:component' ~base_build_id
      ~base_version ~base_release ~to_version ~to_release
  with
  | Some livepatch_file ->
      Livepatch.apply ~component:component' ~livepatch_file ~base_build_id
        ~base_version ~base_release ~to_version ~to_release ;
      Create_misc.create_software_version ~__context ()
  | None ->
      Helpers.internal_error ~log_err:true "No expected livepatch file for %s"
        component

let apply_livepatches' ~__context ~host ~livepatches =
  List.partition_map
    (fun (lp, lps) ->
      let component_str =
        Livepatch.string_of_component lp.LivePatch.component
      in
      try
        Helpers.call_api_functions ~__context (fun rpc session_id ->
            let open LivePatch in
            Client.Client.Repository.apply_livepatch ~rpc ~session_id ~host
              ~component:component_str ~base_build_id:lp.base_build_id
              ~base_version:lp.base_version ~base_release:lp.base_release
              ~to_version:lp.to_version ~to_release:lp.to_release
        ) ;
        Left (lp, lps)
      with
      | Api_errors.(Server_error (_, _)) as e ->
          let host' = Ref.string_of host in
          let msg = ExnHelper.string_of_exn e in
          error "applying %s livepatch (%s) on host ref='%s failed: %s"
            component_str (LivePatch.to_string lp) host' msg ;
          Right (lp, lps)
      | e ->
          let host' = Ref.string_of host in
          let msg = ExnHelper.string_of_exn e in
          error "applying %s livepatch (%s) on host ref='%s failed: %s"
            component_str (LivePatch.to_string lp) host' msg ;
          Right (lp, lps)
    )
    livepatches

let update_cache ~host ~failed_livepatches =
  Hashtbl.replace updates_in_cache host
    (`Assoc
      [
        ("updates", `List [])
      ; ("accumulative_updates", `List [])
      ; ( "livepatches"
        , `List
            (List.map
               (fun (lp, _) -> `String (LivePatch.to_string lp))
               failed_livepatches
            )
        )
      ]
      )

let maybe_set_restart_for_all_vms ~__context ~updates_of_hosts =
  let open Guidance in
  let add_restart_to_vms ~__context ~vms ~kind =
    let ( let* ) = Option.bind in
    let* op =
      match kind with
      | Mandatory ->
          Some Db.VM.add_pending_guidances
      | Recommended ->
          Some Db.VM.add_pending_guidances_recommended
      | Full ->
          Some Db.VM.add_pending_guidances_full
      | Livepatch ->
          None
    in
    Some
      (List.iter
         (fun self ->
           debug "add RestartVM for VM %s" (Ref.string_of self) ;
           op ~__context ~self ~value:`restart_vm
         )
         vms
      )
  in
  if updates_of_hosts <> [] then
    let vms =
      Db.VM.get_all ~__context
      |> List.filter (fun self ->
             Db.VM.get_power_state ~__context ~self <> `Halted
             && not (Db.VM.get_is_control_domain ~__context ~self)
         )
    in
    (* fold each guidance kind from all hosts *)
    updates_of_hosts
    |> List.concat_map (fun HostUpdates.{guidance; _} -> guidance)
    |> List.fold_left
         (fun acc (kind, l) ->
           if List.mem RestartVM l && not (List.mem kind acc) then
             kind :: acc
           else
             acc
         )
         []
    |> List.iter (fun kind ->
           (* set RestartVM for all VMs if it is presented from at least one host *)
           debug "add RestartVM for all VMs' pending %s guidance list"
             (kind_to_string kind) ;
           add_restart_to_vms ~__context ~vms ~kind |> ignore
       )

let apply_updates' ~__context ~host ~updates_info ~livepatches ~acc_rpm_updates
    =
  (* This function runs on coordinator host *)
  let open Guidance in
  let guidance' =
    reduce_guidance ~updates_info ~updates:acc_rpm_updates ~livepatches
  in
  let mandatory =
    match List.assoc_opt Mandatory guidance' with
    | Some tasks ->
        tasks
    | None ->
        warn "No mandatory guidance found. Ignore it." ;
        []
  in
  assert_host_evacuation_if_required ~__context ~host ~mandatory ;
  let guidance =
    (* EvacuateHost should be carried out before host.apply_updates *)
    guidance'
    |> List.map (fun (k, l) -> (k, List.filter (fun x -> x <> EvacuateHost) l))
  in
  (* Install RPM updates *)
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      Client.Client.Repository.apply ~rpc ~session_id ~host
  ) ;
  (* Always apply livepatches even if host will reboot *)
  let applied_livepatches, failed_livepatches =
    apply_livepatches' ~__context ~host ~livepatches
  in
  (* Update states in cache *)
  update_cache ~host ~failed_livepatches ;
  (* Set pending guidance lists *)
  let set_guidance ~kind ~coming =
    List.iter
      (fun g ->
        debug "setting %s pending guidance: %s" (kind_to_string kind)
          (to_string g)
      )
      coming ;
    let ops = get_ops_of_pending ~__context ~host ~kind in
    set_pending_guidances ~ops ~coming
  in
  guidance |> List.iter (fun (kind, coming) -> set_guidance ~kind ~coming) ;
  let get_livepatch_component (lp, _) = lp.LivePatch.component in
  let applied = List.map get_livepatch_component applied_livepatches in
  let failed = List.map get_livepatch_component failed_livepatches in
  update_livepatch_failure_guidance ~__context ~host ~applied ~failed ;
  List.map
    (fun (lp, _) -> [Api_errors.apply_livepatch_failed; LivePatch.to_string lp])
    failed_livepatches

let apply_updates ~__context ~host ~hash =
  (* This function runs on coordinator host *)
  assert_no_host_pending_mandatory_guidance ~__context ~host ;
  try
    let repository = get_single_enabled_update_repository ~__context in
    let repository_name = get_repository_name ~__context ~self:repository in
    if hash = "" || hash <> Db.Repository.get_hash ~__context ~self:repository
    then
      raise Api_errors.(Server_error (updateinfo_hash_mismatch, [])) ;
    with_pool_repositories (fun () ->
        let updates_info =
          parse_updateinfo ~__context ~self:repository ~check:true |> snd
        in
        let updates_of_hosts =
          if Helpers.is_pool_master ~__context ~host then (
            (* save available updates before applying on coordinator *)
            if Hashtbl.length updates_in_cache = 0 then
              set_available_updates ~__context |> ignore ;
            let hosts = Db.Host.get_all ~__context in
            consolidate_updates_of_hosts ~repository_name ~updates_info ~hosts
            |> fst
          ) else
            []
        in
        let host_updates =
          http_get_host_updates_in_json ~__context ~host ~installed:true
        in
        let rpm_updates =
          host_updates
          |> get_list_from_updates_of_host "updates"
          |> List.map Update.of_json
        in
        let livepatches =
          retrieve_livepatches_from_updateinfo ~updates_info
            ~updates:host_updates
        in
        let ret =
          match (rpm_updates, livepatches) with
          | [], [] ->
              let host' = Ref.string_of host in
              info "Host ref='%s' is already up to date." host' ;
              []
          | _ ->
              let _, acc_rpm_updates =
                merge_updates ~repository_name ~updates:host_updates
              in
              apply_updates' ~__context ~host ~updates_info ~livepatches
                ~acc_rpm_updates
        in
        maybe_set_restart_for_all_vms ~__context ~updates_of_hosts ;
        ret
    )
  with
  | Api_errors.(Server_error (code, _)) as e
    when code <> Api_errors.internal_error ->
      raise e
  | e ->
      let host' = Ref.string_of host in
      error "applying updates on host ref='%s' failed: %s" host'
        (ExnHelper.string_of_exn e) ;
      raise Api_errors.(Server_error (apply_updates_failed, [host']))

let reset_updates_in_cache () = Hashtbl.clear updates_in_cache
