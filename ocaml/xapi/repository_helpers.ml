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
open Updateinfo
open Rpm

let exposing_pool_repo_mutex = Mutex.create ()

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

let create_repository_record ~__context ~name_label ~name_description
    ~binary_url ~source_url ~update ~gpgkey_path =
  let ref = Ref.make () in
  let uuid = Uuid.(to_string (make ())) in
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
            List.exists (fun d -> Astring.String.is_suffix ~affix:("." ^ d) h) l
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
    Option.value
      ~default:(Option.get (Helpers.get_management_ip_addr ~__context))
      (Pool_role.get_address_of_coordinator ())
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

let eval_guidance_for_one_update ~updates_info ~update ~kind
    ~upd_ids_of_livepatches =
  let open Update in
  match update.update_id with
  | Some upd_id -> (
    match List.assoc_opt upd_id updates_info with
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
            update.name update.arch upd_id (string_of_bool r)
        in
        let apps = updateinfo.UpdateInfo.guidance_applicabilities in
        match (List.exists is_applicable apps, apps) with
        | true, _ | false, [] -> (
            debug "%s" (dbg_msg true) ;
            match kind with
            | Guidance.Absolute ->
                updateinfo.UpdateInfo.abs_guidance
            | Guidance.Recommended ->
                if UpdateIdSet.mem upd_id upd_ids_of_livepatches then
                  (* The update has an applicable livepatch.
                   * Using the livaptch guidance.
                   *)
                  updateinfo.UpdateInfo.livepatch_guidance
                else
                  updateinfo.UpdateInfo.rec_guidance
          )
        | _ ->
            debug "%s" (dbg_msg false) ;
            None
      )
    | None ->
        warn "Can't find update ID %s from updateinfo.xml for update %s.%s"
          upd_id update.name update.arch ;
        None
  )
  | None ->
      warn "Ignore evaluating against package %s.%s as its update ID is missing"
        update.name update.arch ;
      None

(* In case that the RPM in an update has been installed (including live patch file),
 * but the live patch has not been applied.
 * In other words, this RPM update will not appear in parameter [updates] of
 * function [eval_guidances], but the live patch in it is still applicable.
 *)
let append_livepatch_guidances ~updates_info ~upd_ids_of_livepatches guidances =
  UpdateIdSet.fold
    (fun upd_id acc ->
      match List.assoc_opt upd_id updates_info with
      | Some UpdateInfo.{livepatch_guidance= Some g; _} ->
          GuidanceSet.add g acc
      | _ ->
          acc
    )
    upd_ids_of_livepatches guidances

let eval_guidances ~updates_info ~updates ~kind ~upd_ids_of_livepatches =
  List.fold_left
    (fun acc u ->
      match
        eval_guidance_for_one_update ~updates_info ~update:u ~kind
          ~upd_ids_of_livepatches
      with
      | Some g ->
          GuidanceSet.add g acc
      | None ->
          acc
    )
    GuidanceSet.empty updates
  |> append_livepatch_guidances ~updates_info ~upd_ids_of_livepatches
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
      | Some upd_id ->
          (pkg, Some upd_id, repo)
      | None ->
          warn "Not found update ID for update %s" (Pkg.to_fullname pkg) ;
          (pkg, None, repo)
    )
    latest_updates

let prune_by_latest_updates latest_updates pkg upd_id =
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
        Ok (pkg, upd_id, repo)
  | None ->
      let msg =
        Printf.sprintf
          "Found an accumulative update but this package (name.arch) is not in \
           latest updates: %s"
          (Pkg.to_fullname pkg)
      in
      Error (Some msg)

let prune_by_installed_pkgs installed_pkgs pkg upd_id repo =
  let open Pkg in
  let name_arch = to_name_arch_string pkg in
  match List.assoc_opt name_arch installed_pkgs with
  | Some pkg' ->
      if
        Pkg.gt pkg.epoch pkg.version pkg.release pkg'.epoch pkg'.version
          pkg'.release
      then
        Ok (pkg, upd_id, repo)
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
    (fun (pkg, upd_id) ->
      let open Rresult.R.Infix in
      ( prune_by_latest_updates latest_updates pkg upd_id
      >>= fun (pkg', upd_id', repo') ->
        prune_by_installed_pkgs installed_pkgs pkg' upd_id' repo'
      )
      |> function
      | Ok (pkg', upd_id', repo') ->
          Some (pkg', Some upd_id', repo')
      | Error (Some msg) ->
          warn "%s" msg ; None
      | Error None ->
          None
    )
    accumulative_updates

(* Get metadata of all livepatches with same component and base_build_id from updateinfo *)
let get_livepatches_in_updateinfo ~updates_info ~component ~base_build_id =
  List.fold_left
    (fun acc (_, update_info) ->
      let available_livepatches =
        update_info.UpdateInfo.livepatches
        |> List.filter (fun lp ->
               let open LivePatch in
               lp.component = component && lp.base_build_id = base_build_id
           )
        |> List.map (fun lp -> (lp, update_info))
      in
      List.rev_append available_livepatches acc
    )
    [] updates_info

(* Get all applicable livepatches which are newer than 'since' *)
let get_accumulative_livepatches ~since ~updates_info =
  get_livepatches_in_updateinfo ~updates_info
    ~component:since.Livepatch.component
    ~base_build_id:since.Livepatch.base_build_id
  |> List.filter (fun (lp, _) ->
         let open LivePatch in
         match since with
         | Livepatch.{to_version= Some to_ver; to_release= Some to_rel; _} ->
             (* There is a running livepatch *)
             Pkg.gt None lp.to_version lp.to_release None to_ver to_rel
         | Livepatch.{to_version= None; to_release= None; _} ->
             (* No running livepatch *)
             true
         | _ ->
             (* Ignore unexpected error to get updating proceeded *)
             let lp_in_str =
               Yojson.Basic.pretty_to_string (Livepatch.to_json since)
             in
             warn "Ignore un-expected applied livepatch %s." lp_in_str ;
             false
     )

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
      let upd_id_in_json =
        match update_id with Some s -> `String s | None -> `Null
      in
      let l =
        [
          ("name", `String new_pkg.name)
        ; ("arch", `String new_pkg.arch)
        ; ("newEpochVerRel", to_epoch_ver_rel_json new_pkg)
        ; ("updateId", upd_id_in_json)
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

let merge_updates ~repository_name ~updates =
  let accumulative_updates =
    updates
    |> Yojson.Basic.Util.member "accumulative_updates"
    |> Yojson.Basic.Util.to_list
    |> List.map Update.of_json
  in
  let open Update in
  (* The update IDs and guidances come from accumulative updates *)
  List.fold_left
    (fun (acc_upd_ids', acc_updates') u ->
      match (u.update_id, u.repository = repository_name) with
      | Some id, true ->
          (UpdateIdSet.add id acc_upd_ids', u :: acc_updates')
      | _ ->
          (acc_upd_ids', acc_updates')
    )
    (UpdateIdSet.empty, []) accumulative_updates

let get_list_from_updates_of_host key updates_of_host =
  match Yojson.Basic.Util.member key updates_of_host with
  | `Null ->
      []
  | l ->
      Yojson.Basic.Util.to_list l

let merge_livepatches ~updates_info ~updates =
  let open LivePatch in
  get_list_from_updates_of_host "applied_livepatches" updates
  |> List.map Livepatch.of_json
  |> List.fold_left
       (fun (acc_upd_ids, acc_lps) applied_lp ->
         let acc_livepatches =
           get_accumulative_livepatches ~since:applied_lp ~updates_info
         in
         (* Find the livepatch with maximum 'to-version' and 'to-release' *)
         let latest_livepatch =
           acc_livepatches
           |> List.map (fun (lp, _) -> (lp.to_version, lp.to_release))
           |> get_latest_version_release
           |> fun latest ->
           match latest with
           | Some (latest_ver, latest_rel) ->
               List.find_map
                 (fun (lp, _) ->
                   if lp.to_version = latest_ver && lp.to_release = latest_rel
                   then
                     Some lp
                   else
                     None
                 )
                 acc_livepatches
           | None ->
               None
         in
         match latest_livepatch with
         | Some latest_lp ->
             let upd_ids =
               List.fold_left
                 (fun acc (_, u) -> UpdateIdSet.add u.UpdateInfo.id acc)
                 UpdateIdSet.empty acc_livepatches
             in
             (UpdateIdSet.union upd_ids acc_upd_ids, latest_lp :: acc_lps)
         | None ->
             (acc_upd_ids, acc_lps)
       )
       (UpdateIdSet.empty, [])

let consolidate_updates_of_host ~repository_name ~updates_info host
    updates_of_host =
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
  let ids_of_updates, updates =
    merge_updates ~repository_name ~updates:updates_of_host
  in
  (* Find out applicable latest livepatches and the info of accumulative updates
   * introduced them. These accumulative updates will not be evaluated for guidance.
   * They are only to be returned in the update list.
   *)
  let upd_ids_of_livepatches, livepatches =
    merge_livepatches ~updates_info ~updates:updates_of_host
  in
  let rec_guidances =
    eval_guidances ~updates_info ~updates ~kind:Recommended
      ~upd_ids_of_livepatches
  in
  let abs_guidances_in_json =
    let abs_guidances =
      List.filter
        (fun g -> not (List.mem g rec_guidances))
        (eval_guidances ~updates_info ~updates ~kind:Absolute
           ~upd_ids_of_livepatches:UpdateIdSet.empty
        )
    in
    List.map (fun g -> `String (Guidance.to_string g)) abs_guidances
  in
  let upd_ids_of_livepatches', livepatches' =
    if List.mem Guidance.RebootHost rec_guidances then
      (* Any livepatches should not be applied if packages updates require RebootHost *)
      (UpdateIdSet.empty, [])
    else
      (upd_ids_of_livepatches, livepatches)
  in
  let rec_guidances_in_json =
    List.map (fun g -> `String (Guidance.to_string g)) rec_guidances
  in
  let upd_ids = UpdateIdSet.union ids_of_updates upd_ids_of_livepatches' in
  let json_of_host =
    `Assoc
      [
        ("ref", `String host)
      ; ("recommended-guidance", `List rec_guidances_in_json)
      ; ("absolute-guidance", `List abs_guidances_in_json)
      ; ("RPMS", `List (List.map (fun r -> `String r) rpms))
      ; ( "updates"
        , `List
            (List.map
               (fun upd_id -> `String upd_id)
               (UpdateIdSet.elements upd_ids)
            )
        )
      ; ("livepatches", `List (List.map LivePatch.to_json livepatches'))
      ]
  in
  (json_of_host, upd_ids)

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
