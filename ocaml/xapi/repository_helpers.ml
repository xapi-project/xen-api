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
open Rpm
open Updateinfo
module LivePatchSet = Set.Make (LivePatch)
module RpmFullNameSet = Set.Make (String)

let exposing_pool_repo_mutex = Mutex.create ()

module Pkgs = (val Pkg_mgr.get_pkg_mgr)

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

  let precedences =
    [
      (RebootHost, of_list [RestartToolstack; EvacuateHost; RestartDeviceModel])
    ; (EvacuateHost, of_list [RestartDeviceModel])
    ; (RestartVM, of_list [RestartDeviceModel])
    ]

  (** reduce the set [gs] by sorting partially with the defined [precedences] *)
  let resort gs =
    List.fold_left
      (fun acc (higher, lowers) ->
        if mem higher acc then
          diff acc lowers
        else
          acc
      )
      gs precedences

  (** remove the guidances which are in s1 from s2. *)
  let reduce s1 s2 = diff (resort (union s1 s2)) s1

  (** for each set in the list, remove the guidances which are in previous sets from it. *)
  let reduce_cascaded_list l =
    let _, reduced_list_in_reverse =
      List.fold_left
        (fun (acc_set, acc_list) (k, s) ->
          let reduced = reduce acc_set s in
          (union s acc_set, (k, reduced) :: acc_list)
        )
        (empty, []) l
    in
    List.rev reduced_list_in_reverse
end

let create_repository_record ~__context ~name_label ~name_description
    ~binary_url ~source_url ~update ~gpgkey_path ~origin ~certificate =
  let ref = Ref.make () in
  let uuid = Uuidx.(to_string (make ())) in
  Db.Repository.create ~__context ~ref ~uuid ~name_label ~name_description
    ~binary_url ~source_url ~update ~hash:"" ~up_to_date:false ~gpgkey_path
    ~origin ~certificate ;
  ref

module DomainNameIncludeIP = struct
  include Domain_name

  let host_exn t =
    try host_exn t
    with Invalid_argument _ -> (
      match Ipaddr.of_string (Domain_name.to_string t) with
      | Ok ip ->
          Ipaddr.to_domain_name ip
      | Error _ ->
          invalid_arg "invalid host name"
    )
end

let assert_url_is_valid ~url =
  try
    let uri = Uri.of_string url in
    match Uri.(scheme uri, host uri) with
    | Some ("http" | "https"), Some h -> (
        let subdomain =
          DomainNameIncludeIP.(host_exn (of_string h |> Result.get_ok))
        in
        let invalids, valids =
          List.partition_map
            (fun d ->
              match Domain_name.of_string d with
              | Ok dom ->
                  Right dom
              | Error _ ->
                  Left d
            )
            !Xapi_globs.repository_domain_name_allowlist
        in
        let hostname_allowed =
          List.exists (fun d ->
              DomainNameIncludeIP.(is_subdomain ~subdomain ~domain:(host_exn d))
          )
        in
        match (valids, invalids) with
        | [], [] ->
            ()
        | valids, [] when not (hostname_allowed valids) ->
            let msg = "host is not in allowlist" in
            raise Api_errors.(Server_error (internal_error, [msg]))
        | _, [] ->
            ()
        | _ ->
            error "Invalid domains: %s in repository-domain-name-allowlist."
              (String.concat "," invalids) ;
            raise
              Api_errors.(
                Server_error (invalid_repository_domain_allowlist, invalids)
              )
      )
    | _, None ->
        raise Api_errors.(Server_error (internal_error, ["invalid host in url"]))
    | _ ->
        raise
          Api_errors.(Server_error (internal_error, ["invalid scheme in url"]))
  with
  | Api_errors.Server_error (err, _) as e
    when err = Api_errors.invalid_repository_domain_allowlist ->
      raise e
  | e ->
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
            Gzip.Default.decompress_passive gz_fd_in @@ fun fd_in ->
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
    let Pkg_mgr.{cmd; params} = Pkgs.clean_cache ~repo_name:name in
    ignore (Helpers.call_script cmd params)
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
  let Pkg_mgr.{cmd; params} = Pkgs.get_repo_config ~repo_name in
  Helpers.call_script cmd params
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
  let prefix =
    match Db.Repository.get_origin ~__context ~self with
    | `remote ->
        !Xapi_globs.remote_repository_prefix
    | `bundle ->
        !Xapi_globs.bundle_repository_prefix
    | `remote_pool ->
        !Xapi_globs.remote_pool_repository_prefix
  in
  prefix ^ "-" ^ get_repository_name ~__context ~self

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
            let Pkg_mgr.{cmd; params} =
              [
                "--save"
              ; Printf.sprintf "--setopt=%s.sslverify=false" repo_name
                (* certificate verification is handled by the stunnel proxy *)
              ; Printf.sprintf "--setopt=%s.ptoken=true" repo_name
                (* makes yum include the pool secret as a cookie in all requests
                   (only to access the repo mirror in the coordinator!) *)
              ]
              |> fun config -> Pkgs.config_repo ~repo_name ~config
            in
            ignore (Helpers.call_script cmd params) ;
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

let is_obsoleted pkg_name repositories =
  let Pkg_mgr.{cmd; params} = Pkgs.is_obsoleted ~pkg_name ~repositories in
  match
    Helpers.call_script cmd params |> Astring.String.cuts ~sep:"\n" ~empty:false
  with
  | [] ->
      false
  | _ ->
      debug "Available package %s has been obsoleted." pkg_name ;
      true
  | exception _ ->
      warn
        "Can't determine if available package %s is obsoleted or not. Assuming \
         it is not."
        pkg_name ;
      false

let get_pkgs_from_yum_updateinfo_list sub_command repositories =
  let Pkg_mgr.{cmd; params} =
    Pkgs.get_pkgs_from_updateinfo ~sub_command ~repositories
  in
  Helpers.call_script cmd params
  |> assert_yum_error
  |> Astring.String.cuts ~sep:"\n"
  |> List.map (fun x ->
         debug "yum updateinfo list %s: %s" sub_command x ;
         x
     )
  |> List.fold_left parse_updateinfo_list []

let get_updates_from_updateinfo ~__context repositories =
  (* Use 'updates' to decrease the number of packages to apply 'is_obsoleted' *)
  let updates = get_pkgs_from_yum_updateinfo_list "updates" repositories in
  (* 'new_updates' are a list of RPM packages to be installed, rather than updated *)
  let new_updates =
    get_pkgs_from_yum_updateinfo_list "available" repositories
    |> List.filter (fun x -> not (List.mem x updates))
    |> List.filter (fun (pkg, _) -> not (is_obsoleted pkg.Pkg.name repositories))
  in
  new_updates @ updates

let is_applicable ~update (applicability : Applicability.t) =
  let open Update in
  match
    (update.name = applicability.name, update.arch = applicability.arch)
  with
  | true, true -> (
    match (update.old_epoch, update.old_version, update.old_release) with
    | Some old_epoch, Some old_version, Some old_release ->
        Applicability.eval ~epoch:old_epoch ~version:old_version
          ~release:old_release ~applicability
    | _ ->
        warn "No installed epoch, version or release for package %s.%s"
          update.name update.arch ;
        false
  )
  | _ ->
      false

let eval_guidance_for_one_update ~updates_info ~update ~kind
    ~upd_ids_of_livepatches =
  let open Update in
  match update.update_id with
  | Some upd_id -> (
    match List.assoc_opt upd_id updates_info with
    | Some updateinfo -> (
        let pkg_str =
          Printf.sprintf "package %s.%s in update %s" update.name update.arch
            upd_id
        in
        let dbg_msg r =
          Printf.sprintf "Evaluating applicability for %s returned '%s'" pkg_str
            (string_of_bool r)
        in
        let str_of_guidances guidances =
          String.concat ";" (List.map Guidance.to_string guidances)
        in
        let apps = updateinfo.UpdateInfo.guidance_applicabilities in
        match (List.exists (is_applicable ~update) apps, apps) with
        | true, _ | false, [] -> (
            debug "%s" (dbg_msg true) ;
            let open Guidance in
            let open UpdateInfo in
            match kind with
            | Livepatch ->
                (* Livepatch should not be evaluated directly *)
                []
            | Full | Mandatory ->
                let gs = get_guidances_of_kind ~kind updateinfo in
                debug "%s contributes to %s guidances [%s]" pkg_str
                  (kind_to_string kind) (str_of_guidances gs) ;
                gs
            | Recommended ->
                let gs =
                  match UpdateIdSet.mem upd_id upd_ids_of_livepatches with
                  | true ->
                      debug "use livepatch guidance of %s" pkg_str ;
                      get_guidances_of_kind ~kind:Livepatch updateinfo
                  | false ->
                      get_guidances_of_kind ~kind:Recommended updateinfo
                in
                debug "%s contributes to recommended guidances [%s]" pkg_str
                  (str_of_guidances gs) ;
                gs
          )
        | _ ->
            debug "%s" (dbg_msg false) ;
            []
      )
    | None ->
        warn "Can't find update ID %s from updateinfo.xml for update %s.%s"
          upd_id update.name update.arch ;
        []
  )
  | None ->
      warn "Ignore evaluating against package %s.%s as its update ID is missing"
        update.name update.arch ;
      []

(* In case that the RPM in an update has been installed (including livepatch file),
 * but the livepatch has not been applied.
 * In other words, this RPM update will not appear in parameter [updates] of
 * function [eval_guidances], but the livepatch in it is still applicable.
 *)
let append_livepatch_guidance ~updates_info ~upd_ids_of_livepatches
    guidance_tasks =
  let ( let* ) = Option.bind in
  UpdateIdSet.fold
    (fun upd_id acc ->
      let get_livepatch_guidance () =
        let* updateinfo = List.assoc_opt upd_id updates_info in
        let* l =
          List.assoc_opt Guidance.Livepatch updateinfo.UpdateInfo.guidance
        in
        Some (GuidanceSet.of_list l)
      in
      match get_livepatch_guidance () with
      | Some s ->
          GuidanceSet.union s acc
      | None ->
          acc
    )
    upd_ids_of_livepatches guidance_tasks

let eval_guidances ~updates_info ~updates ~kind ~livepatches =
  let extract_upd_ids lps =
    List.fold_left
      (fun acc (_, lps) ->
        List.fold_left
          (fun acc' (_, upd_info) -> UpdateIdSet.add upd_info.UpdateInfo.id acc')
          acc lps
      )
      UpdateIdSet.empty lps
  in
  let upd_ids_of_livepatches = extract_upd_ids livepatches in
  List.fold_left
    (fun acc u ->
      eval_guidance_for_one_update ~updates_info ~update:u ~kind
        ~upd_ids_of_livepatches
      |> GuidanceSet.of_list
      |> GuidanceSet.union acc
    )
    GuidanceSet.empty updates
  |> (fun l ->
       match kind with
       | Recommended ->
           append_livepatch_guidance ~updates_info ~upd_ids_of_livepatches l
       | _ ->
           l
     )
  |> GuidanceSet.resort

let parse_line_of_repoquery acc line =
  match Astring.String.cuts ~sep:Pkg_mgr.repoquery_sep line with
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
  let Pkg_mgr.{cmd; params} = Pkgs.repoquery_installed () in
  Helpers.call_script cmd params
  |> Astring.String.cuts ~sep:"\n"
  |> List.map (fun x ->
         debug "repoquery installed: %s" x ;
         x
     )
  |> List.fold_left parse_line_of_repoquery []
  |> List.map (fun (pkg, _) -> (Pkg.to_name_arch_string pkg, pkg))

let get_pkgs_from_repoquery cmd params =
  Helpers.call_script cmd params
  |> Astring.String.cuts ~sep:"\n"
  |> List.map (fun x ->
         debug "repoquery available: %s" x ;
         x
     )
  |> List.fold_left parse_line_of_repoquery []

let get_updates_from_repoquery repositories =
  List.iter (fun r -> clean_yum_cache r) repositories ;
  (* Use 'updates' to decrease the number of packages to apply 'is_obsoleted' *)
  let Pkg_mgr.{cmd; params} = Pkgs.repoquery_updates ~repositories in
  let updates = get_pkgs_from_repoquery cmd params in
  (* 'new_updates' are a list of RPM packages to be installed, rather than updated *)
  let Pkg_mgr.{cmd; params} = Pkgs.repoquery_available ~repositories in
  let new_updates =
    get_pkgs_from_repoquery cmd params
    |> List.filter (fun x -> not (List.mem x updates))
    |> List.filter (fun (pkg, _) -> not (is_obsoleted pkg.Pkg.name repositories))
  in
  new_updates @ updates

(* Parser module for the output of command yum upgrade *)
module YumUpgradeOutput = struct
  open Angstrom

  let consume = Consume.Prefix

  let is_eol = function '\n' -> true | _ -> false

  let is_white = Astring.Char.Ascii.is_white

  let rec line ~section ~section_acc ~acc =
    let rec line_after_txn_summary sections =
      at_end_of_input >>= function
      | false -> (
          take_till is_eol <* end_of_line >>= function
          | l
            when String.starts_with
                   ~prefix:" yum load-transaction /tmp/yum_save_tx." l
                 && String.ends_with ~suffix:".yumtx" l ->
              return (sections, Some l)
          | _ ->
              line_after_txn_summary sections
        )
      | true ->
          return (sections, None)
    in
    (* The following sections/separators come from
     * https://github.com/rpm-software-management/yum/blob/master/output.py#L1569-L1576
     *)
    at_end_of_input >>= function
    | false -> (
        take_till is_eol <* end_of_line >>= function
        | ( "Installing:"
          | "Updating:"
          | "Upgrading:"
          | "Removing:"
          | "Reinstalling:"
          | "Downgrading:"
          | "Installing for dependencies:"
          | "Updating for dependencies:"
          | "Removing for dependencies:" ) as section' -> (
          (* a new section *)
          match section with
          | Some s ->
              (* wrap up the old seciton and save it to the final accumulation *)
              line ~section:(Some section') ~section_acc:[]
                ~acc:((s, section_acc) :: acc)
          | None ->
              (* this new section is the first one *)
              line ~section:(Some section') ~section_acc:[] ~acc
        )
        | "Transaction Summary" -> (
          match section with
          | Some s ->
              (* save the last section to the final accumulation *)
              line_after_txn_summary ((s, section_acc) :: acc)
          | None ->
              line_after_txn_summary acc
        )
        | l -> (
          match
            ( section
            , String.starts_with ~prefix:"     replacing " l
            , String.starts_with ~prefix:"Error: " l
            )
          with
          | Some s, true, false ->
              (* in a section, but starting with 'replacing', ignoring the line.
               * https://github.com/rpm-software-management/yum/blob/master/output.py#L1622
               *)
              line ~section:(Some s) ~section_acc ~acc
          | Some s, false, false ->
              (* in a section, append to the section's list *)
              line ~section:(Some s) ~section_acc:(l :: section_acc) ~acc
          | None, false, true ->
              (* error reported from yum upgrade *)
              fail l
          | None, false, false ->
              (* not in any section, ignoring the line *)
              line ~section:None ~section_acc:[] ~acc
          | _ ->
              fail
                (Printf.sprintf
                   "Unexpected output from yum upgrade (dry run): %s" l
                )
        )
      )
    | true ->
        return ([], None)

  let word =
    skip_while is_white >>= fun () ->
    take_while1 (fun x -> not (is_white x)) >>= fun s ->
    skip_while is_white >>= fun () -> return s

  let package =
    word >>= fun name ->
    word >>= fun arch ->
    word >>= fun epoch_version_release ->
    word >>= fun repo ->
    word >>= fun _ ->
    word >>= fun _ ->
    match Pkg.parse_epoch_version_release epoch_version_release with
    | epoch, version, release ->
        return (Pkg.{name; epoch; version; release; arch}, repo)
    | exception e ->
        let msg =
          Printf.sprintf "parse_epoch_version_release: %s"
            (ExnHelper.string_of_exn e)
        in
        fail msg

  let packages = many_till package end_of_input

  let parse_yum_txn_file txn_line_opt =
    let txn_line =
      skip_while is_white >>= fun () ->
      string "yum load-transaction " >>= fun _ ->
      take_till is_white >>= fun s -> return s
    in
    match txn_line_opt with
    | Some line -> (
      match parse_string ~consume txn_line line with
      | Ok txn_file_path ->
          Some txn_file_path
      | Error msg ->
          warn "Can't parse transaction file from line %s: %s." line msg ;
          None
    )
    | None ->
        None

  let parse_output_of_dry_run output =
    Astring.String.cuts ~sep:"\n" output
    |> List.iter (fun x -> debug "yum upgrade (dry run): %s" x) ;

    match
      (parse_string ~consume (line ~section:None ~section_acc:[] ~acc:[]))
        output
    with
    | Ok ((_ :: _ as sections), txn_line_opt) -> (
        let txn_file = parse_yum_txn_file txn_line_opt in
        let get_pkgs lines =
          let s = String.concat "\n" (List.rev_append lines []) in
          parse_string ~consume packages s
        in
        sections
        |> List.filter (fun (section, _) ->
               match section with
               | "Installing:"
               | "Updating:"
               | "Upgrading:"
               | "Installing for dependencies:"
               | "Updating for dependencies:" ->
                   true
               | _ ->
                   false
           )
        |> List.fold_left
             (fun acc (section, lines) ->
               match (acc, get_pkgs lines) with
               | Ok acc', Ok pkgs ->
                   Ok (List.rev_append pkgs acc')
               | Error msg, _ ->
                   Error msg
               | _, Error msg ->
                   error "Failed to parse packages for '%s': %s" section msg ;
                   Error msg
             )
             (Ok [])
        |> function
        | Ok pkgs ->
            let pkgs' =
              List.fast_sort
                (fun (x, _) (y, _) -> String.compare x.Pkg.name y.Pkg.name)
                pkgs
            in
            Ok (pkgs', txn_file)
        | Error msg ->
            Error msg
      )
    | Ok ([], None) ->
        debug "No updates from yum upgrade (dry run)" ;
        Ok ([], None)
    | Ok ([], Some _) ->
        Error "Unexpected output from yum upgrade (dry run)"
    | Error msg ->
        Error
          (Printf.sprintf "Can't parse output from yum upgrade (dry run): %s"
             msg
          )
end

let get_updates_from_yum_upgrade_dry_run repositories =
  let Pkg_mgr.{cmd; params} =
    Pkgs.get_updates_from_upgrade_dry_run ~repositories
  in
  match Forkhelpers.execute_command_get_output cmd params with
  | _, _ ->
      Some []
  | exception Forkhelpers.Spawn_internal_error (stderr, stdout, Unix.WEXITED 1)
    -> (
      (*Yum put the details to stderr while dnf to stdout*)
      (match Pkgs.manager with Yum -> stderr | Dnf -> stdout)
      |> YumUpgradeOutput.parse_output_of_dry_run
      |> function
      | Ok (pkgs, Some txn_file) ->
          Unixext.unlink_safe txn_file ;
          Some pkgs
      | Ok (pkgs, None) ->
          Some pkgs
      | Error msg ->
          error "%s" msg ; None
    )
  | exception e ->
      error "%s" (ExnHelper.string_of_exn e) ;
      None

let get_latest_updates_from_redundancy ~fail_on_error ~pkgs ~fallback_pkgs =
  let err = "Failed to parse output of 'yum upgrade (dry run)' correctly" in
  let get_latest_updates_from_redundancy' ~fail_on_error ~pkgs ~fallback_pkgs =
    let to_set l =
      List.map (fun (pkg, _) -> Rpm.Pkg.to_fullname pkg) l
      |> RpmFullNameSet.of_list
    in
    let is_subset l' l'' = RpmFullNameSet.subset (to_set l') (to_set l'') in
    match (fail_on_error, is_subset pkgs fallback_pkgs) with
    | _, true ->
        debug "Use 'yum upgrade (dry run)'" ;
        pkgs
    | true, false ->
        raise Api_errors.(Server_error (internal_error, [err]))
    | false, false ->
        (* falling back *)
        warn "%s" err ; fallback_pkgs
  in
  match (fail_on_error, pkgs) with
  | true, None ->
      raise Api_errors.(Server_error (internal_error, [err]))
  | false, None ->
      (* falling back *)
      warn "%s" err ; fallback_pkgs
  | _, Some pkgs' ->
      debug "Checking both 'yum upgrade' and 'repoquery' ..." ;
      get_latest_updates_from_redundancy' ~fail_on_error ~pkgs:pkgs'
        ~fallback_pkgs

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
          "Found an accumulative update but this package (name.arch) has not \
           been installed: %s"
          (to_fullname pkg)
      in
      debug "%s" msg ;
      Ok (pkg, upd_id, repo)

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

let retrieve_livepatches_from_updateinfo ~updates_info ~updates =
  let open LivePatch in
  get_list_from_updates_of_host "applied_livepatches" updates
  |> List.map Livepatch.of_json
  |> List.filter_map (fun applied_lp ->
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
             Some (latest_lp, acc_livepatches)
         | None ->
             None
     )

let merge_livepatches ~livepatches =
  let get_accumulative_upd_ids acc_lps =
    List.fold_left
      (fun acc (_, u) -> UpdateIdSet.add u.UpdateInfo.id acc)
      UpdateIdSet.empty acc_lps
  in
  livepatches
  |> List.fold_left
       (fun (acc_upd_ids, acc_latest_lps) (latest_lp, acc_lps) ->
         let upd_ids = get_accumulative_upd_ids acc_lps in
         (UpdateIdSet.union upd_ids acc_upd_ids, latest_lp :: acc_latest_lps)
       )
       (UpdateIdSet.empty, [])

let reduce_guidance ~updates_info ~updates ~livepatches =
  let open Guidance in
  (* The order does matter with the following reducing *)
  [Mandatory; Recommended; Full]
  |> List.map (fun kind ->
         (kind, eval_guidances ~updates_info ~updates ~kind ~livepatches)
     )
  |> GuidanceSet.reduce_cascaded_list
  |> List.map (fun (kind, s) -> (kind, GuidanceSet.elements s))

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
        Pkg.
          {
            name= u.Update.name
          ; arch= u.Update.arch
          ; epoch= u.Update.new_epoch
          ; version= u.Update.new_version
          ; release= u.Update.new_release
          }
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
  let livepatches =
    retrieve_livepatches_from_updateinfo ~updates_info ~updates:updates_of_host
  in
  let guidance = reduce_guidance ~updates_info ~updates ~livepatches in
  let upd_ids_of_livepatches, lps = merge_livepatches ~livepatches in
  let upd_ids = UpdateIdSet.union ids_of_updates upd_ids_of_livepatches in
  let host_updates =
    HostUpdates.
      {
        host
      ; guidance
      ; rpms
      ; update_ids= UpdateIdSet.elements upd_ids
      ; livepatches= lps
      }
  in
  (host_updates, upd_ids)

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
      ([v], []) l
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

let prune_updateinfo_for_livepatches latest_lps updateinfo =
  let livepatches =
    let open LivePatch in
    (* Keep a livepatch if it is rolled up by one of the latest livepatches.
     * The latest livepatches are the ones to be applied actually.
     *)
    updateinfo.UpdateInfo.livepatches
    |> List.filter (fun lp ->
           let is_rolled_up_by latest =
             latest.component = lp.component
             && latest.base_build_id = lp.base_build_id
           in
           LivePatchSet.exists is_rolled_up_by latest_lps
       )
  in
  {updateinfo with livepatches}

let do_with_host_pending_guidances ~op guidances =
  List.iter
    (fun g ->
      match Guidance.to_pending_guidance g with
      | ( Some `restart_toolstack
        | Some `reboot_host
        | Some `reboot_host_on_xen_livepatch_failure
        | Some `reboot_host_on_kernel_livepatch_failure
        | Some `reboot_host_on_livepatch_failure ) as g' ->
          Option.iter op g'
      | _ ->
          ()
    )
    guidances

let do_with_vm_pending_guidances ~op ~vm guidances =
  List.iter
    (fun g ->
      match Guidance.to_pending_guidance g with
      | Some `restart_device_model ->
          op vm `restart_device_model
      | Some `restart_vm ->
          op vm `restart_vm
      | _ ->
          ()
    )
    guidances

let merge_with_pending_guidances ~pending ~coming =
  let open GuidanceSet in
  let pending = of_list pending in
  let unioned = union pending (of_list coming) |> remove EvacuateHost in
  (diff unioned pending |> elements, diff pending unioned |> elements)

let is_livepatch_failure = function
  | Guidance.RebootHostOnLivePatchFailure
  | Guidance.RebootHostOnXenLivePatchFailure
  | Guidance.RebootHostOnKernelLivePatchFailure ->
      true
  | _ ->
      false

type pending_ops = {
    host_get: unit -> Guidance.t list
  ; host_add:
         [ `reboot_host
         | `reboot_host_on_livepatch_failure
         | `restart_toolstack
         | `reboot_host_on_xen_livepatch_failure
         | `reboot_host_on_kernel_livepatch_failure ]
      -> unit
  ; host_remove:
         [ `reboot_host
         | `reboot_host_on_livepatch_failure
         | `restart_toolstack
         | `reboot_host_on_xen_livepatch_failure
         | `reboot_host_on_kernel_livepatch_failure ]
      -> unit
  ; vms_get: unit -> (string * Guidance.t list) list
  ; vm_add: string -> [`restart_device_model | `restart_vm] -> unit
  ; vm_remove: string -> [`restart_device_model | `restart_vm] -> unit
}

let get_ops_of_pending ~__context ~host ~kind =
  let get_pending_guidances_of_host ~db_get =
    db_get ~__context ~self:host |> List.map Guidance.of_pending_guidance
  in
  let get_pending_guidances_of_vms ~db_get =
    Db.Host.get_resident_VMs ~__context ~self:host
    |> List.filter (fun self ->
           not (Db.VM.get_is_control_domain ~__context ~self)
       )
    |> List.map (fun vm_ref ->
           let pending_guidances =
             db_get ~__context ~self:vm_ref
             |> List.map Guidance.of_pending_guidance
           in
           (Ref.string_of vm_ref, pending_guidances)
       )
  in
  let is_vm_applicable ~self = function
    | `restart_device_model ->
        Helpers.has_qemu_currently ~__context ~self
    | `restart_vm ->
        (* RestartVM will be set in host.apply_updates when updating the coordinator.
         * To avoid confusions, not setting it here.
         *)
        false
  in
  match kind with
  | Guidance.Mandatory ->
      let host_get () =
        get_pending_guidances_of_host ~db_get:Db.Host.get_pending_guidances
      in
      let host_add value =
        Db.Host.add_pending_guidances ~__context ~self:host ~value
      in
      let host_remove value =
        Db.Host.remove_pending_guidances ~__context ~self:host ~value
      in
      let vms_get () =
        get_pending_guidances_of_vms ~db_get:Db.VM.get_pending_guidances
      in
      let vm_add vm value =
        let self = Ref.of_string vm in
        if is_vm_applicable ~self value then
          Db.VM.add_pending_guidances ~__context ~self:(Ref.of_string vm) ~value
      in
      let vm_remove vm value =
        Db.VM.remove_pending_guidances ~__context ~self:(Ref.of_string vm)
          ~value
      in
      {host_get; host_add; host_remove; vms_get; vm_add; vm_remove}
  | Guidance.Recommended ->
      let host_get () =
        get_pending_guidances_of_host
          ~db_get:Db.Host.get_pending_guidances_recommended
      in
      let host_add value =
        Db.Host.add_pending_guidances_recommended ~__context ~self:host ~value
      in
      let host_remove value =
        Db.Host.remove_pending_guidances_recommended ~__context ~self:host
          ~value
      in
      let vms_get () =
        get_pending_guidances_of_vms
          ~db_get:Db.VM.get_pending_guidances_recommended
      in
      let vm_add vm value =
        let self = Ref.of_string vm in
        if is_vm_applicable ~self value then
          Db.VM.add_pending_guidances_recommended ~__context ~self ~value
      in
      let vm_remove vm value =
        Db.VM.remove_pending_guidances_recommended ~__context
          ~self:(Ref.of_string vm) ~value
      in
      {host_get; host_add; host_remove; vms_get; vm_add; vm_remove}
  | Guidance.Full ->
      let host_get () =
        get_pending_guidances_of_host ~db_get:Db.Host.get_pending_guidances_full
      in
      let host_add value =
        Db.Host.add_pending_guidances_full ~__context ~self:host ~value
      in
      let host_remove value =
        Db.Host.remove_pending_guidances_full ~__context ~self:host ~value
      in
      let vms_get () =
        get_pending_guidances_of_vms ~db_get:Db.VM.get_pending_guidances_full
      in
      let vm_add vm value =
        let self = Ref.of_string vm in
        if is_vm_applicable ~self value then
          Db.VM.add_pending_guidances_full ~__context ~self ~value
      in
      let vm_remove vm value =
        Db.VM.remove_pending_guidances_full ~__context ~self:(Ref.of_string vm)
          ~value
      in
      {host_get; host_add; host_remove; vms_get; vm_add; vm_remove}
  | Guidance.Livepatch ->
      raise
        Api_errors.(
          Server_error
            (internal_error, ["No pending operations for Livepatch guidance"])
        )

let set_pending_guidances ~ops ~coming =
  let pending_of_host =
    ops.host_get () |> List.filter (fun x -> not (is_livepatch_failure x))
  in
  let to_be_added, to_be_removed =
    merge_with_pending_guidances ~pending:pending_of_host ~coming
  in
  do_with_host_pending_guidances ~op:ops.host_remove to_be_removed ;
  do_with_host_pending_guidances ~op:ops.host_add to_be_added ;

  ops.vms_get ()
  |> List.map (fun (vm_ref_str, pending_of_vm) ->
         let pending = List.append pending_of_host pending_of_vm in
         (vm_ref_str, merge_with_pending_guidances ~pending ~coming)
     )
  |> List.iter (fun (vm_ref_str, (to_be_added, to_be_removed)) ->
         do_with_vm_pending_guidances ~op:ops.vm_remove ~vm:vm_ref_str
           to_be_removed ;
         do_with_vm_pending_guidances ~op:ops.vm_add ~vm:vm_ref_str to_be_added
     )

let failure_of_livepatch_component = function
  | Livepatch.Xen ->
      Guidance.RebootHostOnXenLivePatchFailure
  | Livepatch.Kernel ->
      Guidance.RebootHostOnKernelLivePatchFailure

let component_of_livepatch_failure = function
  | Guidance.RebootHostOnXenLivePatchFailure ->
      Some Livepatch.Xen
  | Guidance.RebootHostOnKernelLivePatchFailure ->
      Some Livepatch.Kernel
  | _ ->
      None

let merge_livepatch_failures ~previous_failures ~applied ~failed =
  let current_failures = List.map failure_of_livepatch_component failed in
  (* Determine how to deal with previous livepatch failures *)
  List.fold_left
    (fun acc_fails prev_fail ->
      match component_of_livepatch_failure prev_fail with
      | Some prev_failed_component -> (
          let comp_str = Livepatch.string_of_component prev_failed_component in
          match
            ( List.mem prev_fail current_failures
            , List.mem prev_failed_component applied
            )
          with
          | true, false ->
              debug "%s Livepatch failed again." comp_str ;
              acc_fails
          | false, true ->
              (* applied in this update. No changes on current failures *)
              debug
                "%s Livepatch (failed in previous updates) has been applied."
                comp_str ;
              acc_fails
          | true, true ->
              (* Impossible case: a component is in both failed and applied *)
              warn "%s Livepatch shouldn't be in both applied and failed lists."
                comp_str ;
              acc_fails
          | false, false ->
              (* Didn't try in this update. Keep previous failure in current list *)
              debug "%s Livepatch failed in previous updates." comp_str ;
              prev_fail :: acc_fails
        )
      | None ->
          warn "Unknown livepatch failure." ;
          acc_fails
    )
    current_failures previous_failures
  |> fun failures ->
  let open GuidanceSet in
  let to_be_removed =
    diff (of_list previous_failures) (of_list failures) |> elements
  in
  let to_be_added =
    diff (of_list failures) (of_list previous_failures) |> elements
  in
  (to_be_removed, to_be_added)

let update_livepatch_failure_guidance ~__context ~host ~applied ~failed =
  let previous_failures =
    Db.Host.get_pending_guidances_recommended ~__context ~self:host
    |> List.map Guidance.of_pending_guidance
    |> List.filter is_livepatch_failure
  in
  let to_be_removed, to_be_added =
    merge_livepatch_failures ~previous_failures ~applied ~failed
  in
  (* The livepatch failure guidance is in host pending recommended list *)
  let host_remove value =
    Db.Host.remove_pending_guidances_recommended ~__context ~self:host ~value
  in
  let host_add value =
    Db.Host.add_pending_guidances_recommended ~__context ~self:host ~value
  in
  List.iter host_remove
    (List.filter_map Guidance.to_pending_guidance to_be_removed) ;
  List.iter host_add (List.filter_map Guidance.to_pending_guidance to_be_added)

let assert_no_host_pending_mandatory_guidance ~__context ~host =
  let host_pending_mandatory_guidances =
    Db.Host.get_pending_guidances ~__context ~self:host
  in
  if host_pending_mandatory_guidances <> [] then (
    error "%s: %d mandatory guidances are pending for host %s: [%s]"
      __FUNCTION__
      (List.length host_pending_mandatory_guidances)
      (Ref.string_of host)
      (host_pending_mandatory_guidances
      |> List.map Updateinfo.Guidance.of_pending_guidance
      |> List.map Updateinfo.Guidance.to_string
      |> String.concat ";"
      ) ;
    raise
      Api_errors.(
        Server_error
          (host_pending_mandatory_guidances_not_empty, [Ref.string_of host])
      )
  )

let assert_host_evacuation_if_required ~__context ~host ~mandatory =
  let open Guidance in
  let need_evacuation =
    List.exists (fun g -> g = RebootHost || g = EvacuateHost) mandatory
  in
  let resident_vms =
    Db.Host.get_resident_VMs ~__context ~self:host
    |> List.filter (fun self ->
           not (Db.VM.get_is_control_domain ~__context ~self)
       )
  in
  match (need_evacuation, resident_vms <> []) with
  | true, true ->
      raise
        Api_errors.(
          Server_error (host_evacuation_is_required, [Ref.string_of host])
        )
  | _ ->
      ()
