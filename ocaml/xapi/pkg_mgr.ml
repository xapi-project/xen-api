(*
 * Copyright (c) Cloud Software Group, Inc.
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

type mgr = Yum | Dnf

type cmd_line = {cmd: string; params: string list}

module Updateinfo = struct
  type t = Available | Updates

  let to_string = function Available -> "available" | Updates -> "updates"
end

let active () =
  match Sys.file_exists !Xapi_globs.dnf_cmd with true -> Dnf | false -> Yum

module type S = sig
  val manager : mgr

  val repoquery_installed : unit -> cmd_line

  val clean_cache : repo_name:string -> cmd_line

  val get_pkgs_from_updateinfo :
    Updateinfo.t -> repositories:string list -> cmd_line

  val get_updates_from_upgrade_dry_run : repositories:string list -> cmd_line

  val is_obsoleted : pkg_name:string -> repositories:string list -> cmd_line

  val repoquery_updates : repositories:string list -> cmd_line

  val repoquery_available : repositories:string list -> cmd_line

  val config_repo : repo_name:string -> config:string list -> cmd_line

  val get_repo_config : repo_name:string -> cmd_line

  val make_cache : repo_name:string -> cmd_line

  val sync_repo : repo_name:string -> cmd_line

  val apply_upgrade : repositories:string list -> cmd_line
end

module type Args = sig
  val pkg_cmd : string

  val repoquery_cmd : string

  val repoconfig_cmd : string

  val reposync_cmd : string

  val repoquery_installed : unit -> string list

  val clean_cache : string -> string list

  val get_pkgs_from_updateinfo : Updateinfo.t -> string list -> string list

  val get_updates_from_upgrade_dry_run : string list -> string list

  val is_obsoleted : string -> string list -> string list

  val repoquery_updates : string list -> string list

  val repoquery_available : string list -> string list

  val config_repo : string -> string list -> string list

  val get_repo_config : string -> string list

  val make_cache : string -> string list

  val sync_repo : string -> string list

  val apply_upgrade : string list -> string list
end

let repoquery_sep = ":|"

module Common_args = struct
  let fmt =
    ["name"; "epoch"; "version"; "release"; "arch"; "repoid"]
    |> List.map (fun field -> Printf.sprintf "%%{%s}" field) (* %{field} *)
    |> String.concat repoquery_sep

  let clean_cache = function
    | "*" ->
        ["clean"; "all"]
    | name ->
        [
          "--disablerepo=*"
        ; Printf.sprintf "--enablerepo=%s" name
        ; "clean"
        ; "all"
        ]

  let is_obsoleted pkg_name repositories =
    [
      "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
    ; "--whatobsoletes"
    ; pkg_name
    ; "--qf"
    ; "%{name}"
    ]

  let get_updates_from_upgrade_dry_run repositories =
    [
      "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
    ; "--assumeno"
    ; "upgrade"
    ]

  let repoquery repositories =
    [
      "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
    ]

  let config_repo repo_name config =
    (* Add --setopt to repo options *)
    List.map (fun x -> Printf.sprintf "--setopt=%s.%s" repo_name x) config
    |> fun x -> ["--save"] @ x @ [repo_name]

  let make_cache repo_name =
    [
      "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" repo_name
    ; "-y"
    ; "makecache"
    ]

  let sync_repo repo_name =
    [
      "-p"
    ; !Xapi_globs.local_pool_repo_dir
    ; "--download-metadata"
    ; "--delete"
    ; "--newest-only"
    ; Printf.sprintf "--repoid=%s" repo_name
    ]

  let apply_upgrade repositories =
    [
      "-y"
    ; "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
    ; "upgrade"
    ]
end

module Yum_args : Args = struct
  include Common_args

  let repoquery_installed () = ["--all"; "--pkgnarrow=installed"; "--qf"; fmt]

  let pkg_cmd = !Xapi_globs.yum_cmd

  let repoquery_cmd = !Xapi_globs.repoquery_cmd

  let repoconfig_cmd = !Xapi_globs.yum_config_manager_cmd

  let reposync_cmd = !Xapi_globs.reposync_cmd

  let get_updates_from_upgrade_dry_run repositories =
    ["--quiet"] @ Common_args.get_updates_from_upgrade_dry_run repositories

  let is_obsoleted pkg_name repositories =
    ["--all"] @ Common_args.is_obsoleted pkg_name repositories @ ["--plugins"]

  let repoquery_available repositories =
    Common_args.repoquery repositories
    @ ["--qf"; fmt; "--all"; "--pkgnarrow"; "available"; "--plugins"]

  let repoquery_updates repositories =
    Common_args.repoquery repositories
    @ ["--qf"; fmt; "--all"; "--pkgnarrow"; "updates"; "--plugins"]

  let sync_repo repo_name =
    Common_args.sync_repo repo_name @ ["--downloadcomps"; "--plugins"]

  let get_repo_config repo_name = [repo_name]

  let get_pkgs_from_updateinfo updateinfo repositories =
    [
      "-q"
    ; "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
    ; "updateinfo"
    ; "list"
    ; Updateinfo.to_string updateinfo
    ]
end

module Dnf_args : Args = struct
  include Common_args

  let pkg_cmd = !Xapi_globs.dnf_cmd

  let repoquery_cmd = !Xapi_globs.dnf_cmd

  let repoconfig_cmd = !Xapi_globs.dnf_cmd

  let reposync_cmd = !Xapi_globs.dnf_cmd

  (* dnf5 removed ending line breaker, we need to add it back *)
  let fmt = Printf.sprintf "%s\n" Common_args.fmt

  type sub_cmd = Repoquery | Repoconfig | Reposync

  let sub_cmd_to_string = function
    | Reposync ->
        "reposync"
    | Repoconfig ->
        "config-manager"
    | Repoquery ->
        "repoquery"

  let add_sub_cmd sub_cmd params = [sub_cmd_to_string sub_cmd] @ params

  let repoquery_installed () =
    ["--qf"; fmt; "--installed"] |> add_sub_cmd Repoquery

  let config_repo repo_name config =
    config |> List.map (fun x -> Printf.sprintf "%s.%s" repo_name x) |> fun x ->
    ["setopt"] @ x |> add_sub_cmd Repoconfig

  let is_obsoleted pkg_name repositories =
    Common_args.is_obsoleted pkg_name repositories |> add_sub_cmd Repoquery

  let repoquery_available repositories =
    Common_args.repoquery repositories @ ["--qf"; fmt; "--available"]
    |> add_sub_cmd Repoquery

  let repoquery_updates repositories =
    Common_args.repoquery repositories @ ["--qf"; fmt; "--upgrades"]
    |> add_sub_cmd Repoquery

  let sync_repo repo_name =
    Common_args.sync_repo repo_name |> add_sub_cmd Reposync

  let get_repo_config repo_name =
    ["--dump"; repo_name] |> add_sub_cmd Repoconfig

  let get_pkgs_from_updateinfo updateinfo repositories =
    [
      "-q"
    ; "--disablerepo=*"
    ; Printf.sprintf "--enablerepo=%s" (String.concat "," repositories)
    ; "advisory"
    ; "list"
    ; (updateinfo |> Updateinfo.to_string |> fun x -> Printf.sprintf "--%s" x)
    ]
end

module Cmd_line (M : Args) : S = struct
  let manager =
    match M.pkg_cmd with cmd when cmd = !Xapi_globs.dnf_cmd -> Dnf | _ -> Yum

  (* functor to construct comand line and arguments *)
  let repoquery_installed () =
    {cmd= M.repoquery_cmd; params= M.repoquery_installed ()}

  let clean_cache ~repo_name = {cmd= M.pkg_cmd; params= M.clean_cache repo_name}

  let get_pkgs_from_updateinfo updateinfo ~repositories =
    {cmd= M.pkg_cmd; params= M.get_pkgs_from_updateinfo updateinfo repositories}

  let get_updates_from_upgrade_dry_run ~repositories =
    {cmd= M.pkg_cmd; params= M.get_updates_from_upgrade_dry_run repositories}

  let is_obsoleted ~pkg_name ~repositories =
    {cmd= M.repoquery_cmd; params= M.is_obsoleted pkg_name repositories}

  let repoquery_updates ~repositories =
    {cmd= M.repoquery_cmd; params= M.repoquery_updates repositories}

  let repoquery_available ~repositories =
    {cmd= M.repoquery_cmd; params= M.repoquery_available repositories}

  let config_repo ~repo_name ~config =
    {cmd= M.repoconfig_cmd; params= M.config_repo repo_name config}

  let get_repo_config ~repo_name =
    {cmd= M.repoconfig_cmd; params= M.get_repo_config repo_name}

  let make_cache ~repo_name = {cmd= M.pkg_cmd; params= M.make_cache repo_name}

  let sync_repo ~repo_name =
    {cmd= M.reposync_cmd; params= M.sync_repo repo_name}

  let apply_upgrade ~repositories =
    {cmd= M.pkg_cmd; params= M.apply_upgrade repositories}
end

module Yum_cmd = Cmd_line (Yum_args)
module Dnf_cmd = Cmd_line (Dnf_args)

let get_pkg_mgr : (module S) =
  match active () with Dnf -> (module Dnf_cmd) | Yum -> (module Yum_cmd)
