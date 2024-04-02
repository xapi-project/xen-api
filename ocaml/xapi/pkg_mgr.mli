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

val repoquery_sep : string

(* Type of package manager supported *)
type mgr = Yum | Dnf

(*Which pakcage manager is actively using*)
val active : unit -> mgr

(* Interfaces to build command line and params for the package manager
 * Each interface has following return result
 * (comandline * commandline params)  with type (string, string list)
 * *)
module type S = sig
  (* Command line and arguments to perform repoquery installed packages*)
  val repoquery_installed : unit -> string * string list

  (* Command line and arguments to clean a repo cache *)
  val clean_cache : repo_name:string -> string * string list

  (*Command line and arguments to get packages from updateinfo*)
  val get_pkgs_from_updateinfo :
    sub_command:string -> repositories:string list -> string * string list

  (*Command line and arguments to dry run an upgrade, with repositories enabled*)
  val get_updates_from_upgrade_dry_run :
    repositories:string list -> string * string list

  (*Command line and arguments to check whether a package is obsoleted by any other
   * package in given repositories*)
  val is_obsoleted :
    pkg_name:string -> repositories:string list -> string * string list

  (*Command line and arguments to perform repoquery in repositories
   * Filter the output by pkg_narrow*)
  val get_pkgs_from_repoquery :
    pkg_narrow:string -> repositories:string list -> string * string list

  (*Command line and arguments to perform repo configuration*)
  val config_repo :
    repo_name:string -> config:string list -> string * string list

  (*Command line and arguments to query repo configuration*)
  val get_repo_config : repo_name:string -> string * string list

  (* Command line and arguments to make cache for repo*)
  val make_cache : repo_name:string -> string * string list

  (* Command line and arguments to sync with remote repo*)
  val sync_repo : repo_name:string -> string * string list

  (* Command line and arguments to apply upgrades from repos*)
  val apply_upgrade : repositories:string list -> string * string list
end

(*Exposed only for unittest, do not use the modules directly
 * Instead, use get_pkg_mgr to detect active package manager*)
module Yum_cmd : S

module Dnf_cmd : S

(* Get active package manager module to provide command line and arguments*)
val get_pkg_mgr : (module S)
