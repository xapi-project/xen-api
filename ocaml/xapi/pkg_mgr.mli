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

val repoquery_sep : string

type mgr = Yum | Dnf

type cmd_line = {cmd: string; params: string list}

module Updateinfo : sig
  type t = Available | Updates

  val to_string : t -> string
end

(** Interfaces to build command line and params for the package manager *)
module type S = sig
  val manager : mgr

  val repoquery_installed : unit -> cmd_line
  (** Command line and arguments to perform repoquery installed packages *)

  val clean_cache : repo_name:string -> cmd_line
  (** Command line and arguments to clean a repo cache *)

  val get_pkgs_from_updateinfo :
    Updateinfo.t -> repositories:string list -> cmd_line
  (** Command line and arguments to get packages from updateinfo *)

  val get_updates_from_upgrade_dry_run : repositories:string list -> cmd_line
  (** Command line and arguments to dry run an upgrade, with repositories enabled *)

  val is_obsoleted : pkg_name:string -> repositories:string list -> cmd_line
  (** Command line and arguments to check whether a package is obsoleted by any other
   * package in given repositories *)

  val repoquery_updates : repositories:string list -> cmd_line
  (** Command line and arguments to perform repoquery in repositories
   * query pacakges that are upgrable  *)

  val repoquery_available : repositories:string list -> cmd_line
  (** Command line and arguments to perform repoquery in repositories
   * query pacakges that are available *)

  val config_repo : repo_name:string -> config:string list -> cmd_line
  (** Command line and arguments to perform repo configuration *)

  val get_repo_config : repo_name:string -> cmd_line
  (** Command line and arguments to query repo configuration *)

  val make_cache : repo_name:string -> cmd_line
  (** Command line and arguments to make cache for repo *)

  val sync_repo : repo_name:string -> cmd_line
  (** Command line and arguments to sync with remote repo *)

  val apply_upgrade : repositories:string list -> cmd_line
  (** Command line and arguments to apply upgrades from repos *)
end

(** Exposed only for unittest, do not use the modules directly
 * Instead, use get_pkg_mgr to detect active package manager *)
module Yum_cmd : S

module Dnf_cmd : S

val get_pkg_mgr : (module S)
(** Get active package manager module to provide command line and arguments *)
