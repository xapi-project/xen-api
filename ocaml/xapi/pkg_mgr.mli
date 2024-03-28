val repoquery_sep : string

type t = Yum | Dnf

val active : unit -> t

module type S = sig
  val repoquery_installed : unit -> string * string list

  val clean_cache : string -> string * string list

  val get_pkgs_from_updateinfo : string -> string list -> string * string list

  val get_updates_from_upgrade_dry_run : string list -> string * string list

  val is_obsoleted : string -> string list -> string * string list

  val get_pkgs_from_repoquery : string -> string list -> string * string list

  val config_repo : string -> string list -> string * string list

  val get_repo_config : string -> string * string list

  val make_cache : string -> string * string list

  val sync_repo : string -> string * string list
end

val get_pkg_mgr : (module S)
