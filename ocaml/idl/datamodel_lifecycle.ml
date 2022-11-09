let prototyped_of_class = function
  | "Gpg_key" ->
      Some "22.31.0-next"
  | "VTPM" ->
      Some "22.26.0"
  | _ ->
      None

let prototyped_of_field = function
  | "Gpg_key", "type" ->
      Some "22.31.0-next"
  | "Gpg_key", "uninstalled" ->
      Some "22.31.0-next"
  | "Gpg_key", "fingerprint" ->
      Some "22.31.0-next"
  | "Gpg_key", "created" ->
      Some "22.31.0-next"
  | "Gpg_key", "name" ->
      Some "22.31.0-next"
  | "Gpg_key", "uuid" ->
      Some "22.31.0-next"
  | "Repository", "gpgkey_path" ->
      Some "22.12.0"
  | "VTPM", "contents" ->
      Some "22.26.0"
  | "VTPM", "is_protected" ->
      Some "22.26.0"
  | "VTPM", "is_unique" ->
      Some "22.26.0"
  | "VTPM", "persistence_backend" ->
      Some "22.26.0"
  | "host", "https_only" ->
      Some "22.27.0"
  | "host", "last_software_update" ->
      Some "22.20.0"
  | _ ->
      None

let prototyped_of_message = function
  | "Repository", "apply_livepatch" ->
      Some "22.20.0"
  | "Repository", "set_gpgkey_path" ->
      Some "22.12.0"
  | "message", "destroy_many" ->
      Some "22.19.0"
  | "VTPM", "set_contents" ->
      Some "22.26.0"
  | "VTPM", "get_contents" ->
      Some "22.26.0"
  | "VTPM", "destroy" ->
      Some "22.26.0"
  | "VTPM", "create" ->
      Some "22.26.0"
  | "host", "uninstall_rpmgpgkey" ->
      Some "22.31.0-next"
  | "host", "install_rpmgpgkey" ->
      Some "22.31.0-next"
  | "host", "set_https_only" ->
      Some "22.27.0"
  | "pool", "get_rpm_pubkey_string" ->
      Some "22.31.0-next"
  | "pool", "uninstall_rpmgpgkey" ->
      Some "22.31.0-next"
  | "pool", "install_rpmgpgkey" ->
      Some "22.31.0-next"
  | "pool", "set_https_only" ->
      Some "22.27.0"
  | _ ->
      None
