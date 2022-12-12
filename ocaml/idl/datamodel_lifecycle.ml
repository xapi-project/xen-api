let prototyped_of_class = function
  | "VTPM" ->
      Some "22.26.0"
  | _ ->
      None

let prototyped_of_field = function
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
  | "VM", "actions__after_softreboot" ->
      Some "22.34.0-next"
  | "pool", "coordinator_bias" ->
      Some "22.34.0-next"
  | "pool", "migration_compression" ->
      Some "22.33.0"
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
  | "host", "set_https_only" ->
      Some "22.27.0"
  | "pool", "set_https_only" ->
      Some "22.27.0"
  | _ ->
      None
