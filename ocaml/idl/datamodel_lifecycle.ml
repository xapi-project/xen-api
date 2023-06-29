let prototyped_of_class = function
  | "Observer" ->
      Some "23.14.0"
  | "VTPM" ->
      Some "22.26.0"
  | _ ->
      None

let prototyped_of_field = function
  | "Observer", "enabled" ->
      Some "23.14.0"
  | "Observer", "components" ->
      Some "23.14.0"
  | "Observer", "endpoints" ->
      Some "23.14.0"
  | "Observer", "attributes" ->
      Some "23.14.0"
  | "Observer", "hosts" ->
      Some "23.14.0"
  | "Observer", "uuid" ->
      Some "23.14.0"
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
      Some "23.1.0"
  | "pool", "telemetry_next_collection" ->
      Some "23.9.0"
  | "pool", "telemetry_frequency" ->
      Some "23.9.0"
  | "pool", "telemetry_uuid" ->
      Some "23.9.0"
  | "pool", "coordinator_bias" ->
      Some "22.37.0"
  | "pool", "migration_compression" ->
      Some "22.33.0"
  | _ ->
      None

let prototyped_of_message = function
  | "Observer", "set_components" ->
      Some "23.14.0"
  | "Observer", "set_endpoints" ->
      Some "23.14.0"
  | "Observer", "set_attributes" ->
      Some "23.14.0"
  | "Observer", "set_enabled" ->
      Some "23.14.0"
  | "Observer", "set_hosts" ->
      Some "23.14.0"
  | "Observer", "unregister" ->
      Some "23.14.0"
  | "Observer", "register" ->
      Some "23.14.0"
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
  | "pool", "reset_telemetry_uuid" ->
      Some "23.9.0"
  | "pool", "set_telemetry_next_collection" ->
      Some "23.9.0"
  | "pool", "set_https_only" ->
      Some "22.27.0"
  | _ ->
      None
