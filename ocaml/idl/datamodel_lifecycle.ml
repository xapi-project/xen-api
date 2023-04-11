let prototyped_of_class = function
  | "Tracing" ->
      Some "23.8.0-next"
  | "VTPM" ->
      Some "22.26.0"
  | _ ->
      None

let prototyped_of_field = function
  | "Tracing", "status" ->
      Some "23.8.0-next"
  | "Tracing", "processors" ->
      Some "23.8.0-next"
  | "Tracing", "filters" ->
      Some "23.8.0-next"
  | "Tracing", "components" ->
      Some "23.8.0-next"
  | "Tracing", "endpoints" ->
      Some "23.8.0-next"
  | "Tracing", "tags" ->
      Some "23.8.0-next"
  | "Tracing", "name_label" ->
      Some "23.8.0-next"
  | "Tracing", "hosts" ->
      Some "23.8.0-next"
  | "Tracing", "uuid" ->
      Some "23.8.0-next"
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
      Some "23.8.0-next"
  | "pool", "telemetry_frequency" ->
      Some "23.8.0-next"
  | "pool", "telemetry_uuid" ->
      Some "23.8.0-next"
  | "pool", "coordinator_bias" ->
      Some "22.37.0"
  | "pool", "migration_compression" ->
      Some "22.33.0"
  | _ ->
      None

let prototyped_of_message = function
  | "Tracing", "set_processors" ->
      Some "23.8.0-next"
  | "Tracing", "set_filters" ->
      Some "23.8.0-next"
  | "Tracing", "set_components" ->
      Some "23.8.0-next"
  | "Tracing", "set_endpoints" ->
      Some "23.8.0-next"
  | "Tracing", "set_tags" ->
      Some "23.8.0-next"
  | "Tracing", "set_status" ->
      Some "23.8.0-next"
  | "Tracing", "set_hosts" ->
      Some "23.8.0-next"
  | "Tracing", "destroy" ->
      Some "23.8.0-next"
  | "Tracing", "create" ->
      Some "23.8.0-next"
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
      Some "23.8.0-next"
  | "pool", "set_telemetry_next_collection" ->
      Some "23.8.0-next"
  | "pool", "set_https_only" ->
      Some "22.27.0"
  | _ ->
      None
