let prototyped_of_class = function
  | "VM_group" ->
      Some "24.19.1"
  | "Observer" ->
      Some "23.14.0"
  | "VTPM" ->
      Some "22.26.0"
  | _ ->
      None

let prototyped_of_field = function
  | "VM_group", "VMs" ->
      Some "24.19.1"
  | "VM_group", "placement" ->
      Some "24.19.1"
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
  | "Repository", "origin" ->
      Some "24.23.0"
  | "Repository", "gpgkey_path" ->
      Some "22.12.0"
  | "Certificate", "fingerprint_sha1" ->
      Some "24.20.0"
  | "Certificate", "fingerprint_sha256" ->
      Some "24.20.0"
  | "Cluster_host", "last_update_live" ->
      Some "24.3.0"
  | "Cluster_host", "live" ->
      Some "24.3.0"
  | "Cluster", "live_hosts" ->
      Some "24.3.0"
  | "Cluster", "quorum" ->
      Some "24.3.0"
  | "Cluster", "is_quorate" ->
      Some "24.3.0"
  | "VTPM", "contents" ->
      Some "22.26.0"
  | "VTPM", "is_protected" ->
      Some "22.26.0"
  | "VTPM", "is_unique" ->
      Some "22.26.0"
  | "VTPM", "persistence_backend" ->
      Some "22.26.0"
  | "host", "last_update_hash" ->
      Some "24.10.0"
  | "host", "pending_guidances_full" ->
      Some "24.10.0"
  | "host", "pending_guidances_recommended" ->
      Some "24.10.0"
  | "host", "numa_affinity_policy" ->
      Some "24.0.0"
  | "host", "latest_synced_updates_applied" ->
      Some "23.18.0"
  | "host", "recommended_guidances" ->
      Some "23.18.0"
  | "host", "https_only" ->
      Some "22.27.0"
  | "host", "last_software_update" ->
      Some "22.20.0"
  | "VM_guest_metrics", "netbios_name" ->
      Some "24.28.0"
  | "VM", "groups" ->
      Some "24.19.1"
  | "VM", "pending_guidances_full" ->
      Some "24.10.0"
  | "VM", "pending_guidances_recommended" ->
      Some "24.10.0"
  | "VM", "recommended_guidances" ->
      Some "23.18.0"
  | "VM", "actions__after_softreboot" ->
      Some "23.1.0"
  | "pool", "recommendations" ->
      Some "24.19.1"
  | "pool", "update_sync_enabled" ->
      Some "23.18.0"
  | "pool", "update_sync_day" ->
      Some "23.18.0"
  | "pool", "update_sync_frequency" ->
      Some "23.18.0"
  | "pool", "last_update_sync" ->
      Some "23.18.0"
  | "pool", "telemetry_next_collection" ->
      Some "23.9.0"
  | "pool", "telemetry_frequency" ->
      Some "23.9.0"
  | "pool", "telemetry_uuid" ->
      Some "23.9.0"
  | "pool", "ext_auth_cache_expiry" ->
      Some "24.31.0"
  | "pool", "ext_auth_cache_size" ->
      Some "24.31.0"
  | "pool", "ext_auth_cache_enabled" ->
      Some "24.31.0"
  | "pool", "ext_auth_max_threads" ->
      Some "23.27.0"
  | "pool", "local_auth_max_threads" ->
      Some "23.27.0"
  | "pool", "coordinator_bias" ->
      Some "22.37.0"
  | "pool", "migration_compression" ->
      Some "22.33.0"
  | "pool", "custom_uefi_certificates" ->
      Some "24.0.0"
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
  | "Repository", "introduce_bundle" ->
      Some "24.23.0"
  | "PCI", "get_dom0_access_status" ->
      Some "24.14.0"
  | "PCI", "enable_dom0_access" ->
      Some "24.14.0"
  | "PCI", "disable_dom0_access" ->
      Some "24.14.0"
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
  | "host", "emergency_clear_mandatory_guidance" ->
      Some "24.10.0"
  | "host", "apply_recommended_guidances" ->
      Some "23.18.0"
  | "host", "set_https_only" ->
      Some "22.27.0"
  | "host", "set_numa_affinity_policy" ->
      Some "24.0.0"
  | "VM", "get_secureboot_readiness" ->
      Some "24.17.0"
  | "VM", "set_uefi_mode" ->
      Some "24.17.0"
  | "VM", "restart_device_models" ->
      Some "23.30.0"
  | "VM", "set_groups" ->
      Some "24.19.1"
  | "pool", "get_guest_secureboot_readiness" ->
      Some "24.17.0"
  | "pool", "set_ext_auth_cache_expiry" ->
      Some "24.31.0"
  | "pool", "set_ext_auth_cache_size" ->
      Some "24.31.0"
  | "pool", "set_ext_auth_cache_enabled" ->
      Some "24.31.0"
  | "pool", "set_ext_auth_max_threads" ->
      Some "23.27.0"
  | "pool", "set_local_auth_max_threads" ->
      Some "23.27.0"
  | "pool", "set_update_sync_enabled" ->
      Some "23.18.0"
  | "pool", "configure_update_sync" ->
      Some "23.18.0"
  | "pool", "reset_telemetry_uuid" ->
      Some "23.9.0"
  | "pool", "set_telemetry_next_collection" ->
      Some "23.9.0"
  | "pool", "set_https_only" ->
      Some "22.27.0"
  | "pool", "set_custom_uefi_certificates" ->
      Some "24.0.0"
  | _ ->
      None
