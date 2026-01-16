let prototyped_of_class = function
  | "Rate_limit" ->
      Some "25.39.0"
  | "Driver_variant" ->
      Some "25.2.0"
  | "Host_driver" ->
      Some "25.2.0"
  | "VM_group" ->
      Some "24.19.1"
  | "Observer" ->
      Some "23.14.0"
  | "VTPM" ->
      Some "22.26.0"
  | _ ->
      None

let prototyped_of_field = function
  | "Rate_limit", "fill_rate" ->
      Some "25.39.0"
  | "Rate_limit", "burst_size" ->
      Some "25.39.0"
  | "Rate_limit", "host_ip" ->
      Some "26.1.0"
  | "Rate_limit", "user_agent" ->
      Some "26.1.0"
  | "Rate_limit", "uuid" ->
      Some "25.39.0"
  | "Driver_variant", "status" ->
      Some "25.2.0"
  | "Driver_variant", "priority" ->
      Some "25.2.0"
  | "Driver_variant", "hardware_present" ->
      Some "25.2.0"
  | "Driver_variant", "version" ->
      Some "25.2.0"
  | "Driver_variant", "driver" ->
      Some "25.2.0"
  | "Driver_variant", "name" ->
      Some "25.2.0"
  | "Driver_variant", "uuid" ->
      Some "25.2.0"
  | "Host_driver", "info" ->
      Some "25.2.0"
  | "Host_driver", "description" ->
      Some "25.2.0"
  | "Host_driver", "type" ->
      Some "25.2.0"
  | "Host_driver", "selected_variant" ->
      Some "25.2.0"
  | "Host_driver", "active_variant" ->
      Some "25.2.0"
  | "Host_driver", "variants" ->
      Some "25.2.0"
  | "Host_driver", "friendly_name" ->
      Some "25.2.0"
  | "Host_driver", "name" ->
      Some "25.2.0"
  | "Host_driver", "host" ->
      Some "25.2.0"
  | "Host_driver", "uuid" ->
      Some "25.2.0"
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
  | "Repository", "certificate" ->
      Some "25.7.0"
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
  | "Cluster", "expected_hosts" ->
      Some "25.17.0"
  | "Cluster", "live_hosts" ->
      Some "24.3.0"
  | "Cluster", "quorum" ->
      Some "24.3.0"
  | "Cluster", "is_quorate" ->
      Some "24.3.0"
  | "Cluster", "cluster_stack_version" ->
      Some "24.15.0"
  | "VTPM", "contents" ->
      Some "22.26.0"
  | "VTPM", "is_protected" ->
      Some "22.26.0"
  | "VTPM", "is_unique" ->
      Some "22.26.0"
  | "VTPM", "persistence_backend" ->
      Some "22.26.0"
  | "SM", "host_pending_features" ->
      Some "24.37.0"
  | "host", "timezone" ->
      Some "26.0.0"
  | "host", "ntp_custom_servers" ->
      Some "26.0.0"
  | "host", "ntp_mode" ->
      Some "26.0.0"
  | "host", "secure_boot" ->
      Some "25.31.0"
  | "host", "max_cstate" ->
      Some "26.0.0"
  | "host", "ssh_auto_mode" ->
      Some "25.27.0"
  | "host", "console_idle_timeout" ->
      Some "25.21.0"
  | "host", "ssh_expiry" ->
      Some "25.21.0"
  | "host", "ssh_enabled_timeout" ->
      Some "25.21.0"
  | "host", "ssh_enabled" ->
      Some "25.21.0"
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
  | "VM_guest_metrics", "services" ->
      Some "25.15.0"
  | "VM_guest_metrics", "netbios_name" ->
      Some "24.28.0"
  | "VM_metrics", "numa_node_memory" ->
      Some "26.2.0"
  | "VM_metrics", "numa_nodes" ->
      Some "26.2.0"
  | "VM_metrics", "numa_optimised" ->
      Some "26.2.0"
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
  | "pool", "vm_console_idle_timeout" ->
      Some "26.1.0"
  | "pool", "limit_console_sessions" ->
      Some "26.1.0"
  | "pool", "ha_reboot_vm_on_internal_shutdown" ->
      Some "25.16.0"
  | "pool", "license_server" ->
      Some "25.6.0"
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
  | "Rate_limit", "destroy" ->
      Some "26.1.0"
  | "Rate_limit", "create" ->
      Some "26.1.0"
  | "Driver_variant", "select" ->
      Some "25.2.0"
  | "Host_driver", "rescan" ->
      Some "25.2.0"
  | "Host_driver", "deselect" ->
      Some "25.2.0"
  | "Host_driver", "select" ->
      Some "25.2.0"
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
  | "Repository", "introduce_remote_pool" ->
      Some "25.7.0"
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
  | "host", "set_servertime" ->
      Some "26.0.0"
  | "host", "get_ntp_synchronized" ->
      Some "26.0.0"
  | "host", "list_timezones" ->
      Some "26.0.0"
  | "host", "set_timezone" ->
      Some "26.0.0"
  | "host", "get_ntp_servers_status" ->
      Some "26.0.0"
  | "host", "set_ntp_custom_servers" ->
      Some "26.0.0"
  | "host", "set_ntp_mode" ->
      Some "26.0.0"
  | "host", "set_max_cstate" ->
      Some "26.0.0"
  | "host", "update_firewalld_service_status" ->
      Some "25.34.0"
  | "host", "get_tracked_user_agents" ->
      Some "25.34.0"
  | "host", "set_ssh_auto_mode" ->
      Some "25.27.0"
  | "host", "set_console_idle_timeout" ->
      Some "25.21.0"
  | "host", "set_ssh_enabled_timeout" ->
      Some "25.21.0"
  | "host", "disable_ssh" ->
      Some "25.13.0"
  | "host", "enable_ssh" ->
      Some "25.13.0"
  | "host", "emergency_clear_mandatory_guidance" ->
      Some "24.10.0"
  | "host", "apply_recommended_guidances" ->
      Some "23.18.0"
  | "host", "set_https_only" ->
      Some "22.27.0"
  | "host", "rescan_drivers" ->
      Some "25.2.0"
  | "host", "set_numa_affinity_policy" ->
      Some "24.0.0"
  | "VM", "sysprep" ->
      Some "25.24.0"
  | "VM", "get_secureboot_readiness" ->
      Some "24.17.0"
  | "VM", "set_uefi_mode" ->
      Some "24.17.0"
  | "VM", "restart_device_models" ->
      Some "23.30.0"
  | "VM", "call_host_plugin" ->
      Some "25.22.0"
  | "VM", "set_groups" ->
      Some "24.19.1"
  | "pool", "set_ssh_auto_mode" ->
      Some "25.27.0"
  | "pool", "set_console_idle_timeout" ->
      Some "25.21.0"
  | "pool", "set_ssh_enabled_timeout" ->
      Some "25.21.0"
  | "pool", "disable_ssh" ->
      Some "25.13.0"
  | "pool", "enable_ssh" ->
      Some "25.13.0"
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
  | "task", "set_other_config" ->
      Some "25.8.0"
  | "task", "remove_from_other_config" ->
      Some "25.8.0"
  | "task", "add_to_other_config" ->
      Some "25.8.0"
  | _ ->
      None
