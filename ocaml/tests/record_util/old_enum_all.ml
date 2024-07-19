let all_certificate_type = [`ca; `host; `host_internal]

let all_cluster_host_operation = [`enable; `disable; `destroy]

let all_cluster_operation = [`add; `remove; `enable; `disable; `destroy]

let all_vusb_operations = [`attach; `plug; `unplug]

let all_sdn_controller_protocol = [`ssl; `pssl]

let all_pvs_proxy_status =
  [
    `stopped
  ; `initialised
  ; `caching
  ; `incompatible_write_cache_mode
  ; `incompatible_protocol_version
  ]

let all_vgpu_type_implementation =
  [`passthrough; `nvidia; `nvidia_sriov; `gvt_g; `mxgpu]

let all_allocation_algorithm = [`breadth_first; `depth_first]

let all_pgpu_dom0_access =
  [`enabled; `disable_on_reboot; `disabled; `enable_on_reboot]

let all_sriov_configuration_mode = [`sysfs; `modprobe; `manual; `unknown]

let all_tunnel_protocol = [`gre; `vxlan]

let all_cls =
  [`VM; `Host; `SR; `Pool; `VMPP; `VMSS; `PVS_proxy; `VDI; `Certificate]

let all_console_protocol = [`vt100; `rfb; `rdp]

let all_persistence_backend = [`xapi]

let all_vtpm_operations = [`destroy]

let all_vbd_mode = [`RO; `RW]

let all_vbd_type = [`CD; `Disk; `Floppy]

let all_vbd_operations =
  [`attach; `eject; `insert; `plug; `unplug; `unplug_force; `pause; `unpause]

let all_on_boot = [`reset; `persist]

let all_vdi_type =
  [
    `system
  ; `user
  ; `ephemeral
  ; `suspend
  ; `crashdump
  ; `ha_statefile
  ; `metadata
  ; `redo_log
  ; `rrd
  ; `pvs_cache
  ; `cbt_metadata
  ]

let all_vdi_operations =
  [
    `clone
  ; `copy
  ; `resize
  ; `resize_online
  ; `snapshot
  ; `mirror
  ; `destroy
  ; `forget
  ; `update
  ; `force_unlock
  ; `generate_config
  ; `enable_cbt
  ; `disable_cbt
  ; `data_destroy
  ; `list_changed_blocks
  ; `set_on_boot
  ; `blocked
  ]

let all_storage_operations =
  [
    `scan
  ; `destroy
  ; `forget
  ; `plug
  ; `unplug
  ; `update
  ; `vdi_create
  ; `vdi_introduce
  ; `vdi_destroy
  ; `vdi_resize
  ; `vdi_clone
  ; `vdi_snapshot
  ; `vdi_mirror
  ; `vdi_enable_cbt
  ; `vdi_disable_cbt
  ; `vdi_data_destroy
  ; `vdi_list_changed_blocks
  ; `vdi_set_on_boot
  ; `pbd_create
  ; `pbd_destroy
  ]

let all_bond_mode = [`balanceslb; `activebackup; `lacp]

let all_primary_address_type = [`IPv4; `IPv6]

let all_ipv6_configuration_mode = [`None; `DHCP; `Static; `Autoconf]

let all_ip_configuration_mode = [`None; `DHCP; `Static]

let all_pif_igmp_status = [`enabled; `disabled; `unknown]

let all_vif_ipv6_configuration_mode = [`None; `Static]

let all_vif_ipv4_configuration_mode = [`None; `Static]

let all_vif_locking_mode = [`network_default; `locked; `unlocked; `disabled]

let all_vif_operations = [`attach; `plug; `unplug]

let all_network_purpose = [`nbd; `insecure_nbd]

let all_network_default_locking_mode = [`unlocked; `disabled]

let all_network_operations = [`attaching]

let all_host_numa_affinity_policy = [`any; `best_effort; `default_policy]

let all_host_sched_gran = [`core; `cpu; `socket]

let all_latest_synced_updates_applied_state = [`yes; `no; `unknown]

let all_update_guidances =
  [
    `reboot_host
  ; `reboot_host_on_livepatch_failure
  ; `restart_toolstack
  ; `restart_device_model
  ]

let all_host_display =
  [`enabled; `disable_on_reboot; `disabled; `enable_on_reboot]

let all_host_allowed_operations =
  [
    `provision
  ; `evacuate
  ; `shutdown
  ; `reboot
  ; `power_on
  ; `vm_start
  ; `vm_resume
  ; `vm_migrate
  ; `apply_updates
  ]

let all_vm_appliance_operation =
  [`start; `clean_shutdown; `hard_shutdown; `shutdown]

let all_vmss_type = [`snapshot; `checkpoint; `snapshot_with_quiesce]

let all_vmss_frequency = [`hourly; `daily; `weekly]

let all_vmpp_archive_target_type = [`none; `cifs; `nfs]

let all_vmpp_archive_frequency = [`never; `always_after_backup; `daily; `weekly]

let all_vmpp_backup_frequency = [`hourly; `daily; `weekly]

let all_vmpp_backup_type = [`snapshot; `checkpoint]

let all_tristate_type = [`yes; `no; `unspecified]

let all_domain_type = [`hvm; `pv; `pv_in_pvh; `pvh; `unspecified]

let all_on_crash_behaviour =
  [
    `destroy
  ; `coredump_and_destroy
  ; `restart
  ; `coredump_and_restart
  ; `preserve
  ; `rename_restart
  ]

let all_vm_operations =
  [
    `snapshot
  ; `clone
  ; `copy
  ; `create_template
  ; `revert
  ; `checkpoint
  ; `snapshot_with_quiesce
  ; `provision
  ; `start
  ; `start_on
  ; `pause
  ; `unpause
  ; `clean_shutdown
  ; `clean_reboot
  ; `hard_shutdown
  ; `power_state_reset
  ; `hard_reboot
  ; `suspend
  ; `csvm
  ; `resume
  ; `resume_on
  ; `pool_migrate
  ; `migrate_send
  ; `get_boot_record
  ; `send_sysrq
  ; `send_trigger
  ; `query_services
  ; `shutdown
  ; `call_plugin
  ; `changing_memory_live
  ; `awaiting_memory_live
  ; `changing_dynamic_range
  ; `changing_static_range
  ; `changing_memory_limits
  ; `changing_shadow_memory
  ; `changing_shadow_memory_live
  ; `changing_VCPUs
  ; `changing_VCPUs_live
  ; `changing_NVRAM
  ; `assert_operation_valid
  ; `data_source_op
  ; `update_allowed_operations
  ; `make_into_template
  ; `import
  ; `export
  ; `metadata_export
  ; `reverting
  ; `destroy
  ; `create_vtpm
  ]

let all_on_normal_exit = [`destroy; `restart]

let all_on_softreboot_behavior = [`soft_reboot; `destroy; `restart; `preserve]

let all_vm_power_state = [`Halted; `Paused; `Running; `Suspended]

let all_update_after_apply_guidance =
  [`restartHVM; `restartPV; `restartHost; `restartXAPI]

let all_after_apply_guidance =
  [`restartHVM; `restartPV; `restartHost; `restartXAPI]

let all_update_sync_frequency = [`daily; `weekly]

let all_telemetry_frequency = [`daily; `weekly; `monthly]

let all_pool_allowed_operations =
  [
    `ha_enable
  ; `ha_disable
  ; `cluster_create
  ; `designate_new_master
  ; `configure_repositories
  ; `sync_updates
  ; `get_updates
  ; `apply_updates
  ; `tls_verification_enable
  ; `cert_refresh
  ; `exchange_certificates_on_join
  ; `exchange_ca_certificates_on_join
  ; `copy_primary_host_certs
  ]

let all_task_status_type =
  [`pending; `success; `failure; `cancelling; `cancelled]

let all_task_allowed_operations = [`cancel; `destroy]

let all_hello_return = [`ok; `unknown_host; `cannot_talk_back]

let all_livepatch_status =
  [`ok_livepatch_complete; `ok_livepatch_incomplete; `ok]

let all_sr_health = [`healthy; `recovering]

let all_event_operation = [`add; `del; `_mod]
