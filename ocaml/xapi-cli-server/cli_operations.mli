(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Cli_cmdtable

type rpc = Rpc.call -> Rpc.response

exception ExitWithError of int

val diagnostic_timing_stats : op

val user_password_change : op

val gen_cmds : rpc -> API.ref_session -> (string * cmd_spec) list
(** For each class, the fields to show when doing [xe <class>-list], whereas
    [xe <class>-param-list uuid=...] shows all the non-hidden fields of a
    record. *)

val message_create : op

val message_destroy : op

val message_destroy_all : op

val pool_enable_binary_storage : op

val pool_disable_binary_storage : op

val pool_ha_enable : op

val pool_ha_disable : op

val pool_ha_prevent_restarts_for : op

val pool_ha_compute_max_host_failures_to_tolerate : op

val pool_ha_compute_hypothetical_max_host_failures_to_tolerate : op

val pool_ha_compute_vm_failover_plan : op

val host_ha_xapi_healthcheck : op_with_fd

val pool_sync_database : op

val pool_designate_new_master : op

val pool_management_reconfigure : op

val pool_join : op

val pool_eject : op_with_fd

val pool_emergency_reset_master : op

val pool_emergency_transition_to_master : op

val pool_recover_slaves : op

val pool_initialize_wlb : op

val pool_deconfigure_wlb : op

val pool_send_wlb_configuration : op

val pool_retrieve_wlb_configuration : op

val pool_retrieve_wlb_recommendations : op

val pool_send_test_post : op

val pool_install_ca_certificate : op_with_fd

val pool_uninstall_ca_certificate : op

val pool_certificate_list : op

val pool_crl_install : op_with_fd

val pool_crl_uninstall : op

val pool_crl_list : op

val pool_certificate_sync : op

val pool_enable_redo_log : op

val pool_disable_redo_log : op

val pool_set_vswitch_controller : op

val pool_disable_ssl_legacy : op

val pool_rotate_secret : op

val pool_enable_tls_verification : op

val pool_enable_client_certificate_auth : op

val pool_disable_client_certificate_auth : op

val pool_set_lldp_enabled : op

val pool_sync_updates : op

val pool_configure_repository_proxy : op

val pool_disable_repository_proxy : op

val pool_reset_telemetry_uuid : op

val pool_configure_update_sync : op

val pool_set_update_sync_enabled : op

val pool_install_trusted_certificate : op_with_fd

val pool_uninstall_trusted_certificate : op

val vdi_create : op

val vdi_introduce : op

val vdi_resize : op

val vdi_generate_config : op

val vdi_copy : op

val vdi_pool_migrate : op

val vdi_clone : op

val vdi_snapshot : op

val vdi_destroy : op

val vdi_forget : op

val vdi_update : op

val vdi_unlock : op

val vdi_enable_cbt : op

val vdi_disable_cbt : op

val vdi_data_destroy : op

val vdi_list_changed_blocks : op_with_fd

val diagnostic_vdi_status : op

val diagnostic_vm_status : op

val vbd_create : op

val vbd_destroy : op

val vbd_eject : op

val vbd_insert : op

val vbd_plug : op

val vbd_unplug : op

val vbd_pause : op

val vbd_unpause : op

val sr_scan : op

val sr_create : op_with_fd

val sr_introduce : op

val sr_probe : op

val sr_probe_ext : op

val sr_destroy : op

val sr_forget : op

val sr_update : op

val sr_enable_database_replication : op

val sr_disable_database_replication : op

val pbd_create : op

val pbd_destroy : op

val pbd_plug : op

val pbd_unplug : op

val vif_create : op

val vif_destroy : op

val vif_plug : op

val vif_unplug : op

val vif_configure_ipv4 : op

val vif_configure_ipv6 : op

val vif_move : op

val net_create : op

val net_destroy : op

val net_attach : op

val vm_create : op

val vm_destroy : op

val event_wait : op

exception Multiple_failure of (string * string) list

val vm_compute_memory_overhead : op

val vm_memory_dynamic_range_set : op

val vm_memory_static_range_set : op

val vm_memory_limits_set : op

val vm_memory_set : op

val vm_memory_target_set : op

val vm_memory_target_wait : op

val vm_call_plugin : op_with_fd

val vm_call_host_plugin : op_with_fd

val vm_data_source_list : op

val vm_data_source_record : op

val vm_data_source_query : op

val vm_data_source_forget : op

val vm_sysprep : op_with_fd

val sr_data_source_list : op

val sr_data_source_record : op

val sr_data_source_query : op

val sr_data_source_forget : op

val host_data_source_list : op

val host_data_source_record : op

val host_data_source_query : op

val host_data_source_forget : op

val host_compute_free_memory : op

val host_compute_memory_overhead : op

val host_get_server_certificate : op

val host_refresh_server_certificate : op

val host_install_server_certificate : op_with_fd

val host_get_sm_diagnostics : op

val host_get_thread_diagnostics : op

val host_sm_dp_destroy : op

val vm_memory_shadow_multiplier_set : op

val vm_query_services : op

val vm_start : op

val vm_suspend : op

val vm_resume : op

val vm_restart_device_models : op

val vm_pause : op

val vm_unpause : op

val vm_install : op
(** The process of finding the VM in this case is special-cased since we want
    to call the params 'template-name', like a foreign key, sort of. *)

val console : op_with_fd

val vm_uninstall : op_with_fd

val template_uninstall : op_with_fd

val vm_clone : op

val vm_snapshot : op

val vm_snapshot_with_quiesce : op

val vm_checkpoint : op

val snapshot_revert : op

val snapshot_clone : op

val snapshot_copy : op

val snapshot_destroy : op

val snapshot_uninstall : op_with_fd

val vm_copy : op

val vm_reset_powerstate : op

val snapshot_reset_powerstate : op

val vm_shutdown : op

val vm_reboot : op

val vm_compute_maximum_memory : op

val vm_retrieve_wlb_recommendations : op

val vm_migrate : op

val vm_disk_list : bool -> op

val snapshot_disk_list : bool -> op

val vm_crashdump_list : op

val vm_disk_add : op
(** Disk add creates a VDI with the size, sr specified. The name and sector
    size can be optionally specified. A VBD is then created with the device
    name as specified. *)

val vm_disk_remove : op

val vm_cd_remove : op

val vm_cd_add : op

val vm_cd_eject : op

val vm_cd_insert : op

val host_forget : op_with_fd

val host_declare_dead : op_with_fd

val host_license_add : op_with_fd

val host_license_remove : op

val host_license_view : op

val host_apply_edition : op

val host_all_editions : op

val host_evacuate : op

val host_get_vms_which_prevent_evacuation : op

val host_retrieve_wlb_evacuate_recommendations : op

val host_shutdown_agent : op

val host_update_firewalld_service_status : op

val vdi_import : op_with_fd

val vdi_export : op_with_fd

val pool_retrieve_wlb_report : op_with_fd

val pool_retrieve_wlb_diagnostics : op_with_fd

val vm_import : op_with_fd

val blob_get : op_with_fd

val blob_put : op_with_fd

val blob_create : op

val vm_export : op_with_fd

val vm_copy_bios_strings : op

val vm_is_bios_customized : op

val template_export : op_with_fd

val snapshot_export : op_with_fd

val vm_vcpu_hotplug : op

val vm_vif_list : op

val vm_recover : op

val vm_assert_can_be_recovered : op

val vm_set_uefi_mode : op

val vm_get_secureboot_readiness : op

val vm_update_secureboot_certificates_on_boot : op

val cd_list : op

val vlan_create : op

val pool_vlan_create : op

val vlan_destroy : op

val tunnel_create : op

val tunnel_destroy : op

val diagnostic_compact : op

val diagnostic_gc_stats : op

val diagnostic_db_stats : op

val diagnostic_net_stats : op

val diagnostic_license_status : op

module Network_sriov : sig
  val create : op

  val destroy : op
end

val pif_reconfigure_ip : op

val pif_reconfigure_ipv6 : op

val pif_set_primary_address_type : op

val pif_set_lldp_mode : op

val pif_unplug : op

val pif_plug : op

val pif_scan : op

val pif_introduce : op

val pif_forget : op

val pif_db_forget : op

val bond_create : op

val bond_destroy : op

val bond_set_mode : op

val host_disable : op

val host_sync_data : op

val host_enable : op

val host_shutdown : op

val host_reboot : op

val host_power_on : op

val host_prepare_for_poweroff : op

val host_dmesg : op

val host_enable_local_storage_caching : op

val host_disable_local_storage_caching : op

val pool_enable_local_storage_caching : op

val pool_disable_local_storage_caching : op

val pool_apply_edition : op

val host_set_power_on_mode : op

val host_crash_upload : op

val host_crash_destroy : op

val host_bugreport_upload : op

val host_backup : op_with_fd

val pool_dump_db : op_with_fd

val pool_restore_db : op_with_fd

val pool_enable_external_auth : op

val pool_disable_external_auth : op

val pool_external_auth_set_ldaps : op

val pool_get_guest_secureboot_readiness : op

val pool_get_cpu_features : op

val pool_sync_bundle : op_with_fd

val pool_enable_ssh : op

val pool_disable_ssh : op

val host_restore : op_with_fd

val host_get_system_status_capabilities : op

val host_get_system_status : op_with_fd

val host_set_hostname_live : op

val host_call_plugin : op_with_fd

val host_enable_external_auth : op

val host_disable_external_auth : op

val host_external_auth_set_ldaps : op

val host_refresh_pack_info : op

val host_cpu_info : op

val host_get_cpu_features : op

val host_enable_display : op

val host_disable_display : op

val patch_upload : op_with_fd

val update_upload : op_with_fd

val patch_clean : op

val patch_pool_clean : op

val patch_destroy : op

val patch_apply : op

val patch_precheck : op

val patch_pool_apply : op

val host_logs_download : op_with_fd

val host_is_in_emergency_mode : op

val host_emergency_management_reconfigure : op

val host_emergency_ha_disable : op

val host_emergency_reset_server_certificate : op

val host_emergency_disable_tls_verification : op

val host_emergency_reenable_tls_verification : op

val host_reset_server_certificate : op

val host_emergency_clear_mandatory_guidance : op

val host_management_reconfigure : op

val host_management_disable : op

val host_signal_networking_change : op

val host_notify : op

val host_syslog_reconfigure : op

val host_send_debug_keys : op

val task_cancel : op

val subject_add : op

val subject_remove : op

val subject_role_add : op

val subject_role_remove : op

val audit_log_get : op_with_fd

val session_subject_identifier_list : op

val session_subject_identifier_logout : op

val session_subject_identifier_logout_all : op

val secret_create : op

val secret_destroy : op

val vmss_create : op

val vmss_destroy : op

val vm_appliance_create : op

val vm_appliance_destroy : op

val vm_appliance_start : op

val vm_appliance_shutdown : op

val vm_appliance_recover : op

val vm_appliance_assert_can_be_recovered : op

val gpu_group_create : op

val gpu_group_destroy : op

val gpu_group_get_remaining_capacity : op

val vgpu_create : op

val vgpu_destroy : op

val dr_task_create : op

val dr_task_destroy : op

val pgpu_enable_dom0_access : op

val pgpu_disable_dom0_access : op

val lvhd_enable_thin_provisioning : op

val pci_enable_dom0_access : op

val pci_disable_dom0_access : op

val get_dom0_access_status : op

module PVS_site : sig
  val introduce : op

  val forget : op
end

module PVS_server : sig
  val introduce : op

  val forget : op
end

module PVS_proxy : sig
  val create : op

  val destroy : op
end

module PVS_cache_storage : sig
  val create : op

  val destroy : op
end

val update_introduce : op

val update_precheck : op

val update_apply : op

val update_pool_apply : op

val update_pool_clean : op

val update_destroy : op

val update_resync_host : op

val host_apply_updates : op

val host_enable_ssh : op

val host_disable_ssh : op

module SDN_controller : sig
  val introduce : op

  val forget : op
end

module PUSB : sig val scan : op end

module VUSB : sig
  val create : op

  val unplug : op

  val destroy : op
end

module Cluster : sig
  val pool_create : op

  val pool_force_destroy : op

  val pool_destroy : op

  val pool_resync : op

  val create : op

  val destroy : op
end

module Cluster_host : sig
  val create : op

  val enable : op

  val disable : op

  val destroy : op

  val force_destroy : op
end

module Repository : sig
  val introduce : op

  val introduce_bundle : op

  val introduce_remote_pool : op_with_fd

  val forget : op

  val set_gpgkey_path : op
end

module Driver_variant : sig val select : op end

module Host_driver : sig
  val select : op

  val deselect : op

  val rescan : op
end

module VTPM : sig
  val create : op

  val destroy : op
end

module Observer : sig
  val create : op

  val destroy : op
end

module VM_group : sig
  val create : op

  val destroy : op
end

module Caller : sig
  val create : op

  val destroy : op

  val query_usage : op

  val list_usage : op
end

module Rate_limit : sig
  val create : op

  val destroy : op

  val add_caller : op

  val remove_caller : op
end
