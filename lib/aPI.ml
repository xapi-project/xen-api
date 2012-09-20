open Xml
open XMLRPC
open Date

module Ref = struct
type 'a t=string
let of_string x = x
let string_of x = x
end

let log_backtrace = ignore

type __unused = unit 
and ref_session_set = [`session] Ref.t list
and ref_auth_set = [`auth] Ref.t list
and ref_auth = [`auth] Ref.t
and ref_event_set = [`event] Ref.t list
and ref_event = [`event] Ref.t
and ref_user_set = [`user] Ref.t list
and ref_data_source_set = [`data_source] Ref.t list
and ref_data_source = [`data_source] Ref.t
and event_operation = [ `add | `del | `_mod ]
and ref_VGPU_to_vGPU_t_map = (ref_VGPU * vGPU_t) list
and ref_GPU_group_to_gPU_group_t_map = (ref_GPU_group * gPU_group_t) list
and ref_GPU_group_set = [`GPU_group] Ref.t list
and ref_PGPU_to_pGPU_t_map = (ref_PGPU * pGPU_t) list
and ref_PCI_to_pCI_t_map = (ref_PCI * pCI_t) list
and ref_tunnel_to_tunnel_t_map = (ref_tunnel * tunnel_t) list
and ref_secret_to_secret_t_map = (ref_secret * secret_t) list
and ref_secret_set = [`secret] Ref.t list
and ref_message_set = [`message] Ref.t list
and ref_message_to_message_t_map = (ref_message * message_t) list
and message_t = { message_uuid:string; message_name:string; message_priority:int64; message_cls:cls; message_obj_uuid:string; message_timestamp:datetime; message_body:string }
and ref_blob_to_blob_t_map = (ref_blob * blob_t) list
and ref_blob_set = [`blob] Ref.t list
and ref_console_to_console_t_map = (ref_console * console_t) list
and ref_crashdump_to_crashdump_t_map = (ref_crashdump * crashdump_t) list
and ref_PBD_to_pBD_t_map = (ref_PBD * pBD_t) list
and ref_VBD_metrics_to_vBD_metrics_t_map = (ref_VBD_metrics * vBD_metrics_t) list
and ref_VBD_metrics_set = [`VBD_metrics] Ref.t list
and ref_VBD_to_vBD_t_map = (ref_VBD * vBD_t) list
and ref_VDI_to_vDI_t_map = (ref_VDI * vDI_t) list
and ref_SR_to_sR_t_map = (ref_SR * sR_t) list
and ref_SM_to_sM_t_map = (ref_SM * sM_t) list
and ref_SM_set = [`SM] Ref.t list
and ref_VLAN_to_vLAN_t_map = (ref_VLAN * vLAN_t) list
and ref_Bond_to_bond_t_map = (ref_Bond * bond_t) list
and ref_PIF_metrics_to_pIF_metrics_t_map = (ref_PIF_metrics * pIF_metrics_t) list
and ref_PIF_metrics_set = [`PIF_metrics] Ref.t list
and ref_PIF_to_pIF_t_map = (ref_PIF * pIF_t) list
and ref_tunnel_set = [`tunnel] Ref.t list
and ref_VLAN_set = [`VLAN] Ref.t list
and ref_Bond_set = [`Bond] Ref.t list
and ref_VIF_metrics_to_vIF_metrics_t_map = (ref_VIF_metrics * vIF_metrics_t) list
and ref_VIF_metrics_set = [`VIF_metrics] Ref.t list
and ref_VIF_to_vIF_t_map = (ref_VIF * vIF_t) list
and ref_network_to_network_t_map = (ref_network * network_t) list
and ref_network_set = [`network] Ref.t list
and ref_host_cpu_to_host_cpu_t_map = (ref_host_cpu * host_cpu_t) list
and ref_host_metrics_to_host_metrics_t_map = (ref_host_metrics * host_metrics_t) list
and ref_host_metrics_set = [`host_metrics] Ref.t list
and ref_host_patch_to_host_patch_t_map = (ref_host_patch * host_patch_t) list
and ref_host_crashdump_to_host_crashdump_t_map = (ref_host_crashdump * host_crashdump_t) list
and ref_host_to_host_t_map = (ref_host * host_t) list
and ref_PGPU_set = [`PGPU] Ref.t list
and ref_host_cpu_set = [`host_cpu] Ref.t list
and ref_PBD_set = [`PBD] Ref.t list
and ref_host_crashdump_set = [`host_crashdump] Ref.t list
and ref_DR_task_to_dR_task_t_map = (ref_DR_task * dR_task_t) list
and ref_DR_task_set = [`DR_task] Ref.t list
and ref_VM_appliance_to_vM_appliance_t_map = (ref_VM_appliance * vM_appliance_t) list
and ref_VM_appliance_set = [`VM_appliance] Ref.t list
and ref_VMPP_to_vMPP_t_map = (ref_VMPP * vMPP_t) list
and ref_VMPP_set = [`VMPP] Ref.t list
and ref_VM_guest_metrics_to_vM_guest_metrics_t_map = (ref_VM_guest_metrics * vM_guest_metrics_t) list
and ref_VM_guest_metrics_set = [`VM_guest_metrics] Ref.t list
and ref_VM_metrics_to_vM_metrics_t_map = (ref_VM_metrics * vM_metrics_t) list
and ref_VM_metrics_set = [`VM_metrics] Ref.t list
and ref_VM_to_vM_t_map = (ref_VM * vM_t) list
and ref_host_to_string_set_map = (ref_host * string_set) list
and data_source_t_set = data_source_t list
and data_source_t = { data_source_name_label:string; data_source_name_description:string; data_source_enabled:bool; data_source_standard:bool; data_source_units:string; data_source_min:float; data_source_max:float; data_source_value:float }
and ref_VGPU_set = [`VGPU] Ref.t list
and ref_VTPM_set = [`VTPM] Ref.t list
and ref_crashdump_set = [`crashdump] Ref.t list
and ref_VBD_set = [`VBD] Ref.t list
and ref_VIF_set = [`VIF] Ref.t list
and ref_console_set = [`console] Ref.t list
and ref_pool_patch_to_pool_patch_t_map = (ref_pool_patch * pool_patch_t) list
and ref_host_patch_set = [`host_patch] Ref.t list
and ref_pool_patch_set = [`pool_patch] Ref.t list
and ref_pool_to_pool_t_map = (ref_pool * pool_t) list
and ref_pool_set = [`pool] Ref.t list
and ref_VM_to_string_set_map = (ref_VM * string_set) list
and ref_VM_to_string_to_string_map_map = (ref_VM * string_to_string_map) list
and hello_return = [ `ok | `unknown_host | `cannot_talk_back ]
and event_t_set = event_t list
and event_t = { event_id:int64; event_timestamp:datetime; event_class:string; event_operation:event_operation; event_ref:string; event_obj_uuid:string }
and ref_task_to_task_t_map = (ref_task * task_t) list
and ref_role_to_role_t_map = (ref_role * role_t) list
and ref_subject_to_subject_t_map = (ref_subject * subject_t) list
and ref_subject_set = [`subject] Ref.t list
and ref_task_set = [`task] Ref.t list
and session_t = { session_uuid:string; session_this_host:ref_host; session_this_user:ref_user; session_last_active:datetime; session_pool:bool; session_other_config:string_to_string_map; session_is_local_superuser:bool; session_subject:ref_subject; session_validation_time:datetime; session_auth_user_sid:string; session_auth_user_name:string; session_rbac_permissions:string_set; session_tasks:ref_task_set; session_parent:ref_session }
and subject_t = { subject_uuid:string; subject_subject_identifier:string; subject_other_config:string_to_string_map; subject_roles:ref_role_set }
and ref_subject = [`subject] Ref.t
and role_t = { role_uuid:string; role_name_label:string; role_name_description:string; role_subroles:ref_role_set }
and ref_role_set = [`role] Ref.t list
and ref_role = [`role] Ref.t
and task_t = { task_uuid:string; task_name_label:string; task_name_description:string; task_allowed_operations:task_allowed_operations_set; task_current_operations:string_to_task_allowed_operations_map; task_created:datetime; task_finished:datetime; task_status:task_status_type; task_resident_on:ref_host; task_progress:float; task_type:string; task_result:string; task_error_info:string_set; task_other_config:string_to_string_map; task_subtask_of:ref_task; task_subtasks:ref_task_set }
and task_allowed_operations_set = [ `cancel ] list
and string_to_task_allowed_operations_map = (string * task_allowed_operations) list
and task_allowed_operations = [ `cancel ]
and task_status_type = [ `pending | `success | `failure | `cancelling | `cancelled ]
and ref_task = [`task] Ref.t
and pool_t = { pool_uuid:string; pool_name_label:string; pool_name_description:string; pool_master:ref_host; pool_default_SR:ref_SR; pool_suspend_image_SR:ref_SR; pool_crash_dump_SR:ref_SR; pool_other_config:string_to_string_map; pool_ha_enabled:bool; pool_ha_configuration:string_to_string_map; pool_ha_statefiles:string_set; pool_ha_host_failures_to_tolerate:int64; pool_ha_plan_exists_for:int64; pool_ha_allow_overcommit:bool; pool_ha_overcommitted:bool; pool_blobs:string_to_ref_blob_map; pool_tags:string_set; pool_gui_config:string_to_string_map; pool_wlb_url:string; pool_wlb_username:string; pool_wlb_enabled:bool; pool_wlb_verify_cert:bool; pool_redo_log_enabled:bool; pool_redo_log_vdi:ref_VDI; pool_vswitch_controller:string; pool_restrictions:string_to_string_map; pool_metadata_VDIs:ref_VDI_set }
and string_to_int64_map = (string * int64) list
and ref_SR_set = [`SR] Ref.t list
and ref_VM_to_string_map = (ref_VM * string) list
and ref_host_set = [`host] Ref.t list
and ref_VM_set = [`VM] Ref.t list
and pool_patch_t = { pool_patch_uuid:string; pool_patch_name_label:string; pool_patch_name_description:string; pool_patch_version:string; pool_patch_size:int64; pool_patch_pool_applied:bool; pool_patch_host_patches:ref_host_patch_set; pool_patch_after_apply_guidance:after_apply_guidance_set; pool_patch_other_config:string_to_string_map }
and after_apply_guidance_set = [ `restartHVM | `restartPV | `restartHost | `restartXAPI ] list
and after_apply_guidance = [ `restartHVM | `restartPV | `restartHost | `restartXAPI ]
and vM_t = { vM_uuid:string; vM_allowed_operations:vm_operations_set; vM_current_operations:string_to_vm_operations_map; vM_power_state:vm_power_state; vM_name_label:string; vM_name_description:string; vM_user_version:int64; vM_is_a_template:bool; vM_suspend_VDI:ref_VDI; vM_resident_on:ref_host; vM_affinity:ref_host; vM_memory_overhead:int64; vM_memory_target:int64; vM_memory_static_max:int64; vM_memory_dynamic_max:int64; vM_memory_dynamic_min:int64; vM_memory_static_min:int64; vM_VCPUs_params:string_to_string_map; vM_VCPUs_max:int64; vM_VCPUs_at_startup:int64; vM_actions_after_shutdown:on_normal_exit; vM_actions_after_reboot:on_normal_exit; vM_actions_after_crash:on_crash_behaviour; vM_consoles:ref_console_set; vM_VIFs:ref_VIF_set; vM_VBDs:ref_VBD_set; vM_crash_dumps:ref_crashdump_set; vM_VTPMs:ref_VTPM_set; vM_PV_bootloader:string; vM_PV_kernel:string; vM_PV_ramdisk:string; vM_PV_args:string; vM_PV_bootloader_args:string; vM_PV_legacy_args:string; vM_HVM_boot_policy:string; vM_HVM_boot_params:string_to_string_map; vM_HVM_shadow_multiplier:float; vM_platform:string_to_string_map; vM_PCI_bus:string; vM_other_config:string_to_string_map; vM_domid:int64; vM_domarch:string; vM_last_boot_CPU_flags:string_to_string_map; vM_is_control_domain:bool; vM_metrics:ref_VM_metrics; vM_guest_metrics:ref_VM_guest_metrics; vM_last_booted_record:string; vM_recommendations:string; vM_xenstore_data:string_to_string_map; vM_ha_always_run:bool; vM_ha_restart_priority:string; vM_is_a_snapshot:bool; vM_snapshot_of:ref_VM; vM_snapshots:ref_VM_set; vM_snapshot_time:datetime; vM_transportable_snapshot_id:string; vM_blobs:string_to_ref_blob_map; vM_tags:string_set; vM_blocked_operations:vm_operations_to_string_map; vM_snapshot_info:string_to_string_map; vM_snapshot_metadata:string; vM_parent:ref_VM; vM_children:ref_VM_set; vM_bios_strings:string_to_string_map; vM_protection_policy:ref_VMPP; vM_is_snapshot_from_vmpp:bool; vM_appliance:ref_VM_appliance; vM_start_delay:int64; vM_shutdown_delay:int64; vM_order:int64; vM_VGPUs:ref_VGPU_set; vM_attached_PCIs:ref_PCI_set; vM_suspend_SR:ref_SR; vM_version:int64 }
and vm_operations_set = [ `snapshot | `clone | `copy | `create_template | `revert | `checkpoint | `snapshot_with_quiesce | `provision | `start | `start_on | `pause | `unpause | `clean_shutdown | `clean_reboot | `hard_shutdown | `power_state_reset | `hard_reboot | `suspend | `csvm | `resume | `resume_on | `pool_migrate | `migrate_send | `get_boot_record | `send_sysrq | `send_trigger | `query_services | `changing_memory_live | `awaiting_memory_live | `changing_dynamic_range | `changing_static_range | `changing_memory_limits | `changing_shadow_memory | `changing_shadow_memory_live | `changing_VCPUs | `changing_VCPUs_live | `assert_operation_valid | `data_source_op | `update_allowed_operations | `make_into_template | `import | `export | `metadata_export | `reverting | `destroy ] list
and string_to_vm_operations_map = (string * vm_operations) list
and vm_power_state = [ `Halted | `Paused | `Running | `Suspended ]
and on_normal_exit = [ `destroy | `restart ]
and on_crash_behaviour = [ `destroy | `coredump_and_destroy | `restart | `coredump_and_restart | `preserve | `rename_restart ]
and vm_operations_to_string_map = (vm_operations * string) list
and ref_VDI_to_ref_SR_map = (ref_VDI * ref_SR) list
and ref_VIF_to_ref_network_map = (ref_VIF * ref_network) list
and vm_operations = [ `snapshot | `clone | `copy | `create_template | `revert | `checkpoint | `snapshot_with_quiesce | `provision | `start | `start_on | `pause | `unpause | `clean_shutdown | `clean_reboot | `hard_shutdown | `power_state_reset | `hard_reboot | `suspend | `csvm | `resume | `resume_on | `pool_migrate | `migrate_send | `get_boot_record | `send_sysrq | `send_trigger | `query_services | `changing_memory_live | `awaiting_memory_live | `changing_dynamic_range | `changing_static_range | `changing_memory_limits | `changing_shadow_memory | `changing_shadow_memory_live | `changing_VCPUs | `changing_VCPUs_live | `assert_operation_valid | `data_source_op | `update_allowed_operations | `make_into_template | `import | `export | `metadata_export | `reverting | `destroy ]
and vM_metrics_t = { vM_metrics_uuid:string; vM_metrics_memory_actual:int64; vM_metrics_VCPUs_number:int64; vM_metrics_VCPUs_utilisation:int64_to_float_map; vM_metrics_VCPUs_CPU:int64_to_int64_map; vM_metrics_VCPUs_params:string_to_string_map; vM_metrics_VCPUs_flags:int64_to_string_set_map; vM_metrics_state:string_set; vM_metrics_start_time:datetime; vM_metrics_install_time:datetime; vM_metrics_last_updated:datetime; vM_metrics_other_config:string_to_string_map }
and int64_to_float_map = (int64 * float) list
and int64_to_int64_map = (int64 * int64) list
and int64_to_string_set_map = (int64 * string_set) list
and ref_VM_metrics = [`VM_metrics] Ref.t
and vM_guest_metrics_t = { vM_guest_metrics_uuid:string; vM_guest_metrics_os_version:string_to_string_map; vM_guest_metrics_PV_drivers_version:string_to_string_map; vM_guest_metrics_PV_drivers_up_to_date:bool; vM_guest_metrics_memory:string_to_string_map; vM_guest_metrics_disks:string_to_string_map; vM_guest_metrics_networks:string_to_string_map; vM_guest_metrics_other:string_to_string_map; vM_guest_metrics_last_updated:datetime; vM_guest_metrics_other_config:string_to_string_map; vM_guest_metrics_live:bool }
and ref_VM_guest_metrics = [`VM_guest_metrics] Ref.t
and vMPP_t = { vMPP_uuid:string; vMPP_name_label:string; vMPP_name_description:string; vMPP_is_policy_enabled:bool; vMPP_backup_type:vmpp_backup_type; vMPP_backup_retention_value:int64; vMPP_backup_frequency:vmpp_backup_frequency; vMPP_backup_schedule:string_to_string_map; vMPP_is_backup_running:bool; vMPP_backup_last_run_time:datetime; vMPP_archive_target_type:vmpp_archive_target_type; vMPP_archive_target_config:string_to_string_map; vMPP_archive_frequency:vmpp_archive_frequency; vMPP_archive_schedule:string_to_string_map; vMPP_is_archive_running:bool; vMPP_archive_last_run_time:datetime; vMPP_VMs:ref_VM_set; vMPP_is_alarm_enabled:bool; vMPP_alarm_config:string_to_string_map; vMPP_recent_alerts:string_set }
and vmpp_backup_type = [ `snapshot | `checkpoint ]
and vmpp_backup_frequency = [ `hourly | `daily | `weekly ]
and vmpp_archive_frequency = [ `never | `always_after_backup | `daily | `weekly ]
and vmpp_archive_target_type = [ `none | `cifs | `nfs ]
and ref_VMPP = [`VMPP] Ref.t
and vM_appliance_t = { vM_appliance_uuid:string; vM_appliance_name_label:string; vM_appliance_name_description:string; vM_appliance_allowed_operations:vm_appliance_operation_set; vM_appliance_current_operations:string_to_vm_appliance_operation_map; vM_appliance_VMs:ref_VM_set }
and vm_appliance_operation_set = [ `start | `clean_shutdown | `hard_shutdown | `shutdown ] list
and string_to_vm_appliance_operation_map = (string * vm_appliance_operation) list
and vm_appliance_operation = [ `start | `clean_shutdown | `hard_shutdown | `shutdown ]
and ref_VM_appliance = [`VM_appliance] Ref.t
and ref_session = [`session] Ref.t
and dR_task_t = { dR_task_uuid:string; dR_task_introduced_SRs:ref_SR_set }
and host_t = { host_uuid:string; host_name_label:string; host_name_description:string; host_memory_overhead:int64; host_allowed_operations:host_allowed_operations_set; host_current_operations:string_to_host_allowed_operations_map; host_API_version_major:int64; host_API_version_minor:int64; host_API_version_vendor:string; host_API_version_vendor_implementation:string_to_string_map; host_enabled:bool; host_software_version:string_to_string_map; host_other_config:string_to_string_map; host_capabilities:string_set; host_cpu_configuration:string_to_string_map; host_sched_policy:string; host_supported_bootloaders:string_set; host_resident_VMs:ref_VM_set; host_logging:string_to_string_map; host_PIFs:ref_PIF_set; host_suspend_image_sr:ref_SR; host_crash_dump_sr:ref_SR; host_crashdumps:ref_host_crashdump_set; host_patches:ref_host_patch_set; host_PBDs:ref_PBD_set; host_host_CPUs:ref_host_cpu_set; host_cpu_info:string_to_string_map; host_hostname:string; host_address:string; host_metrics:ref_host_metrics; host_license_params:string_to_string_map; host_ha_statefiles:string_set; host_ha_network_peers:string_set; host_blobs:string_to_ref_blob_map; host_tags:string_set; host_external_auth_type:string; host_external_auth_service_name:string; host_external_auth_configuration:string_to_string_map; host_edition:string; host_license_server:string_to_string_map; host_bios_strings:string_to_string_map; host_power_on_mode:string; host_power_on_config:string_to_string_map; host_local_cache_sr:ref_SR; host_chipset_info:string_to_string_map; host_PCIs:ref_PCI_set; host_PGPUs:ref_PGPU_set }
and host_allowed_operations_set = [ `provision | `evacuate | `shutdown | `reboot | `power_on | `vm_start | `vm_resume | `vm_migrate ] list
and string_to_host_allowed_operations_map = (string * host_allowed_operations) list
and host_allowed_operations = [ `provision | `evacuate | `shutdown | `reboot | `power_on | `vm_start | `vm_resume | `vm_migrate ]
and ref_VDI_to_string_map = (ref_VDI * string) list
and ref_VDI_set = [`VDI] Ref.t list
and host_crashdump_t = { host_crashdump_uuid:string; host_crashdump_host:ref_host; host_crashdump_timestamp:datetime; host_crashdump_size:int64; host_crashdump_other_config:string_to_string_map }
and ref_host_crashdump = [`host_crashdump] Ref.t
and host_patch_t = { host_patch_uuid:string; host_patch_name_label:string; host_patch_name_description:string; host_patch_version:string; host_patch_host:ref_host; host_patch_applied:bool; host_patch_timestamp_applied:datetime; host_patch_size:int64; host_patch_pool_patch:ref_pool_patch; host_patch_other_config:string_to_string_map }
and ref_pool_patch = [`pool_patch] Ref.t
and ref_host_patch = [`host_patch] Ref.t
and host_metrics_t = { host_metrics_uuid:string; host_metrics_memory_total:int64; host_metrics_memory_free:int64; host_metrics_live:bool; host_metrics_last_updated:datetime; host_metrics_other_config:string_to_string_map }
and ref_host_metrics = [`host_metrics] Ref.t
and host_cpu_t = { host_cpu_uuid:string; host_cpu_host:ref_host; host_cpu_number:int64; host_cpu_vendor:string; host_cpu_speed:int64; host_cpu_modelname:string; host_cpu_family:int64; host_cpu_model:int64; host_cpu_stepping:string; host_cpu_flags:string; host_cpu_features:string; host_cpu_utilisation:float; host_cpu_other_config:string_to_string_map }
and ref_host_cpu = [`host_cpu] Ref.t
and network_t = { network_uuid:string; network_name_label:string; network_name_description:string; network_allowed_operations:network_operations_set; network_current_operations:string_to_network_operations_map; network_VIFs:ref_VIF_set; network_PIFs:ref_PIF_set; network_MTU:int64; network_other_config:string_to_string_map; network_bridge:string; network_blobs:string_to_ref_blob_map; network_tags:string_set; network_default_locking_mode:network_default_locking_mode }
and network_operations_set = [ `attaching ] list
and string_to_network_operations_map = (string * network_operations) list
and network_operations = [ `attaching ]
and network_default_locking_mode = [ `unlocked | `disabled ]
and vIF_t = { vIF_uuid:string; vIF_allowed_operations:vif_operations_set; vIF_current_operations:string_to_vif_operations_map; vIF_device:string; vIF_network:ref_network; vIF_VM:ref_VM; vIF_MAC:string; vIF_MTU:int64; vIF_other_config:string_to_string_map; vIF_currently_attached:bool; vIF_status_code:int64; vIF_status_detail:string; vIF_runtime_properties:string_to_string_map; vIF_qos_algorithm_type:string; vIF_qos_algorithm_params:string_to_string_map; vIF_qos_supported_algorithms:string_set; vIF_metrics:ref_VIF_metrics; vIF_MAC_autogenerated:bool; vIF_locking_mode:vif_locking_mode; vIF_ipv4_allowed:string_set; vIF_ipv6_allowed:string_set }
and vif_operations_set = [ `attach | `plug | `unplug ] list
and string_to_vif_operations_map = (string * vif_operations) list
and vif_operations = [ `attach | `plug | `unplug ]
and vif_locking_mode = [ `network_default | `locked | `unlocked | `disabled ]
and ref_VIF = [`VIF] Ref.t
and vIF_metrics_t = { vIF_metrics_uuid:string; vIF_metrics_io_read_kbs:float; vIF_metrics_io_write_kbs:float; vIF_metrics_last_updated:datetime; vIF_metrics_other_config:string_to_string_map }
and ref_VIF_metrics = [`VIF_metrics] Ref.t
and pIF_t = { pIF_uuid:string; pIF_device:string; pIF_network:ref_network; pIF_host:ref_host; pIF_MAC:string; pIF_MTU:int64; pIF_VLAN:int64; pIF_metrics:ref_PIF_metrics; pIF_physical:bool; pIF_currently_attached:bool; pIF_ip_configuration_mode:ip_configuration_mode; pIF_IP:string; pIF_netmask:string; pIF_gateway:string; pIF_DNS:string; pIF_bond_slave_of:ref_Bond; pIF_bond_master_of:ref_Bond_set; pIF_VLAN_master_of:ref_VLAN; pIF_VLAN_slave_of:ref_VLAN_set; pIF_management:bool; pIF_other_config:string_to_string_map; pIF_disallow_unplug:bool; pIF_tunnel_access_PIF_of:ref_tunnel_set; pIF_tunnel_transport_PIF_of:ref_tunnel_set; pIF_ipv6_configuration_mode:ipv6_configuration_mode; pIF_IPv6:string_set; pIF_ipv6_gateway:string; pIF_primary_address_type:primary_address_type }
and ip_configuration_mode = [ `None | `DHCP | `Static ]
and ipv6_configuration_mode = [ `None | `DHCP | `Static | `Autoconf ]
and primary_address_type = [ `IPv4 | `IPv6 ]
and pIF_metrics_t = { pIF_metrics_uuid:string; pIF_metrics_io_read_kbs:float; pIF_metrics_io_write_kbs:float; pIF_metrics_carrier:bool; pIF_metrics_vendor_id:string; pIF_metrics_vendor_name:string; pIF_metrics_device_id:string; pIF_metrics_device_name:string; pIF_metrics_speed:int64; pIF_metrics_duplex:bool; pIF_metrics_pci_bus_path:string; pIF_metrics_last_updated:datetime; pIF_metrics_other_config:string_to_string_map }
and ref_PIF_metrics = [`PIF_metrics] Ref.t
and bond_t = { bond_uuid:string; bond_master:ref_PIF; bond_slaves:ref_PIF_set; bond_other_config:string_to_string_map; bond_primary_slave:ref_PIF; bond_mode:bond_mode; bond_properties:string_to_string_map; bond_links_up:int64 }
and ref_PIF_set = [`PIF] Ref.t list
and bond_mode = [ `balanceslb | `activebackup | `lacp ]
and ref_Bond = [`Bond] Ref.t
and vLAN_t = { vLAN_uuid:string; vLAN_tagged_PIF:ref_PIF; vLAN_untagged_PIF:ref_PIF; vLAN_tag:int64; vLAN_other_config:string_to_string_map }
and ref_VLAN = [`VLAN] Ref.t
and sM_t = { sM_uuid:string; sM_name_label:string; sM_name_description:string; sM_type:string; sM_vendor:string; sM_copyright:string; sM_version:string; sM_required_api_version:string; sM_configuration:string_to_string_map; sM_capabilities:string_set; sM_other_config:string_to_string_map; sM_driver_filename:string }
and ref_SM = [`SM] Ref.t
and sR_t = { sR_uuid:string; sR_name_label:string; sR_name_description:string; sR_allowed_operations:storage_operations_set; sR_current_operations:string_to_storage_operations_map; sR_VDIs:ref_VDI_set; sR_PBDs:ref_PBD_set; sR_virtual_allocation:int64; sR_physical_utilisation:int64; sR_physical_size:int64; sR_type:string; sR_content_type:string; sR_shared:bool; sR_other_config:string_to_string_map; sR_tags:string_set; sR_sm_config:string_to_string_map; sR_blobs:string_to_ref_blob_map; sR_local_cache_enabled:bool; sR_introduced_by:ref_DR_task }
and storage_operations_set = [ `scan | `destroy | `forget | `plug | `unplug | `update | `vdi_create | `vdi_introduce | `vdi_destroy | `vdi_resize | `vdi_clone | `vdi_snapshot | `pbd_create | `pbd_destroy ] list
and string_to_storage_operations_map = (string * storage_operations) list
and storage_operations = [ `scan | `destroy | `forget | `plug | `unplug | `update | `vdi_create | `vdi_introduce | `vdi_destroy | `vdi_resize | `vdi_clone | `vdi_snapshot | `pbd_create | `pbd_destroy ]
and string_to_ref_blob_map = (string * ref_blob) list
and ref_DR_task = [`DR_task] Ref.t
and vDI_t = { vDI_uuid:string; vDI_name_label:string; vDI_name_description:string; vDI_allowed_operations:vdi_operations_set; vDI_current_operations:string_to_vdi_operations_map; vDI_SR:ref_SR; vDI_VBDs:ref_VBD_set; vDI_crash_dumps:ref_crashdump_set; vDI_virtual_size:int64; vDI_physical_utilisation:int64; vDI_type:vdi_type; vDI_sharable:bool; vDI_read_only:bool; vDI_other_config:string_to_string_map; vDI_storage_lock:bool; vDI_location:string; vDI_managed:bool; vDI_missing:bool; vDI_parent:ref_VDI; vDI_xenstore_data:string_to_string_map; vDI_sm_config:string_to_string_map; vDI_is_a_snapshot:bool; vDI_snapshot_of:ref_VDI; vDI_snapshots:ref_VDI_set; vDI_snapshot_time:datetime; vDI_tags:string_set; vDI_allow_caching:bool; vDI_on_boot:on_boot; vDI_metadata_of_pool:ref_pool; vDI_metadata_latest:bool }
and vdi_operations_set = [ `scan | `clone | `copy | `resize | `resize_online | `snapshot | `destroy | `forget | `update | `force_unlock | `generate_config | `blocked ] list
and string_to_vdi_operations_map = (string * vdi_operations) list
and vdi_operations = [ `scan | `clone | `copy | `resize | `resize_online | `snapshot | `destroy | `forget | `update | `force_unlock | `generate_config | `blocked ]
and vdi_type = [ `system | `user | `ephemeral | `suspend | `crashdump | `ha_statefile | `metadata | `redo_log ]
and ref_pool = [`pool] Ref.t
and on_boot = [ `reset | `persist ]
and vBD_t = { vBD_uuid:string; vBD_allowed_operations:vbd_operations_set; vBD_current_operations:string_to_vbd_operations_map; vBD_VM:ref_VM; vBD_VDI:ref_VDI; vBD_device:string; vBD_userdevice:string; vBD_bootable:bool; vBD_mode:vbd_mode; vBD_type:vbd_type; vBD_unpluggable:bool; vBD_storage_lock:bool; vBD_empty:bool; vBD_other_config:string_to_string_map; vBD_currently_attached:bool; vBD_status_code:int64; vBD_status_detail:string; vBD_runtime_properties:string_to_string_map; vBD_qos_algorithm_type:string; vBD_qos_algorithm_params:string_to_string_map; vBD_qos_supported_algorithms:string_set; vBD_metrics:ref_VBD_metrics }
and vbd_operations_set = [ `attach | `eject | `insert | `plug | `unplug | `unplug_force | `pause | `unpause ] list
and string_to_vbd_operations_map = (string * vbd_operations) list
and vbd_operations = [ `attach | `eject | `insert | `plug | `unplug | `unplug_force | `pause | `unpause ]
and vbd_mode = [ `RO | `RW ]
and vbd_type = [ `CD | `Disk ]
and ref_VBD = [`VBD] Ref.t
and vBD_metrics_t = { vBD_metrics_uuid:string; vBD_metrics_io_read_kbs:float; vBD_metrics_io_write_kbs:float; vBD_metrics_last_updated:datetime; vBD_metrics_other_config:string_to_string_map }
and ref_VBD_metrics = [`VBD_metrics] Ref.t
and pBD_t = { pBD_uuid:string; pBD_host:ref_host; pBD_SR:ref_SR; pBD_device_config:string_to_string_map; pBD_currently_attached:bool; pBD_other_config:string_to_string_map }
and ref_SR = [`SR] Ref.t
and ref_PBD = [`PBD] Ref.t
and crashdump_t = { crashdump_uuid:string; crashdump_VM:ref_VM; crashdump_VDI:ref_VDI; crashdump_other_config:string_to_string_map }
and ref_VDI = [`VDI] Ref.t
and ref_crashdump = [`crashdump] Ref.t
and vTPM_t = { vTPM_uuid:string; vTPM_VM:ref_VM; vTPM_backend:ref_VM }
and ref_VTPM = [`VTPM] Ref.t
and console_t = { console_uuid:string; console_protocol:console_protocol; console_location:string; console_VM:ref_VM; console_other_config:string_to_string_map }
and console_protocol = [ `vt100 | `rfb | `rdp ]
and ref_console = [`console] Ref.t
and user_t = { user_uuid:string; user_short_name:string; user_fullname:string; user_other_config:string_to_string_map }
and ref_user = [`user] Ref.t
and blob_t = { blob_uuid:string; blob_name_label:string; blob_name_description:string; blob_size:int64; blob_public:bool; blob_last_updated:datetime; blob_mime_type:string }
and ref_blob = [`blob] Ref.t
and cls = [ `VM | `Host | `SR | `Pool | `VMPP ]
and datetime = Date.iso8601
and ref_message = [`message] Ref.t
and secret_t = { secret_uuid:string; secret_value:string; secret_other_config:string_to_string_map }
and ref_secret = [`secret] Ref.t
and tunnel_t = { tunnel_uuid:string; tunnel_access_PIF:ref_PIF; tunnel_transport_PIF:ref_PIF; tunnel_status:string_to_string_map; tunnel_other_config:string_to_string_map }
and ref_PIF = [`PIF] Ref.t
and ref_network = [`network] Ref.t
and ref_tunnel = [`tunnel] Ref.t
and pCI_t = { pCI_uuid:string; pCI_class_name:string; pCI_vendor_name:string; pCI_device_name:string; pCI_host:ref_host; pCI_pci_id:string; pCI_dependencies:ref_PCI_set; pCI_other_config:string_to_string_map }
and ref_PCI_set = [`PCI] Ref.t list
and pGPU_t = { pGPU_uuid:string; pGPU_PCI:ref_PCI; pGPU_GPU_group:ref_GPU_group; pGPU_host:ref_host; pGPU_other_config:string_to_string_map }
and ref_PCI = [`PCI] Ref.t
and ref_host = [`host] Ref.t
and ref_PGPU = [`PGPU] Ref.t
and gPU_group_t = { gPU_group_uuid:string; gPU_group_name_label:string; gPU_group_name_description:string; gPU_group_PGPUs:ref_PGPU_set; gPU_group_VGPUs:ref_VGPU_set; gPU_group_GPU_types:string_set; gPU_group_other_config:string_to_string_map }
and string_set = string list
and vGPU_t = { vGPU_uuid:string; vGPU_VM:ref_VM; vGPU_GPU_group:ref_GPU_group; vGPU_device:string; vGPU_currently_attached:bool; vGPU_other_config:string_to_string_map }
and ref_VM = [`VM] Ref.t
and ref_GPU_group = [`GPU_group] Ref.t
and string_to_string_map = (string * string) list
and ref_VGPU = [`VGPU] Ref.t

module From = struct
  open Xml
  
  exception Dispatcher_FieldNotFound of string
  
  let my_assoc fld assoc_list = try List.assoc fld assoc_list with Not_found -> raise (Dispatcher_FieldNotFound fld)
  
  let fromstring_reference = Ref.of_string
  
  let methodCall = From.methodCall
  
  let methodResponse = From.methodResponse
  
  let set f (xml: XMLRPC.xmlrpc) =
    From.array f xml
  
  let map fk fv (xml: XMLRPC.xmlrpc) =
    List.map (fun (k, v) -> fk k, fv v) (From.structure xml)
  
  let structure = From.structure
  
  let rec unused' = ()
  
  and ref_session_set : string -> xml -> ref_session_set =
    fun param -> (fun xml -> try (set (ref_session param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_auth_set : string -> xml -> ref_auth_set =
    fun param -> (fun xml -> try (set (ref_auth param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_auth : string -> xml -> ref_auth =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_event_set : string -> xml -> ref_event_set =
    fun param -> (fun xml -> try (set (ref_event param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_event : string -> xml -> ref_event =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_user_set : string -> xml -> ref_user_set =
    fun param -> (fun xml -> try (set (ref_user param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_data_source_set : string -> xml -> ref_data_source_set =
    fun param -> (fun xml -> try (set (ref_data_source param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_data_source : string -> xml -> ref_data_source =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and event_operation : string -> xml -> event_operation =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "add" -> `add
    | "del" -> `del
    | "mod" -> `_mod
    | _ -> log_backtrace(); raise (RunTimeTypeError("event_operation", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VGPU_to_vGPU_t_map : string -> xml -> ref_VGPU_to_vGPU_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vGPU_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_GPU_group_to_gPU_group_t_map : string -> xml -> ref_GPU_group_to_gPU_group_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((gPU_group_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_GPU_group_set : string -> xml -> ref_GPU_group_set =
    fun param -> (fun xml -> try (set (ref_GPU_group param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PGPU_to_pGPU_t_map : string -> xml -> ref_PGPU_to_pGPU_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((pGPU_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PCI_to_pCI_t_map : string -> xml -> ref_PCI_to_pCI_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((pCI_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_tunnel_to_tunnel_t_map : string -> xml -> ref_tunnel_to_tunnel_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((tunnel_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_secret_to_secret_t_map : string -> xml -> ref_secret_to_secret_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((secret_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_secret_set : string -> xml -> ref_secret_set =
    fun param -> (fun xml -> try (set (ref_secret param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_message_set : string -> xml -> ref_message_set =
    fun param -> (fun xml -> try (set (ref_message param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_message_to_message_t_map : string -> xml -> ref_message_to_message_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((message_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and message_t : string -> xml -> message_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { message_uuid = (string param)(my_assoc "uuid" all);
 message_name = (string param)(my_assoc "name" all);
 message_priority = (int64 param)(my_assoc "priority" all);
 message_cls = (cls param)(my_assoc "cls" all);
 message_obj_uuid = (string param)(my_assoc "obj_uuid" all);
 message_timestamp = (datetime param)(my_assoc "timestamp" all);
 message_body = (string param)(my_assoc "body" all) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_blob_to_blob_t_map : string -> xml -> ref_blob_to_blob_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((blob_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_blob_set : string -> xml -> ref_blob_set =
    fun param -> (fun xml -> try (set (ref_blob param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_console_to_console_t_map : string -> xml -> ref_console_to_console_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((console_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_crashdump_to_crashdump_t_map : string -> xml -> ref_crashdump_to_crashdump_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((crashdump_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PBD_to_pBD_t_map : string -> xml -> ref_PBD_to_pBD_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((pBD_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VBD_metrics_to_vBD_metrics_t_map : string -> xml -> ref_VBD_metrics_to_vBD_metrics_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vBD_metrics_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VBD_metrics_set : string -> xml -> ref_VBD_metrics_set =
    fun param -> (fun xml -> try (set (ref_VBD_metrics param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VBD_to_vBD_t_map : string -> xml -> ref_VBD_to_vBD_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vBD_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VDI_to_vDI_t_map : string -> xml -> ref_VDI_to_vDI_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vDI_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_SR_to_sR_t_map : string -> xml -> ref_SR_to_sR_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((sR_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_SM_to_sM_t_map : string -> xml -> ref_SM_to_sM_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((sM_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_SM_set : string -> xml -> ref_SM_set =
    fun param -> (fun xml -> try (set (ref_SM param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VLAN_to_vLAN_t_map : string -> xml -> ref_VLAN_to_vLAN_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vLAN_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_Bond_to_bond_t_map : string -> xml -> ref_Bond_to_bond_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((bond_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PIF_metrics_to_pIF_metrics_t_map : string -> xml -> ref_PIF_metrics_to_pIF_metrics_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((pIF_metrics_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PIF_metrics_set : string -> xml -> ref_PIF_metrics_set =
    fun param -> (fun xml -> try (set (ref_PIF_metrics param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PIF_to_pIF_t_map : string -> xml -> ref_PIF_to_pIF_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((pIF_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_tunnel_set : string -> xml -> ref_tunnel_set =
    fun param -> (fun xml -> try (set (ref_tunnel param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VLAN_set : string -> xml -> ref_VLAN_set =
    fun param -> (fun xml -> try (set (ref_VLAN param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_Bond_set : string -> xml -> ref_Bond_set =
    fun param -> (fun xml -> try (set (ref_Bond param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VIF_metrics_to_vIF_metrics_t_map : string -> xml -> ref_VIF_metrics_to_vIF_metrics_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vIF_metrics_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VIF_metrics_set : string -> xml -> ref_VIF_metrics_set =
    fun param -> (fun xml -> try (set (ref_VIF_metrics param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VIF_to_vIF_t_map : string -> xml -> ref_VIF_to_vIF_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vIF_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_network_to_network_t_map : string -> xml -> ref_network_to_network_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((network_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_network_set : string -> xml -> ref_network_set =
    fun param -> (fun xml -> try (set (ref_network param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_cpu_to_host_cpu_t_map : string -> xml -> ref_host_cpu_to_host_cpu_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((host_cpu_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_metrics_to_host_metrics_t_map : string -> xml -> ref_host_metrics_to_host_metrics_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((host_metrics_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_metrics_set : string -> xml -> ref_host_metrics_set =
    fun param -> (fun xml -> try (set (ref_host_metrics param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_patch_to_host_patch_t_map : string -> xml -> ref_host_patch_to_host_patch_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((host_patch_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_crashdump_to_host_crashdump_t_map : string -> xml -> ref_host_crashdump_to_host_crashdump_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((host_crashdump_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_to_host_t_map : string -> xml -> ref_host_to_host_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((host_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PGPU_set : string -> xml -> ref_PGPU_set =
    fun param -> (fun xml -> try (set (ref_PGPU param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_cpu_set : string -> xml -> ref_host_cpu_set =
    fun param -> (fun xml -> try (set (ref_host_cpu param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PBD_set : string -> xml -> ref_PBD_set =
    fun param -> (fun xml -> try (set (ref_PBD param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_crashdump_set : string -> xml -> ref_host_crashdump_set =
    fun param -> (fun xml -> try (set (ref_host_crashdump param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_DR_task_to_dR_task_t_map : string -> xml -> ref_DR_task_to_dR_task_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((dR_task_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_DR_task_set : string -> xml -> ref_DR_task_set =
    fun param -> (fun xml -> try (set (ref_DR_task param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_appliance_to_vM_appliance_t_map : string -> xml -> ref_VM_appliance_to_vM_appliance_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vM_appliance_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_appliance_set : string -> xml -> ref_VM_appliance_set =
    fun param -> (fun xml -> try (set (ref_VM_appliance param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VMPP_to_vMPP_t_map : string -> xml -> ref_VMPP_to_vMPP_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vMPP_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VMPP_set : string -> xml -> ref_VMPP_set =
    fun param -> (fun xml -> try (set (ref_VMPP param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_guest_metrics_to_vM_guest_metrics_t_map : string -> xml -> ref_VM_guest_metrics_to_vM_guest_metrics_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vM_guest_metrics_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_guest_metrics_set : string -> xml -> ref_VM_guest_metrics_set =
    fun param -> (fun xml -> try (set (ref_VM_guest_metrics param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_metrics_to_vM_metrics_t_map : string -> xml -> ref_VM_metrics_to_vM_metrics_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vM_metrics_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_metrics_set : string -> xml -> ref_VM_metrics_set =
    fun param -> (fun xml -> try (set (ref_VM_metrics param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_to_vM_t_map : string -> xml -> ref_VM_to_vM_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((vM_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_to_string_set_map : string -> xml -> ref_host_to_string_set_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((string_set param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and data_source_t_set : string -> xml -> data_source_t_set =
    fun param -> (fun xml -> try (set (data_source_t param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and data_source_t : string -> xml -> data_source_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { data_source_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 data_source_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 data_source_enabled = (bool param)(my_assoc "enabled" all);
 data_source_standard = (bool param)(my_assoc "standard" all);
 data_source_units = (string param)(my_assoc "units" all);
 data_source_min = (float param)(my_assoc "min" all);
 data_source_max = (float param)(my_assoc "max" all);
 data_source_value = (float param)(my_assoc "value" all) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VGPU_set : string -> xml -> ref_VGPU_set =
    fun param -> (fun xml -> try (set (ref_VGPU param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VTPM_set : string -> xml -> ref_VTPM_set =
    fun param -> (fun xml -> try (set (ref_VTPM param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_crashdump_set : string -> xml -> ref_crashdump_set =
    fun param -> (fun xml -> try (set (ref_crashdump param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VBD_set : string -> xml -> ref_VBD_set =
    fun param -> (fun xml -> try (set (ref_VBD param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VIF_set : string -> xml -> ref_VIF_set =
    fun param -> (fun xml -> try (set (ref_VIF param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_console_set : string -> xml -> ref_console_set =
    fun param -> (fun xml -> try (set (ref_console param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_pool_patch_to_pool_patch_t_map : string -> xml -> ref_pool_patch_to_pool_patch_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((pool_patch_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_patch_set : string -> xml -> ref_host_patch_set =
    fun param -> (fun xml -> try (set (ref_host_patch param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_pool_patch_set : string -> xml -> ref_pool_patch_set =
    fun param -> (fun xml -> try (set (ref_pool_patch param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_pool_to_pool_t_map : string -> xml -> ref_pool_to_pool_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((pool_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_pool_set : string -> xml -> ref_pool_set =
    fun param -> (fun xml -> try (set (ref_pool param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_to_string_set_map : string -> xml -> ref_VM_to_string_set_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((string_set param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_to_string_to_string_map_map : string -> xml -> ref_VM_to_string_to_string_map_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((string_to_string_map param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and hello_return : string -> xml -> hello_return =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "ok" -> `ok
    | "unknown_host" -> `unknown_host
    | "cannot_talk_back" -> `cannot_talk_back
    | _ -> log_backtrace(); raise (RunTimeTypeError("hello_return", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and event_t_set : string -> xml -> event_t_set =
    fun param -> (fun xml -> try (set (event_t param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and event_t : string -> xml -> event_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { event_id = (int64 param)(my_assoc "id" all);
 event_timestamp = (datetime param)(my_assoc "timestamp" all);
 event_class = (string param)(my_assoc "class" all);
 event_operation = (event_operation param)(my_assoc "operation" all);
 event_ref = (string param)(my_assoc "ref" all);
 event_obj_uuid = (string param)(my_assoc "obj_uuid" all) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_task_to_task_t_map : string -> xml -> ref_task_to_task_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((task_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_role_to_role_t_map : string -> xml -> ref_role_to_role_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((role_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_subject_to_subject_t_map : string -> xml -> ref_subject_to_subject_t_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((subject_t param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_subject_set : string -> xml -> ref_subject_set =
    fun param -> (fun xml -> try (set (ref_subject param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_task_set : string -> xml -> ref_task_set =
    fun param -> (fun xml -> try (set (ref_task param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and session_t : string -> xml -> session_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { session_uuid = (string param)(my_assoc "uuid" all);
 session_this_host = (ref_host param)(my_assoc "this_host" all);
 session_this_user = (ref_user param)(my_assoc "this_user" all);
 session_last_active = (datetime param)(my_assoc "last_active" all);
 session_pool = (bool param)(my_assoc "pool" all);
 session_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>"));
 session_is_local_superuser = (bool param)(if (List.mem_assoc "is_local_superuser" all) then (my_assoc "is_local_superuser" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 session_subject = (ref_subject param)(if (List.mem_assoc "subject" all) then (my_assoc "subject" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 session_validation_time = (datetime param)(if (List.mem_assoc "validation_time" all) then (my_assoc "validation_time" all) else Xml.parse_string ("<value><dateTime.iso8601>19700101T00:00:00Z</dateTime.iso8601></value>"));
 session_auth_user_sid = (string param)(if (List.mem_assoc "auth_user_sid" all) then (my_assoc "auth_user_sid" all) else Xml.parse_string ("<value/>"));
 session_auth_user_name = (string param)(if (List.mem_assoc "auth_user_name" all) then (my_assoc "auth_user_name" all) else Xml.parse_string ("<value/>"));
 session_rbac_permissions = (string_set param)(if (List.mem_assoc "rbac_permissions" all) then (my_assoc "rbac_permissions" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 session_tasks = (ref_task_set param)(if (List.mem_assoc "tasks" all) then (my_assoc "tasks" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 session_parent = (ref_session param)(if (List.mem_assoc "parent" all) then (my_assoc "parent" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and subject_t : string -> xml -> subject_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { subject_uuid = (string param)(my_assoc "uuid" all);
 subject_subject_identifier = (string param)(if (List.mem_assoc "subject_identifier" all) then (my_assoc "subject_identifier" all) else Xml.parse_string ("<value/>"));
 subject_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>"));
 subject_roles = (ref_role_set param)(if (List.mem_assoc "roles" all) then (my_assoc "roles" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_subject : string -> xml -> ref_subject =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and role_t : string -> xml -> role_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { role_uuid = (string param)(my_assoc "uuid" all);
 role_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 role_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 role_subroles = (ref_role_set param)(if (List.mem_assoc "subroles" all) then (my_assoc "subroles" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_role_set : string -> xml -> ref_role_set =
    fun param -> (fun xml -> try (set (ref_role param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_role : string -> xml -> ref_role =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and task_t : string -> xml -> task_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { task_uuid = (string param)(my_assoc "uuid" all);
 task_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 task_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 task_allowed_operations = (task_allowed_operations_set param)(my_assoc "allowed_operations" all);
 task_current_operations = (string_to_task_allowed_operations_map param)(my_assoc "current_operations" all);
 task_created = (datetime param)(my_assoc "created" all);
 task_finished = (datetime param)(my_assoc "finished" all);
 task_status = (task_status_type param)(my_assoc "status" all);
 task_resident_on = (ref_host param)(my_assoc "resident_on" all);
 task_progress = (float param)(my_assoc "progress" all);
 task_type = (string param)(my_assoc "type" all);
 task_result = (string param)(my_assoc "result" all);
 task_error_info = (string_set param)(my_assoc "error_info" all);
 task_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>"));
 task_subtask_of = (ref_task param)(if (List.mem_assoc "subtask_of" all) then (my_assoc "subtask_of" all) else Xml.parse_string ("<value/>"));
 task_subtasks = (ref_task_set param)(if (List.mem_assoc "subtasks" all) then (my_assoc "subtasks" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and task_allowed_operations_set : string -> xml -> task_allowed_operations_set =
    fun param -> (fun xml -> try (set (task_allowed_operations param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_task_allowed_operations_map : string -> xml -> string_to_task_allowed_operations_map =
    fun param -> (fun xml -> try (map (FromString.string) ((task_allowed_operations param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and task_allowed_operations : string -> xml -> task_allowed_operations =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "cancel" -> `cancel
    | _ -> log_backtrace(); raise (RunTimeTypeError("task_allowed_operations", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and task_status_type : string -> xml -> task_status_type =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "pending" -> `pending
    | "success" -> `success
    | "failure" -> `failure
    | "cancelling" -> `cancelling
    | "cancelled" -> `cancelled
    | _ -> log_backtrace(); raise (RunTimeTypeError("task_status_type", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_task : string -> xml -> ref_task =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and pool_t : string -> xml -> pool_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { pool_uuid = (string param)(my_assoc "uuid" all);
 pool_name_label = (string param)(my_assoc "name_label" all);
 pool_name_description = (string param)(my_assoc "name_description" all);
 pool_master = (ref_host param)(my_assoc "master" all);
 pool_default_SR = (ref_SR param)(my_assoc "default_SR" all);
 pool_suspend_image_SR = (ref_SR param)(my_assoc "suspend_image_SR" all);
 pool_crash_dump_SR = (ref_SR param)(my_assoc "crash_dump_SR" all);
 pool_other_config = (string_to_string_map param)(my_assoc "other_config" all);
 pool_ha_enabled = (bool param)(if (List.mem_assoc "ha_enabled" all) then (my_assoc "ha_enabled" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pool_ha_configuration = (string_to_string_map param)(if (List.mem_assoc "ha_configuration" all) then (my_assoc "ha_configuration" all) else Xml.parse_string ("<value><struct/></value>"));
 pool_ha_statefiles = (string_set param)(if (List.mem_assoc "ha_statefiles" all) then (my_assoc "ha_statefiles" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pool_ha_host_failures_to_tolerate = (int64 param)(if (List.mem_assoc "ha_host_failures_to_tolerate" all) then (my_assoc "ha_host_failures_to_tolerate" all) else Xml.parse_string ("<value>0</value>"));
 pool_ha_plan_exists_for = (int64 param)(if (List.mem_assoc "ha_plan_exists_for" all) then (my_assoc "ha_plan_exists_for" all) else Xml.parse_string ("<value>0</value>"));
 pool_ha_allow_overcommit = (bool param)(if (List.mem_assoc "ha_allow_overcommit" all) then (my_assoc "ha_allow_overcommit" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pool_ha_overcommitted = (bool param)(if (List.mem_assoc "ha_overcommitted" all) then (my_assoc "ha_overcommitted" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pool_blobs = (string_to_ref_blob_map param)(if (List.mem_assoc "blobs" all) then (my_assoc "blobs" all) else Xml.parse_string ("<value><struct/></value>"));
 pool_tags = (string_set param)(if (List.mem_assoc "tags" all) then (my_assoc "tags" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pool_gui_config = (string_to_string_map param)(if (List.mem_assoc "gui_config" all) then (my_assoc "gui_config" all) else Xml.parse_string ("<value><struct/></value>"));
 pool_wlb_url = (string param)(if (List.mem_assoc "wlb_url" all) then (my_assoc "wlb_url" all) else Xml.parse_string ("<value/>"));
 pool_wlb_username = (string param)(if (List.mem_assoc "wlb_username" all) then (my_assoc "wlb_username" all) else Xml.parse_string ("<value/>"));
 pool_wlb_enabled = (bool param)(if (List.mem_assoc "wlb_enabled" all) then (my_assoc "wlb_enabled" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pool_wlb_verify_cert = (bool param)(if (List.mem_assoc "wlb_verify_cert" all) then (my_assoc "wlb_verify_cert" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pool_redo_log_enabled = (bool param)(if (List.mem_assoc "redo_log_enabled" all) then (my_assoc "redo_log_enabled" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pool_redo_log_vdi = (ref_VDI param)(if (List.mem_assoc "redo_log_vdi" all) then (my_assoc "redo_log_vdi" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 pool_vswitch_controller = (string param)(if (List.mem_assoc "vswitch_controller" all) then (my_assoc "vswitch_controller" all) else Xml.parse_string ("<value/>"));
 pool_restrictions = (string_to_string_map param)(if (List.mem_assoc "restrictions" all) then (my_assoc "restrictions" all) else Xml.parse_string ("<value><struct/></value>"));
 pool_metadata_VDIs = (ref_VDI_set param)(if (List.mem_assoc "metadata_VDIs" all) then (my_assoc "metadata_VDIs" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_int64_map : string -> xml -> string_to_int64_map =
    fun param -> (fun xml -> try (map (FromString.string) ((int64 param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_SR_set : string -> xml -> ref_SR_set =
    fun param -> (fun xml -> try (set (ref_SR param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_to_string_map : string -> xml -> ref_VM_to_string_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((string param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_set : string -> xml -> ref_host_set =
    fun param -> (fun xml -> try (set (ref_host param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_set : string -> xml -> ref_VM_set =
    fun param -> (fun xml -> try (set (ref_VM param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and pool_patch_t : string -> xml -> pool_patch_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { pool_patch_uuid = (string param)(my_assoc "uuid" all);
 pool_patch_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 pool_patch_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 pool_patch_version = (string param)(if (List.mem_assoc "version" all) then (my_assoc "version" all) else Xml.parse_string ("<value/>"));
 pool_patch_size = (int64 param)(if (List.mem_assoc "size" all) then (my_assoc "size" all) else Xml.parse_string ("<value>0</value>"));
 pool_patch_pool_applied = (bool param)(if (List.mem_assoc "pool_applied" all) then (my_assoc "pool_applied" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pool_patch_host_patches = (ref_host_patch_set param)(if (List.mem_assoc "host_patches" all) then (my_assoc "host_patches" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pool_patch_after_apply_guidance = (after_apply_guidance_set param)(if (List.mem_assoc "after_apply_guidance" all) then (my_assoc "after_apply_guidance" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pool_patch_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and after_apply_guidance_set : string -> xml -> after_apply_guidance_set =
    fun param -> (fun xml -> try (set (after_apply_guidance param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and after_apply_guidance : string -> xml -> after_apply_guidance =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "restarthvm" -> `restartHVM
    | "restartpv" -> `restartPV
    | "restarthost" -> `restartHost
    | "restartxapi" -> `restartXAPI
    | _ -> log_backtrace(); raise (RunTimeTypeError("after_apply_guidance", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vM_t : string -> xml -> vM_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vM_uuid = (string param)(my_assoc "uuid" all);
 vM_allowed_operations = (vm_operations_set param)(my_assoc "allowed_operations" all);
 vM_current_operations = (string_to_vm_operations_map param)(my_assoc "current_operations" all);
 vM_power_state = (vm_power_state param)(my_assoc "power_state" all);
 vM_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 vM_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 vM_user_version = (int64 param)(my_assoc "user_version" all);
 vM_is_a_template = (bool param)(my_assoc "is_a_template" all);
 vM_suspend_VDI = (ref_VDI param)(my_assoc "suspend_VDI" all);
 vM_resident_on = (ref_host param)(my_assoc "resident_on" all);
 vM_affinity = (ref_host param)(my_assoc "affinity" all);
 vM_memory_overhead = (int64 param)(if (List.mem_assoc "memory_overhead" all) then (my_assoc "memory_overhead" all) else Xml.parse_string ("<value>0</value>"));
 vM_memory_target = (int64 param)(if (List.mem_assoc "memory_target" all) then (my_assoc "memory_target" all) else Xml.parse_string ("<value>0</value>"));
 vM_memory_static_max = (int64 param)(my_assoc "memory_static_max" all);
 vM_memory_dynamic_max = (int64 param)(my_assoc "memory_dynamic_max" all);
 vM_memory_dynamic_min = (int64 param)(my_assoc "memory_dynamic_min" all);
 vM_memory_static_min = (int64 param)(my_assoc "memory_static_min" all);
 vM_VCPUs_params = (string_to_string_map param)(my_assoc "VCPUs_params" all);
 vM_VCPUs_max = (int64 param)(my_assoc "VCPUs_max" all);
 vM_VCPUs_at_startup = (int64 param)(my_assoc "VCPUs_at_startup" all);
 vM_actions_after_shutdown = (on_normal_exit param)(my_assoc "actions_after_shutdown" all);
 vM_actions_after_reboot = (on_normal_exit param)(my_assoc "actions_after_reboot" all);
 vM_actions_after_crash = (on_crash_behaviour param)(my_assoc "actions_after_crash" all);
 vM_consoles = (ref_console_set param)(if (List.mem_assoc "consoles" all) then (my_assoc "consoles" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_VIFs = (ref_VIF_set param)(if (List.mem_assoc "VIFs" all) then (my_assoc "VIFs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_VBDs = (ref_VBD_set param)(if (List.mem_assoc "VBDs" all) then (my_assoc "VBDs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_crash_dumps = (ref_crashdump_set param)(if (List.mem_assoc "crash_dumps" all) then (my_assoc "crash_dumps" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_VTPMs = (ref_VTPM_set param)(if (List.mem_assoc "VTPMs" all) then (my_assoc "VTPMs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_PV_bootloader = (string param)(my_assoc "PV_bootloader" all);
 vM_PV_kernel = (string param)(my_assoc "PV_kernel" all);
 vM_PV_ramdisk = (string param)(my_assoc "PV_ramdisk" all);
 vM_PV_args = (string param)(my_assoc "PV_args" all);
 vM_PV_bootloader_args = (string param)(my_assoc "PV_bootloader_args" all);
 vM_PV_legacy_args = (string param)(my_assoc "PV_legacy_args" all);
 vM_HVM_boot_policy = (string param)(my_assoc "HVM_boot_policy" all);
 vM_HVM_boot_params = (string_to_string_map param)(my_assoc "HVM_boot_params" all);
 vM_HVM_shadow_multiplier = (float param)(if (List.mem_assoc "HVM_shadow_multiplier" all) then (my_assoc "HVM_shadow_multiplier" all) else Xml.parse_string ("<value><double>1</double></value>"));
 vM_platform = (string_to_string_map param)(my_assoc "platform" all);
 vM_PCI_bus = (string param)(my_assoc "PCI_bus" all);
 vM_other_config = (string_to_string_map param)(my_assoc "other_config" all);
 vM_domid = (int64 param)(my_assoc "domid" all);
 vM_domarch = (string param)(my_assoc "domarch" all);
 vM_last_boot_CPU_flags = (string_to_string_map param)(my_assoc "last_boot_CPU_flags" all);
 vM_is_control_domain = (bool param)(my_assoc "is_control_domain" all);
 vM_metrics = (ref_VM_metrics param)(my_assoc "metrics" all);
 vM_guest_metrics = (ref_VM_guest_metrics param)(my_assoc "guest_metrics" all);
 vM_last_booted_record = (string param)(if (List.mem_assoc "last_booted_record" all) then (my_assoc "last_booted_record" all) else Xml.parse_string ("<value/>"));
 vM_recommendations = (string param)(my_assoc "recommendations" all);
 vM_xenstore_data = (string_to_string_map param)(if (List.mem_assoc "xenstore_data" all) then (my_assoc "xenstore_data" all) else Xml.parse_string ("<value><struct/></value>"));
 vM_ha_always_run = (bool param)(if (List.mem_assoc "ha_always_run" all) then (my_assoc "ha_always_run" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 vM_ha_restart_priority = (string param)(if (List.mem_assoc "ha_restart_priority" all) then (my_assoc "ha_restart_priority" all) else Xml.parse_string ("<value/>"));
 vM_is_a_snapshot = (bool param)(if (List.mem_assoc "is_a_snapshot" all) then (my_assoc "is_a_snapshot" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 vM_snapshot_of = (ref_VM param)(if (List.mem_assoc "snapshot_of" all) then (my_assoc "snapshot_of" all) else Xml.parse_string ("<value/>"));
 vM_snapshots = (ref_VM_set param)(if (List.mem_assoc "snapshots" all) then (my_assoc "snapshots" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_snapshot_time = (datetime param)(if (List.mem_assoc "snapshot_time" all) then (my_assoc "snapshot_time" all) else Xml.parse_string ("<value><dateTime.iso8601>19700101T00:00:00Z</dateTime.iso8601></value>"));
 vM_transportable_snapshot_id = (string param)(if (List.mem_assoc "transportable_snapshot_id" all) then (my_assoc "transportable_snapshot_id" all) else Xml.parse_string ("<value/>"));
 vM_blobs = (string_to_ref_blob_map param)(if (List.mem_assoc "blobs" all) then (my_assoc "blobs" all) else Xml.parse_string ("<value><struct/></value>"));
 vM_tags = (string_set param)(if (List.mem_assoc "tags" all) then (my_assoc "tags" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_blocked_operations = (vm_operations_to_string_map param)(if (List.mem_assoc "blocked_operations" all) then (my_assoc "blocked_operations" all) else Xml.parse_string ("<value><struct/></value>"));
 vM_snapshot_info = (string_to_string_map param)(if (List.mem_assoc "snapshot_info" all) then (my_assoc "snapshot_info" all) else Xml.parse_string ("<value><struct/></value>"));
 vM_snapshot_metadata = (string param)(if (List.mem_assoc "snapshot_metadata" all) then (my_assoc "snapshot_metadata" all) else Xml.parse_string ("<value/>"));
 vM_parent = (ref_VM param)(if (List.mem_assoc "parent" all) then (my_assoc "parent" all) else Xml.parse_string ("<value/>"));
 vM_children = (ref_VM_set param)(if (List.mem_assoc "children" all) then (my_assoc "children" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_bios_strings = (string_to_string_map param)(if (List.mem_assoc "bios_strings" all) then (my_assoc "bios_strings" all) else Xml.parse_string ("<value><struct/></value>"));
 vM_protection_policy = (ref_VMPP param)(if (List.mem_assoc "protection_policy" all) then (my_assoc "protection_policy" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 vM_is_snapshot_from_vmpp = (bool param)(if (List.mem_assoc "is_snapshot_from_vmpp" all) then (my_assoc "is_snapshot_from_vmpp" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 vM_appliance = (ref_VM_appliance param)(if (List.mem_assoc "appliance" all) then (my_assoc "appliance" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 vM_start_delay = (int64 param)(if (List.mem_assoc "start_delay" all) then (my_assoc "start_delay" all) else Xml.parse_string ("<value>0</value>"));
 vM_shutdown_delay = (int64 param)(if (List.mem_assoc "shutdown_delay" all) then (my_assoc "shutdown_delay" all) else Xml.parse_string ("<value>0</value>"));
 vM_order = (int64 param)(if (List.mem_assoc "order" all) then (my_assoc "order" all) else Xml.parse_string ("<value>0</value>"));
 vM_VGPUs = (ref_VGPU_set param)(if (List.mem_assoc "VGPUs" all) then (my_assoc "VGPUs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_attached_PCIs = (ref_PCI_set param)(if (List.mem_assoc "attached_PCIs" all) then (my_assoc "attached_PCIs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vM_suspend_SR = (ref_SR param)(if (List.mem_assoc "suspend_SR" all) then (my_assoc "suspend_SR" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 vM_version = (int64 param)(if (List.mem_assoc "version" all) then (my_assoc "version" all) else Xml.parse_string ("<value>0</value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vm_operations_set : string -> xml -> vm_operations_set =
    fun param -> (fun xml -> try (set (vm_operations param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_vm_operations_map : string -> xml -> string_to_vm_operations_map =
    fun param -> (fun xml -> try (map (FromString.string) ((vm_operations param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vm_power_state : string -> xml -> vm_power_state =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "halted" -> `Halted
    | "paused" -> `Paused
    | "running" -> `Running
    | "suspended" -> `Suspended
    | _ -> log_backtrace(); raise (RunTimeTypeError("vm_power_state", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and on_normal_exit : string -> xml -> on_normal_exit =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "destroy" -> `destroy
    | "restart" -> `restart
    | _ -> log_backtrace(); raise (RunTimeTypeError("on_normal_exit", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and on_crash_behaviour : string -> xml -> on_crash_behaviour =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "destroy" -> `destroy
    | "coredump_and_destroy" -> `coredump_and_destroy
    | "restart" -> `restart
    | "coredump_and_restart" -> `coredump_and_restart
    | "preserve" -> `preserve
    | "rename_restart" -> `rename_restart
    | _ -> log_backtrace(); raise (RunTimeTypeError("on_crash_behaviour", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vm_operations_to_string_map : string -> xml -> vm_operations_to_string_map =
    fun param -> (fun xml -> try (map (fun txt -> try (
    match String.lowercase txt with
      "snapshot" -> `snapshot
    | "clone" -> `clone
    | "copy" -> `copy
    | "create_template" -> `create_template
    | "revert" -> `revert
    | "checkpoint" -> `checkpoint
    | "snapshot_with_quiesce" -> `snapshot_with_quiesce
    | "provision" -> `provision
    | "start" -> `start
    | "start_on" -> `start_on
    | "pause" -> `pause
    | "unpause" -> `unpause
    | "clean_shutdown" -> `clean_shutdown
    | "clean_reboot" -> `clean_reboot
    | "hard_shutdown" -> `hard_shutdown
    | "power_state_reset" -> `power_state_reset
    | "hard_reboot" -> `hard_reboot
    | "suspend" -> `suspend
    | "csvm" -> `csvm
    | "resume" -> `resume
    | "resume_on" -> `resume_on
    | "pool_migrate" -> `pool_migrate
    | "migrate_send" -> `migrate_send
    | "get_boot_record" -> `get_boot_record
    | "send_sysrq" -> `send_sysrq
    | "send_trigger" -> `send_trigger
    | "query_services" -> `query_services
    | "changing_memory_live" -> `changing_memory_live
    | "awaiting_memory_live" -> `awaiting_memory_live
    | "changing_dynamic_range" -> `changing_dynamic_range
    | "changing_static_range" -> `changing_static_range
    | "changing_memory_limits" -> `changing_memory_limits
    | "changing_shadow_memory" -> `changing_shadow_memory
    | "changing_shadow_memory_live" -> `changing_shadow_memory_live
    | "changing_vcpus" -> `changing_VCPUs
    | "changing_vcpus_live" -> `changing_VCPUs_live
    | "assert_operation_valid" -> `assert_operation_valid
    | "data_source_op" -> `data_source_op
    | "update_allowed_operations" -> `update_allowed_operations
    | "make_into_template" -> `make_into_template
    | "import" -> `import
    | "export" -> `export
    | "metadata_export" -> `metadata_export
    | "reverting" -> `reverting
    | "destroy" -> `destroy
    | _ -> raise (RunTimeTypeError("vm_operations", Xml.parse_string txt))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param]))) ((string param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VDI_to_ref_SR_map : string -> xml -> ref_VDI_to_ref_SR_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((ref_SR param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VIF_to_ref_network_map : string -> xml -> ref_VIF_to_ref_network_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((ref_network param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vm_operations : string -> xml -> vm_operations =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "snapshot" -> `snapshot
    | "clone" -> `clone
    | "copy" -> `copy
    | "create_template" -> `create_template
    | "revert" -> `revert
    | "checkpoint" -> `checkpoint
    | "snapshot_with_quiesce" -> `snapshot_with_quiesce
    | "provision" -> `provision
    | "start" -> `start
    | "start_on" -> `start_on
    | "pause" -> `pause
    | "unpause" -> `unpause
    | "clean_shutdown" -> `clean_shutdown
    | "clean_reboot" -> `clean_reboot
    | "hard_shutdown" -> `hard_shutdown
    | "power_state_reset" -> `power_state_reset
    | "hard_reboot" -> `hard_reboot
    | "suspend" -> `suspend
    | "csvm" -> `csvm
    | "resume" -> `resume
    | "resume_on" -> `resume_on
    | "pool_migrate" -> `pool_migrate
    | "migrate_send" -> `migrate_send
    | "get_boot_record" -> `get_boot_record
    | "send_sysrq" -> `send_sysrq
    | "send_trigger" -> `send_trigger
    | "query_services" -> `query_services
    | "changing_memory_live" -> `changing_memory_live
    | "awaiting_memory_live" -> `awaiting_memory_live
    | "changing_dynamic_range" -> `changing_dynamic_range
    | "changing_static_range" -> `changing_static_range
    | "changing_memory_limits" -> `changing_memory_limits
    | "changing_shadow_memory" -> `changing_shadow_memory
    | "changing_shadow_memory_live" -> `changing_shadow_memory_live
    | "changing_vcpus" -> `changing_VCPUs
    | "changing_vcpus_live" -> `changing_VCPUs_live
    | "assert_operation_valid" -> `assert_operation_valid
    | "data_source_op" -> `data_source_op
    | "update_allowed_operations" -> `update_allowed_operations
    | "make_into_template" -> `make_into_template
    | "import" -> `import
    | "export" -> `export
    | "metadata_export" -> `metadata_export
    | "reverting" -> `reverting
    | "destroy" -> `destroy
    | _ -> log_backtrace(); raise (RunTimeTypeError("vm_operations", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vM_metrics_t : string -> xml -> vM_metrics_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vM_metrics_uuid = (string param)(my_assoc "uuid" all);
 vM_metrics_memory_actual = (int64 param)(my_assoc "memory_actual" all);
 vM_metrics_VCPUs_number = (int64 param)(my_assoc "VCPUs_number" all);
 vM_metrics_VCPUs_utilisation = (int64_to_float_map param)(my_assoc "VCPUs_utilisation" all);
 vM_metrics_VCPUs_CPU = (int64_to_int64_map param)(my_assoc "VCPUs_CPU" all);
 vM_metrics_VCPUs_params = (string_to_string_map param)(my_assoc "VCPUs_params" all);
 vM_metrics_VCPUs_flags = (int64_to_string_set_map param)(my_assoc "VCPUs_flags" all);
 vM_metrics_state = (string_set param)(my_assoc "state" all);
 vM_metrics_start_time = (datetime param)(my_assoc "start_time" all);
 vM_metrics_install_time = (datetime param)(my_assoc "install_time" all);
 vM_metrics_last_updated = (datetime param)(my_assoc "last_updated" all);
 vM_metrics_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and int64_to_float_map : string -> xml -> int64_to_float_map =
    fun param -> (fun xml -> try (map (FromString.int64) ((float param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and int64_to_int64_map : string -> xml -> int64_to_int64_map =
    fun param -> (fun xml -> try (map (FromString.int64) ((int64 param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and int64_to_string_set_map : string -> xml -> int64_to_string_set_map =
    fun param -> (fun xml -> try (map (FromString.int64) ((string_set param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_metrics : string -> xml -> ref_VM_metrics =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vM_guest_metrics_t : string -> xml -> vM_guest_metrics_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vM_guest_metrics_uuid = (string param)(my_assoc "uuid" all);
 vM_guest_metrics_os_version = (string_to_string_map param)(my_assoc "os_version" all);
 vM_guest_metrics_PV_drivers_version = (string_to_string_map param)(my_assoc "PV_drivers_version" all);
 vM_guest_metrics_PV_drivers_up_to_date = (bool param)(my_assoc "PV_drivers_up_to_date" all);
 vM_guest_metrics_memory = (string_to_string_map param)(my_assoc "memory" all);
 vM_guest_metrics_disks = (string_to_string_map param)(my_assoc "disks" all);
 vM_guest_metrics_networks = (string_to_string_map param)(my_assoc "networks" all);
 vM_guest_metrics_other = (string_to_string_map param)(my_assoc "other" all);
 vM_guest_metrics_last_updated = (datetime param)(my_assoc "last_updated" all);
 vM_guest_metrics_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>"));
 vM_guest_metrics_live = (bool param)(if (List.mem_assoc "live" all) then (my_assoc "live" all) else Xml.parse_string ("<value><boolean>0</boolean></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_guest_metrics : string -> xml -> ref_VM_guest_metrics =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vMPP_t : string -> xml -> vMPP_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vMPP_uuid = (string param)(my_assoc "uuid" all);
 vMPP_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 vMPP_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 vMPP_is_policy_enabled = (bool param)(if (List.mem_assoc "is_policy_enabled" all) then (my_assoc "is_policy_enabled" all) else Xml.parse_string ("<value><boolean>1</boolean></value>"));
 vMPP_backup_type = (vmpp_backup_type param)(my_assoc "backup_type" all);
 vMPP_backup_retention_value = (int64 param)(if (List.mem_assoc "backup_retention_value" all) then (my_assoc "backup_retention_value" all) else Xml.parse_string ("<value>7</value>"));
 vMPP_backup_frequency = (vmpp_backup_frequency param)(my_assoc "backup_frequency" all);
 vMPP_backup_schedule = (string_to_string_map param)(my_assoc "backup_schedule" all);
 vMPP_is_backup_running = (bool param)(my_assoc "is_backup_running" all);
 vMPP_backup_last_run_time = (datetime param)(if (List.mem_assoc "backup_last_run_time" all) then (my_assoc "backup_last_run_time" all) else Xml.parse_string ("<value><dateTime.iso8601>19700101T00:00:00Z</dateTime.iso8601></value>"));
 vMPP_archive_target_type = (vmpp_archive_target_type param)(if (List.mem_assoc "archive_target_type" all) then (my_assoc "archive_target_type" all) else Xml.parse_string ("<value>none</value>"));
 vMPP_archive_target_config = (string_to_string_map param)(if (List.mem_assoc "archive_target_config" all) then (my_assoc "archive_target_config" all) else Xml.parse_string ("<value><struct/></value>"));
 vMPP_archive_frequency = (vmpp_archive_frequency param)(if (List.mem_assoc "archive_frequency" all) then (my_assoc "archive_frequency" all) else Xml.parse_string ("<value>never</value>"));
 vMPP_archive_schedule = (string_to_string_map param)(if (List.mem_assoc "archive_schedule" all) then (my_assoc "archive_schedule" all) else Xml.parse_string ("<value><struct/></value>"));
 vMPP_is_archive_running = (bool param)(my_assoc "is_archive_running" all);
 vMPP_archive_last_run_time = (datetime param)(if (List.mem_assoc "archive_last_run_time" all) then (my_assoc "archive_last_run_time" all) else Xml.parse_string ("<value><dateTime.iso8601>19700101T00:00:00Z</dateTime.iso8601></value>"));
 vMPP_VMs = (ref_VM_set param)(if (List.mem_assoc "VMs" all) then (my_assoc "VMs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vMPP_is_alarm_enabled = (bool param)(if (List.mem_assoc "is_alarm_enabled" all) then (my_assoc "is_alarm_enabled" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 vMPP_alarm_config = (string_to_string_map param)(if (List.mem_assoc "alarm_config" all) then (my_assoc "alarm_config" all) else Xml.parse_string ("<value><struct/></value>"));
 vMPP_recent_alerts = (string_set param)(if (List.mem_assoc "recent_alerts" all) then (my_assoc "recent_alerts" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vmpp_backup_type : string -> xml -> vmpp_backup_type =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "snapshot" -> `snapshot
    | "checkpoint" -> `checkpoint
    | _ -> log_backtrace(); raise (RunTimeTypeError("vmpp_backup_type", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vmpp_backup_frequency : string -> xml -> vmpp_backup_frequency =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "hourly" -> `hourly
    | "daily" -> `daily
    | "weekly" -> `weekly
    | _ -> log_backtrace(); raise (RunTimeTypeError("vmpp_backup_frequency", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vmpp_archive_frequency : string -> xml -> vmpp_archive_frequency =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "never" -> `never
    | "always_after_backup" -> `always_after_backup
    | "daily" -> `daily
    | "weekly" -> `weekly
    | _ -> log_backtrace(); raise (RunTimeTypeError("vmpp_archive_frequency", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vmpp_archive_target_type : string -> xml -> vmpp_archive_target_type =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "none" -> `none
    | "cifs" -> `cifs
    | "nfs" -> `nfs
    | _ -> log_backtrace(); raise (RunTimeTypeError("vmpp_archive_target_type", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VMPP : string -> xml -> ref_VMPP =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vM_appliance_t : string -> xml -> vM_appliance_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vM_appliance_uuid = (string param)(my_assoc "uuid" all);
 vM_appliance_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 vM_appliance_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 vM_appliance_allowed_operations = (vm_appliance_operation_set param)(my_assoc "allowed_operations" all);
 vM_appliance_current_operations = (string_to_vm_appliance_operation_map param)(my_assoc "current_operations" all);
 vM_appliance_VMs = (ref_VM_set param)(if (List.mem_assoc "VMs" all) then (my_assoc "VMs" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vm_appliance_operation_set : string -> xml -> vm_appliance_operation_set =
    fun param -> (fun xml -> try (set (vm_appliance_operation param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_vm_appliance_operation_map : string -> xml -> string_to_vm_appliance_operation_map =
    fun param -> (fun xml -> try (map (FromString.string) ((vm_appliance_operation param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vm_appliance_operation : string -> xml -> vm_appliance_operation =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "start" -> `start
    | "clean_shutdown" -> `clean_shutdown
    | "hard_shutdown" -> `hard_shutdown
    | "shutdown" -> `shutdown
    | _ -> log_backtrace(); raise (RunTimeTypeError("vm_appliance_operation", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM_appliance : string -> xml -> ref_VM_appliance =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_session : string -> xml -> ref_session =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and dR_task_t : string -> xml -> dR_task_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { dR_task_uuid = (string param)(my_assoc "uuid" all);
 dR_task_introduced_SRs = (ref_SR_set param)(if (List.mem_assoc "introduced_SRs" all) then (my_assoc "introduced_SRs" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and host_t : string -> xml -> host_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { host_uuid = (string param)(my_assoc "uuid" all);
 host_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 host_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 host_memory_overhead = (int64 param)(if (List.mem_assoc "memory_overhead" all) then (my_assoc "memory_overhead" all) else Xml.parse_string ("<value>0</value>"));
 host_allowed_operations = (host_allowed_operations_set param)(my_assoc "allowed_operations" all);
 host_current_operations = (string_to_host_allowed_operations_map param)(my_assoc "current_operations" all);
 host_API_version_major = (int64 param)(my_assoc "API_version_major" all);
 host_API_version_minor = (int64 param)(my_assoc "API_version_minor" all);
 host_API_version_vendor = (string param)(my_assoc "API_version_vendor" all);
 host_API_version_vendor_implementation = (string_to_string_map param)(my_assoc "API_version_vendor_implementation" all);
 host_enabled = (bool param)(my_assoc "enabled" all);
 host_software_version = (string_to_string_map param)(my_assoc "software_version" all);
 host_other_config = (string_to_string_map param)(my_assoc "other_config" all);
 host_capabilities = (string_set param)(my_assoc "capabilities" all);
 host_cpu_configuration = (string_to_string_map param)(my_assoc "cpu_configuration" all);
 host_sched_policy = (string param)(my_assoc "sched_policy" all);
 host_supported_bootloaders = (string_set param)(my_assoc "supported_bootloaders" all);
 host_resident_VMs = (ref_VM_set param)(if (List.mem_assoc "resident_VMs" all) then (my_assoc "resident_VMs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_logging = (string_to_string_map param)(my_assoc "logging" all);
 host_PIFs = (ref_PIF_set param)(if (List.mem_assoc "PIFs" all) then (my_assoc "PIFs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_suspend_image_sr = (ref_SR param)(my_assoc "suspend_image_sr" all);
 host_crash_dump_sr = (ref_SR param)(my_assoc "crash_dump_sr" all);
 host_crashdumps = (ref_host_crashdump_set param)(if (List.mem_assoc "crashdumps" all) then (my_assoc "crashdumps" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_patches = (ref_host_patch_set param)(if (List.mem_assoc "patches" all) then (my_assoc "patches" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_PBDs = (ref_PBD_set param)(if (List.mem_assoc "PBDs" all) then (my_assoc "PBDs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_host_CPUs = (ref_host_cpu_set param)(if (List.mem_assoc "host_CPUs" all) then (my_assoc "host_CPUs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_cpu_info = (string_to_string_map param)(if (List.mem_assoc "cpu_info" all) then (my_assoc "cpu_info" all) else Xml.parse_string ("<value><struct/></value>"));
 host_hostname = (string param)(my_assoc "hostname" all);
 host_address = (string param)(my_assoc "address" all);
 host_metrics = (ref_host_metrics param)(my_assoc "metrics" all);
 host_license_params = (string_to_string_map param)(my_assoc "license_params" all);
 host_ha_statefiles = (string_set param)(if (List.mem_assoc "ha_statefiles" all) then (my_assoc "ha_statefiles" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_ha_network_peers = (string_set param)(if (List.mem_assoc "ha_network_peers" all) then (my_assoc "ha_network_peers" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_blobs = (string_to_ref_blob_map param)(if (List.mem_assoc "blobs" all) then (my_assoc "blobs" all) else Xml.parse_string ("<value><struct/></value>"));
 host_tags = (string_set param)(if (List.mem_assoc "tags" all) then (my_assoc "tags" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_external_auth_type = (string param)(if (List.mem_assoc "external_auth_type" all) then (my_assoc "external_auth_type" all) else Xml.parse_string ("<value/>"));
 host_external_auth_service_name = (string param)(if (List.mem_assoc "external_auth_service_name" all) then (my_assoc "external_auth_service_name" all) else Xml.parse_string ("<value/>"));
 host_external_auth_configuration = (string_to_string_map param)(if (List.mem_assoc "external_auth_configuration" all) then (my_assoc "external_auth_configuration" all) else Xml.parse_string ("<value><struct/></value>"));
 host_edition = (string param)(if (List.mem_assoc "edition" all) then (my_assoc "edition" all) else Xml.parse_string ("<value/>"));
 host_license_server = (string_to_string_map param)(if (List.mem_assoc "license_server" all) then (my_assoc "license_server" all) else Xml.parse_string ("<value><struct><member><name>address</name><value>localhost</value></member><member><name>port</name><value>27000</value></member></struct></value>"));
 host_bios_strings = (string_to_string_map param)(if (List.mem_assoc "bios_strings" all) then (my_assoc "bios_strings" all) else Xml.parse_string ("<value><struct/></value>"));
 host_power_on_mode = (string param)(if (List.mem_assoc "power_on_mode" all) then (my_assoc "power_on_mode" all) else Xml.parse_string ("<value/>"));
 host_power_on_config = (string_to_string_map param)(if (List.mem_assoc "power_on_config" all) then (my_assoc "power_on_config" all) else Xml.parse_string ("<value><struct/></value>"));
 host_local_cache_sr = (ref_SR param)(if (List.mem_assoc "local_cache_sr" all) then (my_assoc "local_cache_sr" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 host_chipset_info = (string_to_string_map param)(if (List.mem_assoc "chipset_info" all) then (my_assoc "chipset_info" all) else Xml.parse_string ("<value><struct/></value>"));
 host_PCIs = (ref_PCI_set param)(if (List.mem_assoc "PCIs" all) then (my_assoc "PCIs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 host_PGPUs = (ref_PGPU_set param)(if (List.mem_assoc "PGPUs" all) then (my_assoc "PGPUs" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and host_allowed_operations_set : string -> xml -> host_allowed_operations_set =
    fun param -> (fun xml -> try (set (host_allowed_operations param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_host_allowed_operations_map : string -> xml -> string_to_host_allowed_operations_map =
    fun param -> (fun xml -> try (map (FromString.string) ((host_allowed_operations param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and host_allowed_operations : string -> xml -> host_allowed_operations =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "provision" -> `provision
    | "evacuate" -> `evacuate
    | "shutdown" -> `shutdown
    | "reboot" -> `reboot
    | "power_on" -> `power_on
    | "vm_start" -> `vm_start
    | "vm_resume" -> `vm_resume
    | "vm_migrate" -> `vm_migrate
    | _ -> log_backtrace(); raise (RunTimeTypeError("host_allowed_operations", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VDI_to_string_map : string -> xml -> ref_VDI_to_string_map =
    fun param -> (fun xml -> try (map (fromstring_reference) ((string param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VDI_set : string -> xml -> ref_VDI_set =
    fun param -> (fun xml -> try (set (ref_VDI param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and host_crashdump_t : string -> xml -> host_crashdump_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { host_crashdump_uuid = (string param)(my_assoc "uuid" all);
 host_crashdump_host = (ref_host param)(my_assoc "host" all);
 host_crashdump_timestamp = (datetime param)(my_assoc "timestamp" all);
 host_crashdump_size = (int64 param)(my_assoc "size" all);
 host_crashdump_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_crashdump : string -> xml -> ref_host_crashdump =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and host_patch_t : string -> xml -> host_patch_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { host_patch_uuid = (string param)(my_assoc "uuid" all);
 host_patch_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 host_patch_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 host_patch_version = (string param)(my_assoc "version" all);
 host_patch_host = (ref_host param)(my_assoc "host" all);
 host_patch_applied = (bool param)(my_assoc "applied" all);
 host_patch_timestamp_applied = (datetime param)(my_assoc "timestamp_applied" all);
 host_patch_size = (int64 param)(my_assoc "size" all);
 host_patch_pool_patch = (ref_pool_patch param)(if (List.mem_assoc "pool_patch" all) then (my_assoc "pool_patch" all) else Xml.parse_string ("<value/>"));
 host_patch_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_pool_patch : string -> xml -> ref_pool_patch =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_patch : string -> xml -> ref_host_patch =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and host_metrics_t : string -> xml -> host_metrics_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { host_metrics_uuid = (string param)(my_assoc "uuid" all);
 host_metrics_memory_total = (int64 param)(my_assoc "memory_total" all);
 host_metrics_memory_free = (int64 param)(my_assoc "memory_free" all);
 host_metrics_live = (bool param)(my_assoc "live" all);
 host_metrics_last_updated = (datetime param)(my_assoc "last_updated" all);
 host_metrics_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_metrics : string -> xml -> ref_host_metrics =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and host_cpu_t : string -> xml -> host_cpu_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { host_cpu_uuid = (string param)(my_assoc "uuid" all);
 host_cpu_host = (ref_host param)(my_assoc "host" all);
 host_cpu_number = (int64 param)(my_assoc "number" all);
 host_cpu_vendor = (string param)(my_assoc "vendor" all);
 host_cpu_speed = (int64 param)(my_assoc "speed" all);
 host_cpu_modelname = (string param)(my_assoc "modelname" all);
 host_cpu_family = (int64 param)(my_assoc "family" all);
 host_cpu_model = (int64 param)(my_assoc "model" all);
 host_cpu_stepping = (string param)(my_assoc "stepping" all);
 host_cpu_flags = (string param)(my_assoc "flags" all);
 host_cpu_features = (string param)(my_assoc "features" all);
 host_cpu_utilisation = (float param)(my_assoc "utilisation" all);
 host_cpu_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host_cpu : string -> xml -> ref_host_cpu =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and network_t : string -> xml -> network_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { network_uuid = (string param)(my_assoc "uuid" all);
 network_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 network_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 network_allowed_operations = (network_operations_set param)(my_assoc "allowed_operations" all);
 network_current_operations = (string_to_network_operations_map param)(my_assoc "current_operations" all);
 network_VIFs = (ref_VIF_set param)(if (List.mem_assoc "VIFs" all) then (my_assoc "VIFs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 network_PIFs = (ref_PIF_set param)(if (List.mem_assoc "PIFs" all) then (my_assoc "PIFs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 network_MTU = (int64 param)(if (List.mem_assoc "MTU" all) then (my_assoc "MTU" all) else Xml.parse_string ("<value>1500</value>"));
 network_other_config = (string_to_string_map param)(my_assoc "other_config" all);
 network_bridge = (string param)(my_assoc "bridge" all);
 network_blobs = (string_to_ref_blob_map param)(if (List.mem_assoc "blobs" all) then (my_assoc "blobs" all) else Xml.parse_string ("<value><struct/></value>"));
 network_tags = (string_set param)(if (List.mem_assoc "tags" all) then (my_assoc "tags" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 network_default_locking_mode = (network_default_locking_mode param)(if (List.mem_assoc "default_locking_mode" all) then (my_assoc "default_locking_mode" all) else Xml.parse_string ("<value>unlocked</value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and network_operations_set : string -> xml -> network_operations_set =
    fun param -> (fun xml -> try (set (network_operations param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_network_operations_map : string -> xml -> string_to_network_operations_map =
    fun param -> (fun xml -> try (map (FromString.string) ((network_operations param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and network_operations : string -> xml -> network_operations =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "attaching" -> `attaching
    | _ -> log_backtrace(); raise (RunTimeTypeError("network_operations", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and network_default_locking_mode : string -> xml -> network_default_locking_mode =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "unlocked" -> `unlocked
    | "disabled" -> `disabled
    | _ -> log_backtrace(); raise (RunTimeTypeError("network_default_locking_mode", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vIF_t : string -> xml -> vIF_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vIF_uuid = (string param)(my_assoc "uuid" all);
 vIF_allowed_operations = (vif_operations_set param)(my_assoc "allowed_operations" all);
 vIF_current_operations = (string_to_vif_operations_map param)(my_assoc "current_operations" all);
 vIF_device = (string param)(my_assoc "device" all);
 vIF_network = (ref_network param)(my_assoc "network" all);
 vIF_VM = (ref_VM param)(my_assoc "VM" all);
 vIF_MAC = (string param)(my_assoc "MAC" all);
 vIF_MTU = (int64 param)(my_assoc "MTU" all);
 vIF_other_config = (string_to_string_map param)(my_assoc "other_config" all);
 vIF_currently_attached = (bool param)(my_assoc "currently_attached" all);
 vIF_status_code = (int64 param)(my_assoc "status_code" all);
 vIF_status_detail = (string param)(my_assoc "status_detail" all);
 vIF_runtime_properties = (string_to_string_map param)(my_assoc "runtime_properties" all);
 vIF_qos_algorithm_type = (string param)(my_assoc "qos_algorithm_type" all);
 vIF_qos_algorithm_params = (string_to_string_map param)(my_assoc "qos_algorithm_params" all);
 vIF_qos_supported_algorithms = (string_set param)(my_assoc "qos_supported_algorithms" all);
 vIF_metrics = (ref_VIF_metrics param)(my_assoc "metrics" all);
 vIF_MAC_autogenerated = (bool param)(if (List.mem_assoc "MAC_autogenerated" all) then (my_assoc "MAC_autogenerated" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 vIF_locking_mode = (vif_locking_mode param)(if (List.mem_assoc "locking_mode" all) then (my_assoc "locking_mode" all) else Xml.parse_string ("<value>network_default</value>"));
 vIF_ipv4_allowed = (string_set param)(if (List.mem_assoc "ipv4_allowed" all) then (my_assoc "ipv4_allowed" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vIF_ipv6_allowed = (string_set param)(if (List.mem_assoc "ipv6_allowed" all) then (my_assoc "ipv6_allowed" all) else Xml.parse_string ("<value><array><data/></array></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vif_operations_set : string -> xml -> vif_operations_set =
    fun param -> (fun xml -> try (set (vif_operations param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_vif_operations_map : string -> xml -> string_to_vif_operations_map =
    fun param -> (fun xml -> try (map (FromString.string) ((vif_operations param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vif_operations : string -> xml -> vif_operations =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "attach" -> `attach
    | "plug" -> `plug
    | "unplug" -> `unplug
    | _ -> log_backtrace(); raise (RunTimeTypeError("vif_operations", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vif_locking_mode : string -> xml -> vif_locking_mode =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "network_default" -> `network_default
    | "locked" -> `locked
    | "unlocked" -> `unlocked
    | "disabled" -> `disabled
    | _ -> log_backtrace(); raise (RunTimeTypeError("vif_locking_mode", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VIF : string -> xml -> ref_VIF =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vIF_metrics_t : string -> xml -> vIF_metrics_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vIF_metrics_uuid = (string param)(my_assoc "uuid" all);
 vIF_metrics_io_read_kbs = (float param)(my_assoc "io_read_kbs" all);
 vIF_metrics_io_write_kbs = (float param)(my_assoc "io_write_kbs" all);
 vIF_metrics_last_updated = (datetime param)(my_assoc "last_updated" all);
 vIF_metrics_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VIF_metrics : string -> xml -> ref_VIF_metrics =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and pIF_t : string -> xml -> pIF_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { pIF_uuid = (string param)(my_assoc "uuid" all);
 pIF_device = (string param)(my_assoc "device" all);
 pIF_network = (ref_network param)(my_assoc "network" all);
 pIF_host = (ref_host param)(my_assoc "host" all);
 pIF_MAC = (string param)(my_assoc "MAC" all);
 pIF_MTU = (int64 param)(my_assoc "MTU" all);
 pIF_VLAN = (int64 param)(my_assoc "VLAN" all);
 pIF_metrics = (ref_PIF_metrics param)(my_assoc "metrics" all);
 pIF_physical = (bool param)(if (List.mem_assoc "physical" all) then (my_assoc "physical" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pIF_currently_attached = (bool param)(if (List.mem_assoc "currently_attached" all) then (my_assoc "currently_attached" all) else Xml.parse_string ("<value><boolean>1</boolean></value>"));
 pIF_ip_configuration_mode = (ip_configuration_mode param)(if (List.mem_assoc "ip_configuration_mode" all) then (my_assoc "ip_configuration_mode" all) else Xml.parse_string ("<value>None</value>"));
 pIF_IP = (string param)(if (List.mem_assoc "IP" all) then (my_assoc "IP" all) else Xml.parse_string ("<value/>"));
 pIF_netmask = (string param)(if (List.mem_assoc "netmask" all) then (my_assoc "netmask" all) else Xml.parse_string ("<value/>"));
 pIF_gateway = (string param)(if (List.mem_assoc "gateway" all) then (my_assoc "gateway" all) else Xml.parse_string ("<value/>"));
 pIF_DNS = (string param)(if (List.mem_assoc "DNS" all) then (my_assoc "DNS" all) else Xml.parse_string ("<value/>"));
 pIF_bond_slave_of = (ref_Bond param)(if (List.mem_assoc "bond_slave_of" all) then (my_assoc "bond_slave_of" all) else Xml.parse_string ("<value/>"));
 pIF_bond_master_of = (ref_Bond_set param)(if (List.mem_assoc "bond_master_of" all) then (my_assoc "bond_master_of" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pIF_VLAN_master_of = (ref_VLAN param)(if (List.mem_assoc "VLAN_master_of" all) then (my_assoc "VLAN_master_of" all) else Xml.parse_string ("<value/>"));
 pIF_VLAN_slave_of = (ref_VLAN_set param)(if (List.mem_assoc "VLAN_slave_of" all) then (my_assoc "VLAN_slave_of" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pIF_management = (bool param)(if (List.mem_assoc "management" all) then (my_assoc "management" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pIF_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>"));
 pIF_disallow_unplug = (bool param)(if (List.mem_assoc "disallow_unplug" all) then (my_assoc "disallow_unplug" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 pIF_tunnel_access_PIF_of = (ref_tunnel_set param)(if (List.mem_assoc "tunnel_access_PIF_of" all) then (my_assoc "tunnel_access_PIF_of" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pIF_tunnel_transport_PIF_of = (ref_tunnel_set param)(if (List.mem_assoc "tunnel_transport_PIF_of" all) then (my_assoc "tunnel_transport_PIF_of" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pIF_ipv6_configuration_mode = (ipv6_configuration_mode param)(if (List.mem_assoc "ipv6_configuration_mode" all) then (my_assoc "ipv6_configuration_mode" all) else Xml.parse_string ("<value>None</value>"));
 pIF_IPv6 = (string_set param)(if (List.mem_assoc "IPv6" all) then (my_assoc "IPv6" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pIF_ipv6_gateway = (string param)(if (List.mem_assoc "ipv6_gateway" all) then (my_assoc "ipv6_gateway" all) else Xml.parse_string ("<value/>"));
 pIF_primary_address_type = (primary_address_type param)(if (List.mem_assoc "primary_address_type" all) then (my_assoc "primary_address_type" all) else Xml.parse_string ("<value>IPv4</value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ip_configuration_mode : string -> xml -> ip_configuration_mode =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "none" -> `None
    | "dhcp" -> `DHCP
    | "static" -> `Static
    | _ -> log_backtrace(); raise (RunTimeTypeError("ip_configuration_mode", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ipv6_configuration_mode : string -> xml -> ipv6_configuration_mode =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "none" -> `None
    | "dhcp" -> `DHCP
    | "static" -> `Static
    | "autoconf" -> `Autoconf
    | _ -> log_backtrace(); raise (RunTimeTypeError("ipv6_configuration_mode", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and primary_address_type : string -> xml -> primary_address_type =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "ipv4" -> `IPv4
    | "ipv6" -> `IPv6
    | _ -> log_backtrace(); raise (RunTimeTypeError("primary_address_type", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and pIF_metrics_t : string -> xml -> pIF_metrics_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { pIF_metrics_uuid = (string param)(my_assoc "uuid" all);
 pIF_metrics_io_read_kbs = (float param)(my_assoc "io_read_kbs" all);
 pIF_metrics_io_write_kbs = (float param)(my_assoc "io_write_kbs" all);
 pIF_metrics_carrier = (bool param)(my_assoc "carrier" all);
 pIF_metrics_vendor_id = (string param)(my_assoc "vendor_id" all);
 pIF_metrics_vendor_name = (string param)(my_assoc "vendor_name" all);
 pIF_metrics_device_id = (string param)(my_assoc "device_id" all);
 pIF_metrics_device_name = (string param)(my_assoc "device_name" all);
 pIF_metrics_speed = (int64 param)(my_assoc "speed" all);
 pIF_metrics_duplex = (bool param)(my_assoc "duplex" all);
 pIF_metrics_pci_bus_path = (string param)(my_assoc "pci_bus_path" all);
 pIF_metrics_last_updated = (datetime param)(my_assoc "last_updated" all);
 pIF_metrics_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PIF_metrics : string -> xml -> ref_PIF_metrics =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and bond_t : string -> xml -> bond_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { bond_uuid = (string param)(my_assoc "uuid" all);
 bond_master = (ref_PIF param)(if (List.mem_assoc "master" all) then (my_assoc "master" all) else Xml.parse_string ("<value/>"));
 bond_slaves = (ref_PIF_set param)(if (List.mem_assoc "slaves" all) then (my_assoc "slaves" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 bond_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>"));
 bond_primary_slave = (ref_PIF param)(if (List.mem_assoc "primary_slave" all) then (my_assoc "primary_slave" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 bond_mode = (bond_mode param)(if (List.mem_assoc "mode" all) then (my_assoc "mode" all) else Xml.parse_string ("<value>balance-slb</value>"));
 bond_properties = (string_to_string_map param)(if (List.mem_assoc "properties" all) then (my_assoc "properties" all) else Xml.parse_string ("<value><struct/></value>"));
 bond_links_up = (int64 param)(if (List.mem_assoc "links_up" all) then (my_assoc "links_up" all) else Xml.parse_string ("<value>0</value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PIF_set : string -> xml -> ref_PIF_set =
    fun param -> (fun xml -> try (set (ref_PIF param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and bond_mode : string -> xml -> bond_mode =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "balance-slb" -> `balanceslb
    | "active-backup" -> `activebackup
    | "lacp" -> `lacp
    | _ -> log_backtrace(); raise (RunTimeTypeError("bond_mode", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_Bond : string -> xml -> ref_Bond =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vLAN_t : string -> xml -> vLAN_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vLAN_uuid = (string param)(my_assoc "uuid" all);
 vLAN_tagged_PIF = (ref_PIF param)(if (List.mem_assoc "tagged_PIF" all) then (my_assoc "tagged_PIF" all) else Xml.parse_string ("<value/>"));
 vLAN_untagged_PIF = (ref_PIF param)(if (List.mem_assoc "untagged_PIF" all) then (my_assoc "untagged_PIF" all) else Xml.parse_string ("<value/>"));
 vLAN_tag = (int64 param)(if (List.mem_assoc "tag" all) then (my_assoc "tag" all) else Xml.parse_string ("<value>-1</value>"));
 vLAN_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VLAN : string -> xml -> ref_VLAN =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and sM_t : string -> xml -> sM_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { sM_uuid = (string param)(my_assoc "uuid" all);
 sM_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 sM_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 sM_type = (string param)(my_assoc "type" all);
 sM_vendor = (string param)(my_assoc "vendor" all);
 sM_copyright = (string param)(my_assoc "copyright" all);
 sM_version = (string param)(my_assoc "version" all);
 sM_required_api_version = (string param)(my_assoc "required_api_version" all);
 sM_configuration = (string_to_string_map param)(my_assoc "configuration" all);
 sM_capabilities = (string_set param)(if (List.mem_assoc "capabilities" all) then (my_assoc "capabilities" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 sM_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>"));
 sM_driver_filename = (string param)(if (List.mem_assoc "driver_filename" all) then (my_assoc "driver_filename" all) else Xml.parse_string ("<value/>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_SM : string -> xml -> ref_SM =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and sR_t : string -> xml -> sR_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { sR_uuid = (string param)(my_assoc "uuid" all);
 sR_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 sR_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 sR_allowed_operations = (storage_operations_set param)(my_assoc "allowed_operations" all);
 sR_current_operations = (string_to_storage_operations_map param)(my_assoc "current_operations" all);
 sR_VDIs = (ref_VDI_set param)(if (List.mem_assoc "VDIs" all) then (my_assoc "VDIs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 sR_PBDs = (ref_PBD_set param)(if (List.mem_assoc "PBDs" all) then (my_assoc "PBDs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 sR_virtual_allocation = (int64 param)(my_assoc "virtual_allocation" all);
 sR_physical_utilisation = (int64 param)(my_assoc "physical_utilisation" all);
 sR_physical_size = (int64 param)(my_assoc "physical_size" all);
 sR_type = (string param)(my_assoc "type" all);
 sR_content_type = (string param)(my_assoc "content_type" all);
 sR_shared = (bool param)(my_assoc "shared" all);
 sR_other_config = (string_to_string_map param)(my_assoc "other_config" all);
 sR_tags = (string_set param)(if (List.mem_assoc "tags" all) then (my_assoc "tags" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 sR_sm_config = (string_to_string_map param)(if (List.mem_assoc "sm_config" all) then (my_assoc "sm_config" all) else Xml.parse_string ("<value><struct/></value>"));
 sR_blobs = (string_to_ref_blob_map param)(if (List.mem_assoc "blobs" all) then (my_assoc "blobs" all) else Xml.parse_string ("<value><struct/></value>"));
 sR_local_cache_enabled = (bool param)(if (List.mem_assoc "local_cache_enabled" all) then (my_assoc "local_cache_enabled" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 sR_introduced_by = (ref_DR_task param)(if (List.mem_assoc "introduced_by" all) then (my_assoc "introduced_by" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and storage_operations_set : string -> xml -> storage_operations_set =
    fun param -> (fun xml -> try (set (storage_operations param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_storage_operations_map : string -> xml -> string_to_storage_operations_map =
    fun param -> (fun xml -> try (map (FromString.string) ((storage_operations param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and storage_operations : string -> xml -> storage_operations =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "scan" -> `scan
    | "destroy" -> `destroy
    | "forget" -> `forget
    | "plug" -> `plug
    | "unplug" -> `unplug
    | "update" -> `update
    | "vdi_create" -> `vdi_create
    | "vdi_introduce" -> `vdi_introduce
    | "vdi_destroy" -> `vdi_destroy
    | "vdi_resize" -> `vdi_resize
    | "vdi_clone" -> `vdi_clone
    | "vdi_snapshot" -> `vdi_snapshot
    | "pbd_create" -> `pbd_create
    | "pbd_destroy" -> `pbd_destroy
    | _ -> log_backtrace(); raise (RunTimeTypeError("storage_operations", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_ref_blob_map : string -> xml -> string_to_ref_blob_map =
    fun param -> (fun xml -> try (map (FromString.string) ((ref_blob param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_DR_task : string -> xml -> ref_DR_task =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vDI_t : string -> xml -> vDI_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vDI_uuid = (string param)(my_assoc "uuid" all);
 vDI_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 vDI_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 vDI_allowed_operations = (vdi_operations_set param)(my_assoc "allowed_operations" all);
 vDI_current_operations = (string_to_vdi_operations_map param)(my_assoc "current_operations" all);
 vDI_SR = (ref_SR param)(my_assoc "SR" all);
 vDI_VBDs = (ref_VBD_set param)(if (List.mem_assoc "VBDs" all) then (my_assoc "VBDs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vDI_crash_dumps = (ref_crashdump_set param)(if (List.mem_assoc "crash_dumps" all) then (my_assoc "crash_dumps" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vDI_virtual_size = (int64 param)(my_assoc "virtual_size" all);
 vDI_physical_utilisation = (int64 param)(my_assoc "physical_utilisation" all);
 vDI_type = (vdi_type param)(my_assoc "type" all);
 vDI_sharable = (bool param)(my_assoc "sharable" all);
 vDI_read_only = (bool param)(my_assoc "read_only" all);
 vDI_other_config = (string_to_string_map param)(my_assoc "other_config" all);
 vDI_storage_lock = (bool param)(my_assoc "storage_lock" all);
 vDI_location = (string param)(if (List.mem_assoc "location" all) then (my_assoc "location" all) else Xml.parse_string ("<value/>"));
 vDI_managed = (bool param)(my_assoc "managed" all);
 vDI_missing = (bool param)(my_assoc "missing" all);
 vDI_parent = (ref_VDI param)(my_assoc "parent" all);
 vDI_xenstore_data = (string_to_string_map param)(if (List.mem_assoc "xenstore_data" all) then (my_assoc "xenstore_data" all) else Xml.parse_string ("<value><struct/></value>"));
 vDI_sm_config = (string_to_string_map param)(if (List.mem_assoc "sm_config" all) then (my_assoc "sm_config" all) else Xml.parse_string ("<value><struct/></value>"));
 vDI_is_a_snapshot = (bool param)(if (List.mem_assoc "is_a_snapshot" all) then (my_assoc "is_a_snapshot" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 vDI_snapshot_of = (ref_VDI param)(if (List.mem_assoc "snapshot_of" all) then (my_assoc "snapshot_of" all) else Xml.parse_string ("<value/>"));
 vDI_snapshots = (ref_VDI_set param)(if (List.mem_assoc "snapshots" all) then (my_assoc "snapshots" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vDI_snapshot_time = (datetime param)(if (List.mem_assoc "snapshot_time" all) then (my_assoc "snapshot_time" all) else Xml.parse_string ("<value><dateTime.iso8601>19700101T00:00:00Z</dateTime.iso8601></value>"));
 vDI_tags = (string_set param)(if (List.mem_assoc "tags" all) then (my_assoc "tags" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 vDI_allow_caching = (bool param)(if (List.mem_assoc "allow_caching" all) then (my_assoc "allow_caching" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 vDI_on_boot = (on_boot param)(if (List.mem_assoc "on_boot" all) then (my_assoc "on_boot" all) else Xml.parse_string ("<value>persist</value>"));
 vDI_metadata_of_pool = (ref_pool param)(if (List.mem_assoc "metadata_of_pool" all) then (my_assoc "metadata_of_pool" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 vDI_metadata_latest = (bool param)(if (List.mem_assoc "metadata_latest" all) then (my_assoc "metadata_latest" all) else Xml.parse_string ("<value><boolean>0</boolean></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vdi_operations_set : string -> xml -> vdi_operations_set =
    fun param -> (fun xml -> try (set (vdi_operations param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_vdi_operations_map : string -> xml -> string_to_vdi_operations_map =
    fun param -> (fun xml -> try (map (FromString.string) ((vdi_operations param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vdi_operations : string -> xml -> vdi_operations =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "scan" -> `scan
    | "clone" -> `clone
    | "copy" -> `copy
    | "resize" -> `resize
    | "resize_online" -> `resize_online
    | "snapshot" -> `snapshot
    | "destroy" -> `destroy
    | "forget" -> `forget
    | "update" -> `update
    | "force_unlock" -> `force_unlock
    | "generate_config" -> `generate_config
    | "blocked" -> `blocked
    | _ -> log_backtrace(); raise (RunTimeTypeError("vdi_operations", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vdi_type : string -> xml -> vdi_type =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "system" -> `system
    | "user" -> `user
    | "ephemeral" -> `ephemeral
    | "suspend" -> `suspend
    | "crashdump" -> `crashdump
    | "ha_statefile" -> `ha_statefile
    | "metadata" -> `metadata
    | "redo_log" -> `redo_log
    | _ -> log_backtrace(); raise (RunTimeTypeError("vdi_type", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_pool : string -> xml -> ref_pool =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and on_boot : string -> xml -> on_boot =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "reset" -> `reset
    | "persist" -> `persist
    | _ -> log_backtrace(); raise (RunTimeTypeError("on_boot", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vBD_t : string -> xml -> vBD_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vBD_uuid = (string param)(my_assoc "uuid" all);
 vBD_allowed_operations = (vbd_operations_set param)(my_assoc "allowed_operations" all);
 vBD_current_operations = (string_to_vbd_operations_map param)(my_assoc "current_operations" all);
 vBD_VM = (ref_VM param)(my_assoc "VM" all);
 vBD_VDI = (ref_VDI param)(my_assoc "VDI" all);
 vBD_device = (string param)(my_assoc "device" all);
 vBD_userdevice = (string param)(my_assoc "userdevice" all);
 vBD_bootable = (bool param)(my_assoc "bootable" all);
 vBD_mode = (vbd_mode param)(my_assoc "mode" all);
 vBD_type = (vbd_type param)(my_assoc "type" all);
 vBD_unpluggable = (bool param)(if (List.mem_assoc "unpluggable" all) then (my_assoc "unpluggable" all) else Xml.parse_string ("<value><boolean>1</boolean></value>"));
 vBD_storage_lock = (bool param)(my_assoc "storage_lock" all);
 vBD_empty = (bool param)(my_assoc "empty" all);
 vBD_other_config = (string_to_string_map param)(my_assoc "other_config" all);
 vBD_currently_attached = (bool param)(my_assoc "currently_attached" all);
 vBD_status_code = (int64 param)(my_assoc "status_code" all);
 vBD_status_detail = (string param)(my_assoc "status_detail" all);
 vBD_runtime_properties = (string_to_string_map param)(my_assoc "runtime_properties" all);
 vBD_qos_algorithm_type = (string param)(my_assoc "qos_algorithm_type" all);
 vBD_qos_algorithm_params = (string_to_string_map param)(my_assoc "qos_algorithm_params" all);
 vBD_qos_supported_algorithms = (string_set param)(my_assoc "qos_supported_algorithms" all);
 vBD_metrics = (ref_VBD_metrics param)(my_assoc "metrics" all) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vbd_operations_set : string -> xml -> vbd_operations_set =
    fun param -> (fun xml -> try (set (vbd_operations param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_vbd_operations_map : string -> xml -> string_to_vbd_operations_map =
    fun param -> (fun xml -> try (map (FromString.string) ((vbd_operations param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vbd_operations : string -> xml -> vbd_operations =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "attach" -> `attach
    | "eject" -> `eject
    | "insert" -> `insert
    | "plug" -> `plug
    | "unplug" -> `unplug
    | "unplug_force" -> `unplug_force
    | "pause" -> `pause
    | "unpause" -> `unpause
    | _ -> log_backtrace(); raise (RunTimeTypeError("vbd_operations", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vbd_mode : string -> xml -> vbd_mode =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "ro" -> `RO
    | "rw" -> `RW
    | _ -> log_backtrace(); raise (RunTimeTypeError("vbd_mode", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vbd_type : string -> xml -> vbd_type =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "cd" -> `CD
    | "disk" -> `Disk
    | _ -> log_backtrace(); raise (RunTimeTypeError("vbd_type", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VBD : string -> xml -> ref_VBD =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vBD_metrics_t : string -> xml -> vBD_metrics_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vBD_metrics_uuid = (string param)(my_assoc "uuid" all);
 vBD_metrics_io_read_kbs = (float param)(my_assoc "io_read_kbs" all);
 vBD_metrics_io_write_kbs = (float param)(my_assoc "io_write_kbs" all);
 vBD_metrics_last_updated = (datetime param)(my_assoc "last_updated" all);
 vBD_metrics_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and float : string -> xml -> float =
    fun param -> (fun xml -> try (From.double xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VBD_metrics : string -> xml -> ref_VBD_metrics =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and pBD_t : string -> xml -> pBD_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { pBD_uuid = (string param)(my_assoc "uuid" all);
 pBD_host = (ref_host param)(my_assoc "host" all);
 pBD_SR = (ref_SR param)(my_assoc "SR" all);
 pBD_device_config = (string_to_string_map param)(my_assoc "device_config" all);
 pBD_currently_attached = (bool param)(my_assoc "currently_attached" all);
 pBD_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_SR : string -> xml -> ref_SR =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PBD : string -> xml -> ref_PBD =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and crashdump_t : string -> xml -> crashdump_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { crashdump_uuid = (string param)(my_assoc "uuid" all);
 crashdump_VM = (ref_VM param)(my_assoc "VM" all);
 crashdump_VDI = (ref_VDI param)(my_assoc "VDI" all);
 crashdump_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VDI : string -> xml -> ref_VDI =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_crashdump : string -> xml -> ref_crashdump =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vTPM_t : string -> xml -> vTPM_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vTPM_uuid = (string param)(my_assoc "uuid" all);
 vTPM_VM = (ref_VM param)(my_assoc "VM" all);
 vTPM_backend = (ref_VM param)(my_assoc "backend" all) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VTPM : string -> xml -> ref_VTPM =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and console_t : string -> xml -> console_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { console_uuid = (string param)(my_assoc "uuid" all);
 console_protocol = (console_protocol param)(my_assoc "protocol" all);
 console_location = (string param)(my_assoc "location" all);
 console_VM = (ref_VM param)(my_assoc "VM" all);
 console_other_config = (string_to_string_map param)(my_assoc "other_config" all) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and console_protocol : string -> xml -> console_protocol =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "vt100" -> `vt100
    | "rfb" -> `rfb
    | "rdp" -> `rdp
    | _ -> log_backtrace(); raise (RunTimeTypeError("console_protocol", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_console : string -> xml -> ref_console =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and user_t : string -> xml -> user_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { user_uuid = (string param)(my_assoc "uuid" all);
 user_short_name = (string param)(my_assoc "short_name" all);
 user_fullname = (string param)(my_assoc "fullname" all);
 user_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_user : string -> xml -> ref_user =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and blob_t : string -> xml -> blob_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { blob_uuid = (string param)(my_assoc "uuid" all);
 blob_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 blob_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 blob_size = (int64 param)(my_assoc "size" all);
 blob_public = (bool param)(if (List.mem_assoc "public" all) then (my_assoc "public" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 blob_last_updated = (datetime param)(my_assoc "last_updated" all);
 blob_mime_type = (string param)(my_assoc "mime_type" all) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_blob : string -> xml -> ref_blob =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and int64 : string -> xml -> int64 =
    fun param -> (fun xml -> try (Int64.of_string(From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and cls : string -> xml -> cls =
    fun param -> (fun xml -> try (
    match String.lowercase (From.string xml) with
      "vm" -> `VM
    | "host" -> `Host
    | "sr" -> `SR
    | "pool" -> `Pool
    | "vmpp" -> `VMPP
    | _ -> log_backtrace(); raise (RunTimeTypeError("cls", xml))) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and datetime : string -> xml -> datetime =
    fun param -> (fun xml -> try (From.datetime xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_message : string -> xml -> ref_message =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and secret_t : string -> xml -> secret_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { secret_uuid = (string param)(my_assoc "uuid" all);
 secret_value = (string param)(my_assoc "value" all);
 secret_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_secret : string -> xml -> ref_secret =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and tunnel_t : string -> xml -> tunnel_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { tunnel_uuid = (string param)(my_assoc "uuid" all);
 tunnel_access_PIF = (ref_PIF param)(if (List.mem_assoc "access_PIF" all) then (my_assoc "access_PIF" all) else Xml.parse_string ("<value/>"));
 tunnel_transport_PIF = (ref_PIF param)(if (List.mem_assoc "transport_PIF" all) then (my_assoc "transport_PIF" all) else Xml.parse_string ("<value/>"));
 tunnel_status = (string_to_string_map param)(if (List.mem_assoc "status" all) then (my_assoc "status" all) else Xml.parse_string ("<value><struct><member><name>active</name><value>false</value></member></struct></value>"));
 tunnel_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PIF : string -> xml -> ref_PIF =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_network : string -> xml -> ref_network =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_tunnel : string -> xml -> ref_tunnel =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and pCI_t : string -> xml -> pCI_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { pCI_uuid = (string param)(my_assoc "uuid" all);
 pCI_class_name = (string param)(if (List.mem_assoc "class_name" all) then (my_assoc "class_name" all) else Xml.parse_string ("<value/>"));
 pCI_vendor_name = (string param)(if (List.mem_assoc "vendor_name" all) then (my_assoc "vendor_name" all) else Xml.parse_string ("<value/>"));
 pCI_device_name = (string param)(if (List.mem_assoc "device_name" all) then (my_assoc "device_name" all) else Xml.parse_string ("<value/>"));
 pCI_host = (ref_host param)(if (List.mem_assoc "host" all) then (my_assoc "host" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 pCI_pci_id = (string param)(if (List.mem_assoc "pci_id" all) then (my_assoc "pci_id" all) else Xml.parse_string ("<value/>"));
 pCI_dependencies = (ref_PCI_set param)(if (List.mem_assoc "dependencies" all) then (my_assoc "dependencies" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 pCI_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PCI_set : string -> xml -> ref_PCI_set =
    fun param -> (fun xml -> try (set (ref_PCI param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and pGPU_t : string -> xml -> pGPU_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { pGPU_uuid = (string param)(my_assoc "uuid" all);
 pGPU_PCI = (ref_PCI param)(if (List.mem_assoc "PCI" all) then (my_assoc "PCI" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 pGPU_GPU_group = (ref_GPU_group param)(if (List.mem_assoc "GPU_group" all) then (my_assoc "GPU_group" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 pGPU_host = (ref_host param)(if (List.mem_assoc "host" all) then (my_assoc "host" all) else Xml.parse_string ("<value>OpaqueRef:NULL</value>"));
 pGPU_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PCI : string -> xml -> ref_PCI =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_host : string -> xml -> ref_host =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_PGPU : string -> xml -> ref_PGPU =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and gPU_group_t : string -> xml -> gPU_group_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { gPU_group_uuid = (string param)(my_assoc "uuid" all);
 gPU_group_name_label = (string param)(if (List.mem_assoc "name_label" all) then (my_assoc "name_label" all) else Xml.parse_string ("<value/>"));
 gPU_group_name_description = (string param)(if (List.mem_assoc "name_description" all) then (my_assoc "name_description" all) else Xml.parse_string ("<value/>"));
 gPU_group_PGPUs = (ref_PGPU_set param)(if (List.mem_assoc "PGPUs" all) then (my_assoc "PGPUs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 gPU_group_VGPUs = (ref_VGPU_set param)(if (List.mem_assoc "VGPUs" all) then (my_assoc "VGPUs" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 gPU_group_GPU_types = (string_set param)(if (List.mem_assoc "GPU_types" all) then (my_assoc "GPU_types" all) else Xml.parse_string ("<value><array><data/></array></value>"));
 gPU_group_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_set : string -> xml -> string_set =
    fun param -> (fun xml -> try (set (string param) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and vGPU_t : string -> xml -> vGPU_t =
    fun param -> (fun xml -> try (let all = From.structure xml in { vGPU_uuid = (string param)(my_assoc "uuid" all);
 vGPU_VM = (ref_VM param)(my_assoc "VM" all);
 vGPU_GPU_group = (ref_GPU_group param)(my_assoc "GPU_group" all);
 vGPU_device = (string param)(if (List.mem_assoc "device" all) then (my_assoc "device" all) else Xml.parse_string ("<value>0</value>"));
 vGPU_currently_attached = (bool param)(if (List.mem_assoc "currently_attached" all) then (my_assoc "currently_attached" all) else Xml.parse_string ("<value><boolean>0</boolean></value>"));
 vGPU_other_config = (string_to_string_map param)(if (List.mem_assoc "other_config" all) then (my_assoc "other_config" all) else Xml.parse_string ("<value><struct/></value>")) }) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and bool : string -> xml -> bool =
    fun param -> (fun xml -> try (From.boolean xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VM : string -> xml -> ref_VM =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_GPU_group : string -> xml -> ref_GPU_group =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string_to_string_map : string -> xml -> string_to_string_map =
    fun param -> (fun xml -> try (map (FromString.string) ((string param)) xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and ref_VGPU : string -> xml -> ref_VGPU =
    fun param -> (fun xml -> try (Ref.of_string (From.string xml)) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
  
  and string : string -> xml -> string =
    fun param -> (fun xml -> try (From.string xml) with _ -> log_backtrace (); raise (Api_errors.Server_error (Api_errors.field_type_error,[param])))
end

module To = struct
  open Xml
  
  let methodCall = To.methodCall
  
  let methodResponse f x = To.methodResponse (f x)
  
  let tostring_reference = Ref.string_of
  
  let set f l =
    To.array (List.map f l)
  
  let map fk fv m =
    let elements = List.map (fun (k, v) -> fk k, fv v) m in
    XMLRPC.To.structure elements
  
  let structure = To.structure
  
  let rec unused' = ()
  
  and ref_session_set : ref_session_set -> xml =
    fun s -> set ref_session s
  
  and ref_auth_set : ref_auth_set -> xml =
    fun s -> set ref_auth s
  
  and ref_auth : ref_auth -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_event_set : ref_event_set -> xml =
    fun s -> set ref_event s
  
  and ref_event : ref_event -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_user_set : ref_user_set -> xml =
    fun s -> set ref_user s
  
  and ref_data_source_set : ref_data_source_set -> xml =
    fun s -> set ref_data_source s
  
  and ref_data_source : ref_data_source -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and event_operation : event_operation -> xml =
        fun v -> To.string(match v with
                         `add -> "add"
                       | `del -> "del"
                       | `_mod -> "mod")
  
  and ref_VGPU_to_vGPU_t_map : ref_VGPU_to_vGPU_t_map -> xml =
    fun m -> map (tostring_reference) (vGPU_t) m
  
  and ref_GPU_group_to_gPU_group_t_map : ref_GPU_group_to_gPU_group_t_map -> xml =
    fun m -> map (tostring_reference) (gPU_group_t) m
  
  and ref_GPU_group_set : ref_GPU_group_set -> xml =
    fun s -> set ref_GPU_group s
  
  and ref_PGPU_to_pGPU_t_map : ref_PGPU_to_pGPU_t_map -> xml =
    fun m -> map (tostring_reference) (pGPU_t) m
  
  and ref_PCI_to_pCI_t_map : ref_PCI_to_pCI_t_map -> xml =
    fun m -> map (tostring_reference) (pCI_t) m
  
  and ref_tunnel_to_tunnel_t_map : ref_tunnel_to_tunnel_t_map -> xml =
    fun m -> map (tostring_reference) (tunnel_t) m
  
  and ref_secret_to_secret_t_map : ref_secret_to_secret_t_map -> xml =
    fun m -> map (tostring_reference) (secret_t) m
  
  and ref_secret_set : ref_secret_set -> xml =
    fun s -> set ref_secret s
  
  and ref_message_set : ref_message_set -> xml =
    fun s -> set ref_message s
  
  and ref_message_to_message_t_map : ref_message_to_message_t_map -> xml =
    fun m -> map (tostring_reference) (message_t) m
  
  and message_t : message_t -> xml =
    fun x -> To.structure [ "uuid", string x.message_uuid; "name", string x.message_name; "priority", int64 x.message_priority; "cls", cls x.message_cls; "obj_uuid", string x.message_obj_uuid; "timestamp", datetime x.message_timestamp; "body", string x.message_body ]
  
  and ref_blob_to_blob_t_map : ref_blob_to_blob_t_map -> xml =
    fun m -> map (tostring_reference) (blob_t) m
  
  and ref_blob_set : ref_blob_set -> xml =
    fun s -> set ref_blob s
  
  and ref_console_to_console_t_map : ref_console_to_console_t_map -> xml =
    fun m -> map (tostring_reference) (console_t) m
  
  and ref_crashdump_to_crashdump_t_map : ref_crashdump_to_crashdump_t_map -> xml =
    fun m -> map (tostring_reference) (crashdump_t) m
  
  and ref_PBD_to_pBD_t_map : ref_PBD_to_pBD_t_map -> xml =
    fun m -> map (tostring_reference) (pBD_t) m
  
  and ref_VBD_metrics_to_vBD_metrics_t_map : ref_VBD_metrics_to_vBD_metrics_t_map -> xml =
    fun m -> map (tostring_reference) (vBD_metrics_t) m
  
  and ref_VBD_metrics_set : ref_VBD_metrics_set -> xml =
    fun s -> set ref_VBD_metrics s
  
  and ref_VBD_to_vBD_t_map : ref_VBD_to_vBD_t_map -> xml =
    fun m -> map (tostring_reference) (vBD_t) m
  
  and ref_VDI_to_vDI_t_map : ref_VDI_to_vDI_t_map -> xml =
    fun m -> map (tostring_reference) (vDI_t) m
  
  and ref_SR_to_sR_t_map : ref_SR_to_sR_t_map -> xml =
    fun m -> map (tostring_reference) (sR_t) m
  
  and ref_SM_to_sM_t_map : ref_SM_to_sM_t_map -> xml =
    fun m -> map (tostring_reference) (sM_t) m
  
  and ref_SM_set : ref_SM_set -> xml =
    fun s -> set ref_SM s
  
  and ref_VLAN_to_vLAN_t_map : ref_VLAN_to_vLAN_t_map -> xml =
    fun m -> map (tostring_reference) (vLAN_t) m
  
  and ref_Bond_to_bond_t_map : ref_Bond_to_bond_t_map -> xml =
    fun m -> map (tostring_reference) (bond_t) m
  
  and ref_PIF_metrics_to_pIF_metrics_t_map : ref_PIF_metrics_to_pIF_metrics_t_map -> xml =
    fun m -> map (tostring_reference) (pIF_metrics_t) m
  
  and ref_PIF_metrics_set : ref_PIF_metrics_set -> xml =
    fun s -> set ref_PIF_metrics s
  
  and ref_PIF_to_pIF_t_map : ref_PIF_to_pIF_t_map -> xml =
    fun m -> map (tostring_reference) (pIF_t) m
  
  and ref_tunnel_set : ref_tunnel_set -> xml =
    fun s -> set ref_tunnel s
  
  and ref_VLAN_set : ref_VLAN_set -> xml =
    fun s -> set ref_VLAN s
  
  and ref_Bond_set : ref_Bond_set -> xml =
    fun s -> set ref_Bond s
  
  and ref_VIF_metrics_to_vIF_metrics_t_map : ref_VIF_metrics_to_vIF_metrics_t_map -> xml =
    fun m -> map (tostring_reference) (vIF_metrics_t) m
  
  and ref_VIF_metrics_set : ref_VIF_metrics_set -> xml =
    fun s -> set ref_VIF_metrics s
  
  and ref_VIF_to_vIF_t_map : ref_VIF_to_vIF_t_map -> xml =
    fun m -> map (tostring_reference) (vIF_t) m
  
  and ref_network_to_network_t_map : ref_network_to_network_t_map -> xml =
    fun m -> map (tostring_reference) (network_t) m
  
  and ref_network_set : ref_network_set -> xml =
    fun s -> set ref_network s
  
  and ref_host_cpu_to_host_cpu_t_map : ref_host_cpu_to_host_cpu_t_map -> xml =
    fun m -> map (tostring_reference) (host_cpu_t) m
  
  and ref_host_metrics_to_host_metrics_t_map : ref_host_metrics_to_host_metrics_t_map -> xml =
    fun m -> map (tostring_reference) (host_metrics_t) m
  
  and ref_host_metrics_set : ref_host_metrics_set -> xml =
    fun s -> set ref_host_metrics s
  
  and ref_host_patch_to_host_patch_t_map : ref_host_patch_to_host_patch_t_map -> xml =
    fun m -> map (tostring_reference) (host_patch_t) m
  
  and ref_host_crashdump_to_host_crashdump_t_map : ref_host_crashdump_to_host_crashdump_t_map -> xml =
    fun m -> map (tostring_reference) (host_crashdump_t) m
  
  and ref_host_to_host_t_map : ref_host_to_host_t_map -> xml =
    fun m -> map (tostring_reference) (host_t) m
  
  and ref_PGPU_set : ref_PGPU_set -> xml =
    fun s -> set ref_PGPU s
  
  and ref_host_cpu_set : ref_host_cpu_set -> xml =
    fun s -> set ref_host_cpu s
  
  and ref_PBD_set : ref_PBD_set -> xml =
    fun s -> set ref_PBD s
  
  and ref_host_crashdump_set : ref_host_crashdump_set -> xml =
    fun s -> set ref_host_crashdump s
  
  and ref_DR_task_to_dR_task_t_map : ref_DR_task_to_dR_task_t_map -> xml =
    fun m -> map (tostring_reference) (dR_task_t) m
  
  and ref_DR_task_set : ref_DR_task_set -> xml =
    fun s -> set ref_DR_task s
  
  and ref_VM_appliance_to_vM_appliance_t_map : ref_VM_appliance_to_vM_appliance_t_map -> xml =
    fun m -> map (tostring_reference) (vM_appliance_t) m
  
  and ref_VM_appliance_set : ref_VM_appliance_set -> xml =
    fun s -> set ref_VM_appliance s
  
  and ref_VMPP_to_vMPP_t_map : ref_VMPP_to_vMPP_t_map -> xml =
    fun m -> map (tostring_reference) (vMPP_t) m
  
  and ref_VMPP_set : ref_VMPP_set -> xml =
    fun s -> set ref_VMPP s
  
  and ref_VM_guest_metrics_to_vM_guest_metrics_t_map : ref_VM_guest_metrics_to_vM_guest_metrics_t_map -> xml =
    fun m -> map (tostring_reference) (vM_guest_metrics_t) m
  
  and ref_VM_guest_metrics_set : ref_VM_guest_metrics_set -> xml =
    fun s -> set ref_VM_guest_metrics s
  
  and ref_VM_metrics_to_vM_metrics_t_map : ref_VM_metrics_to_vM_metrics_t_map -> xml =
    fun m -> map (tostring_reference) (vM_metrics_t) m
  
  and ref_VM_metrics_set : ref_VM_metrics_set -> xml =
    fun s -> set ref_VM_metrics s
  
  and ref_VM_to_vM_t_map : ref_VM_to_vM_t_map -> xml =
    fun m -> map (tostring_reference) (vM_t) m
  
  and ref_host_to_string_set_map : ref_host_to_string_set_map -> xml =
    fun m -> map (tostring_reference) (string_set) m
  
  and data_source_t_set : data_source_t_set -> xml =
    fun s -> set data_source_t s
  
  and data_source_t : data_source_t -> xml =
    fun x -> To.structure [ "name_label", string x.data_source_name_label; "name_description", string x.data_source_name_description; "enabled", bool x.data_source_enabled; "standard", bool x.data_source_standard; "units", string x.data_source_units; "min", float x.data_source_min; "max", float x.data_source_max; "value", float x.data_source_value ]
  
  and ref_VGPU_set : ref_VGPU_set -> xml =
    fun s -> set ref_VGPU s
  
  and ref_VTPM_set : ref_VTPM_set -> xml =
    fun s -> set ref_VTPM s
  
  and ref_crashdump_set : ref_crashdump_set -> xml =
    fun s -> set ref_crashdump s
  
  and ref_VBD_set : ref_VBD_set -> xml =
    fun s -> set ref_VBD s
  
  and ref_VIF_set : ref_VIF_set -> xml =
    fun s -> set ref_VIF s
  
  and ref_console_set : ref_console_set -> xml =
    fun s -> set ref_console s
  
  and ref_pool_patch_to_pool_patch_t_map : ref_pool_patch_to_pool_patch_t_map -> xml =
    fun m -> map (tostring_reference) (pool_patch_t) m
  
  and ref_host_patch_set : ref_host_patch_set -> xml =
    fun s -> set ref_host_patch s
  
  and ref_pool_patch_set : ref_pool_patch_set -> xml =
    fun s -> set ref_pool_patch s
  
  and ref_pool_to_pool_t_map : ref_pool_to_pool_t_map -> xml =
    fun m -> map (tostring_reference) (pool_t) m
  
  and ref_pool_set : ref_pool_set -> xml =
    fun s -> set ref_pool s
  
  and ref_VM_to_string_set_map : ref_VM_to_string_set_map -> xml =
    fun m -> map (tostring_reference) (string_set) m
  
  and ref_VM_to_string_to_string_map_map : ref_VM_to_string_to_string_map_map -> xml =
    fun m -> map (tostring_reference) (string_to_string_map) m
  
  and hello_return : hello_return -> xml =
        fun v -> To.string(match v with
                         `ok -> "ok"
                       | `unknown_host -> "unknown_host"
                       | `cannot_talk_back -> "cannot_talk_back")
  
  and event_t_set : event_t_set -> xml =
    fun s -> set event_t s
  
  and event_t : event_t -> xml =
    fun x -> To.structure [ "id", int64 x.event_id; "timestamp", datetime x.event_timestamp; "class", string x.event_class; "operation", event_operation x.event_operation; "ref", string x.event_ref; "obj_uuid", string x.event_obj_uuid ]
  
  and ref_task_to_task_t_map : ref_task_to_task_t_map -> xml =
    fun m -> map (tostring_reference) (task_t) m
  
  and ref_role_to_role_t_map : ref_role_to_role_t_map -> xml =
    fun m -> map (tostring_reference) (role_t) m
  
  and ref_subject_to_subject_t_map : ref_subject_to_subject_t_map -> xml =
    fun m -> map (tostring_reference) (subject_t) m
  
  and ref_subject_set : ref_subject_set -> xml =
    fun s -> set ref_subject s
  
  and ref_task_set : ref_task_set -> xml =
    fun s -> set ref_task s
  
  and session_t : session_t -> xml =
    fun x -> To.structure [ "uuid", string x.session_uuid; "this_host", ref_host x.session_this_host; "this_user", ref_user x.session_this_user; "last_active", datetime x.session_last_active; "pool", bool x.session_pool; "other_config", string_to_string_map x.session_other_config; "is_local_superuser", bool x.session_is_local_superuser; "subject", ref_subject x.session_subject; "validation_time", datetime x.session_validation_time; "auth_user_sid", string x.session_auth_user_sid; "auth_user_name", string x.session_auth_user_name; "rbac_permissions", string_set x.session_rbac_permissions; "tasks", ref_task_set x.session_tasks; "parent", ref_session x.session_parent ]
  
  and subject_t : subject_t -> xml =
    fun x -> To.structure [ "uuid", string x.subject_uuid; "subject_identifier", string x.subject_subject_identifier; "other_config", string_to_string_map x.subject_other_config; "roles", ref_role_set x.subject_roles ]
  
  and ref_subject : ref_subject -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and role_t : role_t -> xml =
    fun x -> To.structure [ "uuid", string x.role_uuid; "name_label", string x.role_name_label; "name_description", string x.role_name_description; "subroles", ref_role_set x.role_subroles ]
  
  and ref_role_set : ref_role_set -> xml =
    fun s -> set ref_role s
  
  and ref_role : ref_role -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and task_t : task_t -> xml =
    fun x -> To.structure [ "uuid", string x.task_uuid; "name_label", string x.task_name_label; "name_description", string x.task_name_description; "allowed_operations", task_allowed_operations_set x.task_allowed_operations; "current_operations", string_to_task_allowed_operations_map x.task_current_operations; "created", datetime x.task_created; "finished", datetime x.task_finished; "status", task_status_type x.task_status; "resident_on", ref_host x.task_resident_on; "progress", float x.task_progress; "type", string x.task_type; "result", string x.task_result; "error_info", string_set x.task_error_info; "other_config", string_to_string_map x.task_other_config; "subtask_of", ref_task x.task_subtask_of; "subtasks", ref_task_set x.task_subtasks ]
  
  and task_allowed_operations_set : task_allowed_operations_set -> xml =
    fun s -> set task_allowed_operations s
  
  and string_to_task_allowed_operations_map : string_to_task_allowed_operations_map -> xml =
    fun m -> map (ToString.string) (task_allowed_operations) m
  
  and task_allowed_operations : task_allowed_operations -> xml =
        fun v -> To.string(match v with
                         `cancel -> "cancel")
  
  and task_status_type : task_status_type -> xml =
        fun v -> To.string(match v with
                         `pending -> "pending"
                       | `success -> "success"
                       | `failure -> "failure"
                       | `cancelling -> "cancelling"
                       | `cancelled -> "cancelled")
  
  and ref_task : ref_task -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and pool_t : pool_t -> xml =
    fun x -> To.structure [ "uuid", string x.pool_uuid; "name_label", string x.pool_name_label; "name_description", string x.pool_name_description; "master", ref_host x.pool_master; "default_SR", ref_SR x.pool_default_SR; "suspend_image_SR", ref_SR x.pool_suspend_image_SR; "crash_dump_SR", ref_SR x.pool_crash_dump_SR; "other_config", string_to_string_map x.pool_other_config; "ha_enabled", bool x.pool_ha_enabled; "ha_configuration", string_to_string_map x.pool_ha_configuration; "ha_statefiles", string_set x.pool_ha_statefiles; "ha_host_failures_to_tolerate", int64 x.pool_ha_host_failures_to_tolerate; "ha_plan_exists_for", int64 x.pool_ha_plan_exists_for; "ha_allow_overcommit", bool x.pool_ha_allow_overcommit; "ha_overcommitted", bool x.pool_ha_overcommitted; "blobs", string_to_ref_blob_map x.pool_blobs; "tags", string_set x.pool_tags; "gui_config", string_to_string_map x.pool_gui_config; "wlb_url", string x.pool_wlb_url; "wlb_username", string x.pool_wlb_username; "wlb_enabled", bool x.pool_wlb_enabled; "wlb_verify_cert", bool x.pool_wlb_verify_cert; "redo_log_enabled", bool x.pool_redo_log_enabled; "redo_log_vdi", ref_VDI x.pool_redo_log_vdi; "vswitch_controller", string x.pool_vswitch_controller; "restrictions", string_to_string_map x.pool_restrictions; "metadata_VDIs", ref_VDI_set x.pool_metadata_VDIs ]
  
  and string_to_int64_map : string_to_int64_map -> xml =
    fun m -> map (ToString.string) (int64) m
  
  and ref_SR_set : ref_SR_set -> xml =
    fun s -> set ref_SR s
  
  and ref_VM_to_string_map : ref_VM_to_string_map -> xml =
    fun m -> map (tostring_reference) (string) m
  
  and ref_host_set : ref_host_set -> xml =
    fun s -> set ref_host s
  
  and ref_VM_set : ref_VM_set -> xml =
    fun s -> set ref_VM s
  
  and pool_patch_t : pool_patch_t -> xml =
    fun x -> To.structure [ "uuid", string x.pool_patch_uuid; "name_label", string x.pool_patch_name_label; "name_description", string x.pool_patch_name_description; "version", string x.pool_patch_version; "size", int64 x.pool_patch_size; "pool_applied", bool x.pool_patch_pool_applied; "host_patches", ref_host_patch_set x.pool_patch_host_patches; "after_apply_guidance", after_apply_guidance_set x.pool_patch_after_apply_guidance; "other_config", string_to_string_map x.pool_patch_other_config ]
  
  and after_apply_guidance_set : after_apply_guidance_set -> xml =
    fun s -> set after_apply_guidance s
  
  and after_apply_guidance : after_apply_guidance -> xml =
        fun v -> To.string(match v with
                         `restartHVM -> "restartHVM"
                       | `restartPV -> "restartPV"
                       | `restartHost -> "restartHost"
                       | `restartXAPI -> "restartXAPI")
  
  and vM_t : vM_t -> xml =
    fun x -> To.structure [ "uuid", string x.vM_uuid; "allowed_operations", vm_operations_set x.vM_allowed_operations; "current_operations", string_to_vm_operations_map x.vM_current_operations; "power_state", vm_power_state x.vM_power_state; "name_label", string x.vM_name_label; "name_description", string x.vM_name_description; "user_version", int64 x.vM_user_version; "is_a_template", bool x.vM_is_a_template; "suspend_VDI", ref_VDI x.vM_suspend_VDI; "resident_on", ref_host x.vM_resident_on; "affinity", ref_host x.vM_affinity; "memory_overhead", int64 x.vM_memory_overhead; "memory_target", int64 x.vM_memory_target; "memory_static_max", int64 x.vM_memory_static_max; "memory_dynamic_max", int64 x.vM_memory_dynamic_max; "memory_dynamic_min", int64 x.vM_memory_dynamic_min; "memory_static_min", int64 x.vM_memory_static_min; "VCPUs_params", string_to_string_map x.vM_VCPUs_params; "VCPUs_max", int64 x.vM_VCPUs_max; "VCPUs_at_startup", int64 x.vM_VCPUs_at_startup; "actions_after_shutdown", on_normal_exit x.vM_actions_after_shutdown; "actions_after_reboot", on_normal_exit x.vM_actions_after_reboot; "actions_after_crash", on_crash_behaviour x.vM_actions_after_crash; "consoles", ref_console_set x.vM_consoles; "VIFs", ref_VIF_set x.vM_VIFs; "VBDs", ref_VBD_set x.vM_VBDs; "crash_dumps", ref_crashdump_set x.vM_crash_dumps; "VTPMs", ref_VTPM_set x.vM_VTPMs; "PV_bootloader", string x.vM_PV_bootloader; "PV_kernel", string x.vM_PV_kernel; "PV_ramdisk", string x.vM_PV_ramdisk; "PV_args", string x.vM_PV_args; "PV_bootloader_args", string x.vM_PV_bootloader_args; "PV_legacy_args", string x.vM_PV_legacy_args; "HVM_boot_policy", string x.vM_HVM_boot_policy; "HVM_boot_params", string_to_string_map x.vM_HVM_boot_params; "HVM_shadow_multiplier", float x.vM_HVM_shadow_multiplier; "platform", string_to_string_map x.vM_platform; "PCI_bus", string x.vM_PCI_bus; "other_config", string_to_string_map x.vM_other_config; "domid", int64 x.vM_domid; "domarch", string x.vM_domarch; "last_boot_CPU_flags", string_to_string_map x.vM_last_boot_CPU_flags; "is_control_domain", bool x.vM_is_control_domain; "metrics", ref_VM_metrics x.vM_metrics; "guest_metrics", ref_VM_guest_metrics x.vM_guest_metrics; "last_booted_record", string x.vM_last_booted_record; "recommendations", string x.vM_recommendations; "xenstore_data", string_to_string_map x.vM_xenstore_data; "ha_always_run", bool x.vM_ha_always_run; "ha_restart_priority", string x.vM_ha_restart_priority; "is_a_snapshot", bool x.vM_is_a_snapshot; "snapshot_of", ref_VM x.vM_snapshot_of; "snapshots", ref_VM_set x.vM_snapshots; "snapshot_time", datetime x.vM_snapshot_time; "transportable_snapshot_id", string x.vM_transportable_snapshot_id; "blobs", string_to_ref_blob_map x.vM_blobs; "tags", string_set x.vM_tags; "blocked_operations", vm_operations_to_string_map x.vM_blocked_operations; "snapshot_info", string_to_string_map x.vM_snapshot_info; "snapshot_metadata", string x.vM_snapshot_metadata; "parent", ref_VM x.vM_parent; "children", ref_VM_set x.vM_children; "bios_strings", string_to_string_map x.vM_bios_strings; "protection_policy", ref_VMPP x.vM_protection_policy; "is_snapshot_from_vmpp", bool x.vM_is_snapshot_from_vmpp; "appliance", ref_VM_appliance x.vM_appliance; "start_delay", int64 x.vM_start_delay; "shutdown_delay", int64 x.vM_shutdown_delay; "order", int64 x.vM_order; "VGPUs", ref_VGPU_set x.vM_VGPUs; "attached_PCIs", ref_PCI_set x.vM_attached_PCIs; "suspend_SR", ref_SR x.vM_suspend_SR; "version", int64 x.vM_version ]
  
  and vm_operations_set : vm_operations_set -> xml =
    fun s -> set vm_operations s
  
  and string_to_vm_operations_map : string_to_vm_operations_map -> xml =
    fun m -> map (ToString.string) (vm_operations) m
  
  and vm_power_state : vm_power_state -> xml =
        fun v -> To.string(match v with
                         `Halted -> "Halted"
                       | `Paused -> "Paused"
                       | `Running -> "Running"
                       | `Suspended -> "Suspended")
  
  and on_normal_exit : on_normal_exit -> xml =
        fun v -> To.string(match v with
                         `destroy -> "destroy"
                       | `restart -> "restart")
  
  and on_crash_behaviour : on_crash_behaviour -> xml =
        fun v -> To.string(match v with
                         `destroy -> "destroy"
                       | `coredump_and_destroy -> "coredump_and_destroy"
                       | `restart -> "restart"
                       | `coredump_and_restart -> "coredump_and_restart"
                       | `preserve -> "preserve"
                       | `rename_restart -> "rename_restart")
  
  and vm_operations_to_string_map : vm_operations_to_string_map -> xml =
    fun m -> map (   function `snapshot -> "snapshot"
                       | `clone -> "clone"
                       | `copy -> "copy"
                       | `create_template -> "create_template"
                       | `revert -> "revert"
                       | `checkpoint -> "checkpoint"
                       | `snapshot_with_quiesce -> "snapshot_with_quiesce"
                       | `provision -> "provision"
                       | `start -> "start"
                       | `start_on -> "start_on"
                       | `pause -> "pause"
                       | `unpause -> "unpause"
                       | `clean_shutdown -> "clean_shutdown"
                       | `clean_reboot -> "clean_reboot"
                       | `hard_shutdown -> "hard_shutdown"
                       | `power_state_reset -> "power_state_reset"
                       | `hard_reboot -> "hard_reboot"
                       | `suspend -> "suspend"
                       | `csvm -> "csvm"
                       | `resume -> "resume"
                       | `resume_on -> "resume_on"
                       | `pool_migrate -> "pool_migrate"
                       | `migrate_send -> "migrate_send"
                       | `get_boot_record -> "get_boot_record"
                       | `send_sysrq -> "send_sysrq"
                       | `send_trigger -> "send_trigger"
                       | `query_services -> "query_services"
                       | `changing_memory_live -> "changing_memory_live"
                       | `awaiting_memory_live -> "awaiting_memory_live"
                       | `changing_dynamic_range -> "changing_dynamic_range"
                       | `changing_static_range -> "changing_static_range"
                       | `changing_memory_limits -> "changing_memory_limits"
                       | `changing_shadow_memory -> "changing_shadow_memory"
                       | `changing_shadow_memory_live -> "changing_shadow_memory_live"
                       | `changing_VCPUs -> "changing_vcpus"
                       | `changing_VCPUs_live -> "changing_vcpus_live"
                       | `assert_operation_valid -> "assert_operation_valid"
                       | `data_source_op -> "data_source_op"
                       | `update_allowed_operations -> "update_allowed_operations"
                       | `make_into_template -> "make_into_template"
                       | `import -> "import"
                       | `export -> "export"
                       | `metadata_export -> "metadata_export"
                       | `reverting -> "reverting"
                       | `destroy -> "destroy") (string) m
  
  and ref_VDI_to_ref_SR_map : ref_VDI_to_ref_SR_map -> xml =
    fun m -> map (tostring_reference) (ref_SR) m
  
  and ref_VIF_to_ref_network_map : ref_VIF_to_ref_network_map -> xml =
    fun m -> map (tostring_reference) (ref_network) m
  
  and vm_operations : vm_operations -> xml =
        fun v -> To.string(match v with
                         `snapshot -> "snapshot"
                       | `clone -> "clone"
                       | `copy -> "copy"
                       | `create_template -> "create_template"
                       | `revert -> "revert"
                       | `checkpoint -> "checkpoint"
                       | `snapshot_with_quiesce -> "snapshot_with_quiesce"
                       | `provision -> "provision"
                       | `start -> "start"
                       | `start_on -> "start_on"
                       | `pause -> "pause"
                       | `unpause -> "unpause"
                       | `clean_shutdown -> "clean_shutdown"
                       | `clean_reboot -> "clean_reboot"
                       | `hard_shutdown -> "hard_shutdown"
                       | `power_state_reset -> "power_state_reset"
                       | `hard_reboot -> "hard_reboot"
                       | `suspend -> "suspend"
                       | `csvm -> "csvm"
                       | `resume -> "resume"
                       | `resume_on -> "resume_on"
                       | `pool_migrate -> "pool_migrate"
                       | `migrate_send -> "migrate_send"
                       | `get_boot_record -> "get_boot_record"
                       | `send_sysrq -> "send_sysrq"
                       | `send_trigger -> "send_trigger"
                       | `query_services -> "query_services"
                       | `changing_memory_live -> "changing_memory_live"
                       | `awaiting_memory_live -> "awaiting_memory_live"
                       | `changing_dynamic_range -> "changing_dynamic_range"
                       | `changing_static_range -> "changing_static_range"
                       | `changing_memory_limits -> "changing_memory_limits"
                       | `changing_shadow_memory -> "changing_shadow_memory"
                       | `changing_shadow_memory_live -> "changing_shadow_memory_live"
                       | `changing_VCPUs -> "changing_VCPUs"
                       | `changing_VCPUs_live -> "changing_VCPUs_live"
                       | `assert_operation_valid -> "assert_operation_valid"
                       | `data_source_op -> "data_source_op"
                       | `update_allowed_operations -> "update_allowed_operations"
                       | `make_into_template -> "make_into_template"
                       | `import -> "import"
                       | `export -> "export"
                       | `metadata_export -> "metadata_export"
                       | `reverting -> "reverting"
                       | `destroy -> "destroy")
  
  and vM_metrics_t : vM_metrics_t -> xml =
    fun x -> To.structure [ "uuid", string x.vM_metrics_uuid; "memory_actual", int64 x.vM_metrics_memory_actual; "VCPUs_number", int64 x.vM_metrics_VCPUs_number; "VCPUs_utilisation", int64_to_float_map x.vM_metrics_VCPUs_utilisation; "VCPUs_CPU", int64_to_int64_map x.vM_metrics_VCPUs_CPU; "VCPUs_params", string_to_string_map x.vM_metrics_VCPUs_params; "VCPUs_flags", int64_to_string_set_map x.vM_metrics_VCPUs_flags; "state", string_set x.vM_metrics_state; "start_time", datetime x.vM_metrics_start_time; "install_time", datetime x.vM_metrics_install_time; "last_updated", datetime x.vM_metrics_last_updated; "other_config", string_to_string_map x.vM_metrics_other_config ]
  
  and int64_to_float_map : int64_to_float_map -> xml =
    fun m -> map (ToString.int64) (float) m
  
  and int64_to_int64_map : int64_to_int64_map -> xml =
    fun m -> map (ToString.int64) (int64) m
  
  and int64_to_string_set_map : int64_to_string_set_map -> xml =
    fun m -> map (ToString.int64) (string_set) m
  
  and ref_VM_metrics : ref_VM_metrics -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and vM_guest_metrics_t : vM_guest_metrics_t -> xml =
    fun x -> To.structure [ "uuid", string x.vM_guest_metrics_uuid; "os_version", string_to_string_map x.vM_guest_metrics_os_version; "PV_drivers_version", string_to_string_map x.vM_guest_metrics_PV_drivers_version; "PV_drivers_up_to_date", bool x.vM_guest_metrics_PV_drivers_up_to_date; "memory", string_to_string_map x.vM_guest_metrics_memory; "disks", string_to_string_map x.vM_guest_metrics_disks; "networks", string_to_string_map x.vM_guest_metrics_networks; "other", string_to_string_map x.vM_guest_metrics_other; "last_updated", datetime x.vM_guest_metrics_last_updated; "other_config", string_to_string_map x.vM_guest_metrics_other_config; "live", bool x.vM_guest_metrics_live ]
  
  and ref_VM_guest_metrics : ref_VM_guest_metrics -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and vMPP_t : vMPP_t -> xml =
    fun x -> To.structure [ "uuid", string x.vMPP_uuid; "name_label", string x.vMPP_name_label; "name_description", string x.vMPP_name_description; "is_policy_enabled", bool x.vMPP_is_policy_enabled; "backup_type", vmpp_backup_type x.vMPP_backup_type; "backup_retention_value", int64 x.vMPP_backup_retention_value; "backup_frequency", vmpp_backup_frequency x.vMPP_backup_frequency; "backup_schedule", string_to_string_map x.vMPP_backup_schedule; "is_backup_running", bool x.vMPP_is_backup_running; "backup_last_run_time", datetime x.vMPP_backup_last_run_time; "archive_target_type", vmpp_archive_target_type x.vMPP_archive_target_type; "archive_target_config", string_to_string_map x.vMPP_archive_target_config; "archive_frequency", vmpp_archive_frequency x.vMPP_archive_frequency; "archive_schedule", string_to_string_map x.vMPP_archive_schedule; "is_archive_running", bool x.vMPP_is_archive_running; "archive_last_run_time", datetime x.vMPP_archive_last_run_time; "VMs", ref_VM_set x.vMPP_VMs; "is_alarm_enabled", bool x.vMPP_is_alarm_enabled; "alarm_config", string_to_string_map x.vMPP_alarm_config; "recent_alerts", string_set x.vMPP_recent_alerts ]
  
  and vmpp_backup_type : vmpp_backup_type -> xml =
        fun v -> To.string(match v with
                         `snapshot -> "snapshot"
                       | `checkpoint -> "checkpoint")
  
  and vmpp_backup_frequency : vmpp_backup_frequency -> xml =
        fun v -> To.string(match v with
                         `hourly -> "hourly"
                       | `daily -> "daily"
                       | `weekly -> "weekly")
  
  and vmpp_archive_frequency : vmpp_archive_frequency -> xml =
        fun v -> To.string(match v with
                         `never -> "never"
                       | `always_after_backup -> "always_after_backup"
                       | `daily -> "daily"
                       | `weekly -> "weekly")
  
  and vmpp_archive_target_type : vmpp_archive_target_type -> xml =
        fun v -> To.string(match v with
                         `none -> "none"
                       | `cifs -> "cifs"
                       | `nfs -> "nfs")
  
  and ref_VMPP : ref_VMPP -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and vM_appliance_t : vM_appliance_t -> xml =
    fun x -> To.structure [ "uuid", string x.vM_appliance_uuid; "name_label", string x.vM_appliance_name_label; "name_description", string x.vM_appliance_name_description; "allowed_operations", vm_appliance_operation_set x.vM_appliance_allowed_operations; "current_operations", string_to_vm_appliance_operation_map x.vM_appliance_current_operations; "VMs", ref_VM_set x.vM_appliance_VMs ]
  
  and vm_appliance_operation_set : vm_appliance_operation_set -> xml =
    fun s -> set vm_appliance_operation s
  
  and string_to_vm_appliance_operation_map : string_to_vm_appliance_operation_map -> xml =
    fun m -> map (ToString.string) (vm_appliance_operation) m
  
  and vm_appliance_operation : vm_appliance_operation -> xml =
        fun v -> To.string(match v with
                         `start -> "start"
                       | `clean_shutdown -> "clean_shutdown"
                       | `hard_shutdown -> "hard_shutdown"
                       | `shutdown -> "shutdown")
  
  and ref_VM_appliance : ref_VM_appliance -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_session : ref_session -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and dR_task_t : dR_task_t -> xml =
    fun x -> To.structure [ "uuid", string x.dR_task_uuid; "introduced_SRs", ref_SR_set x.dR_task_introduced_SRs ]
  
  and host_t : host_t -> xml =
    fun x -> To.structure [ "uuid", string x.host_uuid; "name_label", string x.host_name_label; "name_description", string x.host_name_description; "memory_overhead", int64 x.host_memory_overhead; "allowed_operations", host_allowed_operations_set x.host_allowed_operations; "current_operations", string_to_host_allowed_operations_map x.host_current_operations; "API_version_major", int64 x.host_API_version_major; "API_version_minor", int64 x.host_API_version_minor; "API_version_vendor", string x.host_API_version_vendor; "API_version_vendor_implementation", string_to_string_map x.host_API_version_vendor_implementation; "enabled", bool x.host_enabled; "software_version", string_to_string_map x.host_software_version; "other_config", string_to_string_map x.host_other_config; "capabilities", string_set x.host_capabilities; "cpu_configuration", string_to_string_map x.host_cpu_configuration; "sched_policy", string x.host_sched_policy; "supported_bootloaders", string_set x.host_supported_bootloaders; "resident_VMs", ref_VM_set x.host_resident_VMs; "logging", string_to_string_map x.host_logging; "PIFs", ref_PIF_set x.host_PIFs; "suspend_image_sr", ref_SR x.host_suspend_image_sr; "crash_dump_sr", ref_SR x.host_crash_dump_sr; "crashdumps", ref_host_crashdump_set x.host_crashdumps; "patches", ref_host_patch_set x.host_patches; "PBDs", ref_PBD_set x.host_PBDs; "host_CPUs", ref_host_cpu_set x.host_host_CPUs; "cpu_info", string_to_string_map x.host_cpu_info; "hostname", string x.host_hostname; "address", string x.host_address; "metrics", ref_host_metrics x.host_metrics; "license_params", string_to_string_map x.host_license_params; "ha_statefiles", string_set x.host_ha_statefiles; "ha_network_peers", string_set x.host_ha_network_peers; "blobs", string_to_ref_blob_map x.host_blobs; "tags", string_set x.host_tags; "external_auth_type", string x.host_external_auth_type; "external_auth_service_name", string x.host_external_auth_service_name; "external_auth_configuration", string_to_string_map x.host_external_auth_configuration; "edition", string x.host_edition; "license_server", string_to_string_map x.host_license_server; "bios_strings", string_to_string_map x.host_bios_strings; "power_on_mode", string x.host_power_on_mode; "power_on_config", string_to_string_map x.host_power_on_config; "local_cache_sr", ref_SR x.host_local_cache_sr; "chipset_info", string_to_string_map x.host_chipset_info; "PCIs", ref_PCI_set x.host_PCIs; "PGPUs", ref_PGPU_set x.host_PGPUs ]
  
  and host_allowed_operations_set : host_allowed_operations_set -> xml =
    fun s -> set host_allowed_operations s
  
  and string_to_host_allowed_operations_map : string_to_host_allowed_operations_map -> xml =
    fun m -> map (ToString.string) (host_allowed_operations) m
  
  and host_allowed_operations : host_allowed_operations -> xml =
        fun v -> To.string(match v with
                         `provision -> "provision"
                       | `evacuate -> "evacuate"
                       | `shutdown -> "shutdown"
                       | `reboot -> "reboot"
                       | `power_on -> "power_on"
                       | `vm_start -> "vm_start"
                       | `vm_resume -> "vm_resume"
                       | `vm_migrate -> "vm_migrate")
  
  and ref_VDI_to_string_map : ref_VDI_to_string_map -> xml =
    fun m -> map (tostring_reference) (string) m
  
  and ref_VDI_set : ref_VDI_set -> xml =
    fun s -> set ref_VDI s
  
  and host_crashdump_t : host_crashdump_t -> xml =
    fun x -> To.structure [ "uuid", string x.host_crashdump_uuid; "host", ref_host x.host_crashdump_host; "timestamp", datetime x.host_crashdump_timestamp; "size", int64 x.host_crashdump_size; "other_config", string_to_string_map x.host_crashdump_other_config ]
  
  and ref_host_crashdump : ref_host_crashdump -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and host_patch_t : host_patch_t -> xml =
    fun x -> To.structure [ "uuid", string x.host_patch_uuid; "name_label", string x.host_patch_name_label; "name_description", string x.host_patch_name_description; "version", string x.host_patch_version; "host", ref_host x.host_patch_host; "applied", bool x.host_patch_applied; "timestamp_applied", datetime x.host_patch_timestamp_applied; "size", int64 x.host_patch_size; "pool_patch", ref_pool_patch x.host_patch_pool_patch; "other_config", string_to_string_map x.host_patch_other_config ]
  
  and ref_pool_patch : ref_pool_patch -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_host_patch : ref_host_patch -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and host_metrics_t : host_metrics_t -> xml =
    fun x -> To.structure [ "uuid", string x.host_metrics_uuid; "memory_total", int64 x.host_metrics_memory_total; "memory_free", int64 x.host_metrics_memory_free; "live", bool x.host_metrics_live; "last_updated", datetime x.host_metrics_last_updated; "other_config", string_to_string_map x.host_metrics_other_config ]
  
  and ref_host_metrics : ref_host_metrics -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and host_cpu_t : host_cpu_t -> xml =
    fun x -> To.structure [ "uuid", string x.host_cpu_uuid; "host", ref_host x.host_cpu_host; "number", int64 x.host_cpu_number; "vendor", string x.host_cpu_vendor; "speed", int64 x.host_cpu_speed; "modelname", string x.host_cpu_modelname; "family", int64 x.host_cpu_family; "model", int64 x.host_cpu_model; "stepping", string x.host_cpu_stepping; "flags", string x.host_cpu_flags; "features", string x.host_cpu_features; "utilisation", float x.host_cpu_utilisation; "other_config", string_to_string_map x.host_cpu_other_config ]
  
  and ref_host_cpu : ref_host_cpu -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and network_t : network_t -> xml =
    fun x -> To.structure [ "uuid", string x.network_uuid; "name_label", string x.network_name_label; "name_description", string x.network_name_description; "allowed_operations", network_operations_set x.network_allowed_operations; "current_operations", string_to_network_operations_map x.network_current_operations; "VIFs", ref_VIF_set x.network_VIFs; "PIFs", ref_PIF_set x.network_PIFs; "MTU", int64 x.network_MTU; "other_config", string_to_string_map x.network_other_config; "bridge", string x.network_bridge; "blobs", string_to_ref_blob_map x.network_blobs; "tags", string_set x.network_tags; "default_locking_mode", network_default_locking_mode x.network_default_locking_mode ]
  
  and network_operations_set : network_operations_set -> xml =
    fun s -> set network_operations s
  
  and string_to_network_operations_map : string_to_network_operations_map -> xml =
    fun m -> map (ToString.string) (network_operations) m
  
  and network_operations : network_operations -> xml =
        fun v -> To.string(match v with
                         `attaching -> "attaching")
  
  and network_default_locking_mode : network_default_locking_mode -> xml =
        fun v -> To.string(match v with
                         `unlocked -> "unlocked"
                       | `disabled -> "disabled")
  
  and vIF_t : vIF_t -> xml =
    fun x -> To.structure [ "uuid", string x.vIF_uuid; "allowed_operations", vif_operations_set x.vIF_allowed_operations; "current_operations", string_to_vif_operations_map x.vIF_current_operations; "device", string x.vIF_device; "network", ref_network x.vIF_network; "VM", ref_VM x.vIF_VM; "MAC", string x.vIF_MAC; "MTU", int64 x.vIF_MTU; "other_config", string_to_string_map x.vIF_other_config; "currently_attached", bool x.vIF_currently_attached; "status_code", int64 x.vIF_status_code; "status_detail", string x.vIF_status_detail; "runtime_properties", string_to_string_map x.vIF_runtime_properties; "qos_algorithm_type", string x.vIF_qos_algorithm_type; "qos_algorithm_params", string_to_string_map x.vIF_qos_algorithm_params; "qos_supported_algorithms", string_set x.vIF_qos_supported_algorithms; "metrics", ref_VIF_metrics x.vIF_metrics; "MAC_autogenerated", bool x.vIF_MAC_autogenerated; "locking_mode", vif_locking_mode x.vIF_locking_mode; "ipv4_allowed", string_set x.vIF_ipv4_allowed; "ipv6_allowed", string_set x.vIF_ipv6_allowed ]
  
  and vif_operations_set : vif_operations_set -> xml =
    fun s -> set vif_operations s
  
  and string_to_vif_operations_map : string_to_vif_operations_map -> xml =
    fun m -> map (ToString.string) (vif_operations) m
  
  and vif_operations : vif_operations -> xml =
        fun v -> To.string(match v with
                         `attach -> "attach"
                       | `plug -> "plug"
                       | `unplug -> "unplug")
  
  and vif_locking_mode : vif_locking_mode -> xml =
        fun v -> To.string(match v with
                         `network_default -> "network_default"
                       | `locked -> "locked"
                       | `unlocked -> "unlocked"
                       | `disabled -> "disabled")
  
  and ref_VIF : ref_VIF -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and vIF_metrics_t : vIF_metrics_t -> xml =
    fun x -> To.structure [ "uuid", string x.vIF_metrics_uuid; "io_read_kbs", float x.vIF_metrics_io_read_kbs; "io_write_kbs", float x.vIF_metrics_io_write_kbs; "last_updated", datetime x.vIF_metrics_last_updated; "other_config", string_to_string_map x.vIF_metrics_other_config ]
  
  and ref_VIF_metrics : ref_VIF_metrics -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and pIF_t : pIF_t -> xml =
    fun x -> To.structure [ "uuid", string x.pIF_uuid; "device", string x.pIF_device; "network", ref_network x.pIF_network; "host", ref_host x.pIF_host; "MAC", string x.pIF_MAC; "MTU", int64 x.pIF_MTU; "VLAN", int64 x.pIF_VLAN; "metrics", ref_PIF_metrics x.pIF_metrics; "physical", bool x.pIF_physical; "currently_attached", bool x.pIF_currently_attached; "ip_configuration_mode", ip_configuration_mode x.pIF_ip_configuration_mode; "IP", string x.pIF_IP; "netmask", string x.pIF_netmask; "gateway", string x.pIF_gateway; "DNS", string x.pIF_DNS; "bond_slave_of", ref_Bond x.pIF_bond_slave_of; "bond_master_of", ref_Bond_set x.pIF_bond_master_of; "VLAN_master_of", ref_VLAN x.pIF_VLAN_master_of; "VLAN_slave_of", ref_VLAN_set x.pIF_VLAN_slave_of; "management", bool x.pIF_management; "other_config", string_to_string_map x.pIF_other_config; "disallow_unplug", bool x.pIF_disallow_unplug; "tunnel_access_PIF_of", ref_tunnel_set x.pIF_tunnel_access_PIF_of; "tunnel_transport_PIF_of", ref_tunnel_set x.pIF_tunnel_transport_PIF_of; "ipv6_configuration_mode", ipv6_configuration_mode x.pIF_ipv6_configuration_mode; "IPv6", string_set x.pIF_IPv6; "ipv6_gateway", string x.pIF_ipv6_gateway; "primary_address_type", primary_address_type x.pIF_primary_address_type ]
  
  and ip_configuration_mode : ip_configuration_mode -> xml =
        fun v -> To.string(match v with
                         `None -> "None"
                       | `DHCP -> "DHCP"
                       | `Static -> "Static")
  
  and ipv6_configuration_mode : ipv6_configuration_mode -> xml =
        fun v -> To.string(match v with
                         `None -> "None"
                       | `DHCP -> "DHCP"
                       | `Static -> "Static"
                       | `Autoconf -> "Autoconf")
  
  and primary_address_type : primary_address_type -> xml =
        fun v -> To.string(match v with
                         `IPv4 -> "IPv4"
                       | `IPv6 -> "IPv6")
  
  and pIF_metrics_t : pIF_metrics_t -> xml =
    fun x -> To.structure [ "uuid", string x.pIF_metrics_uuid; "io_read_kbs", float x.pIF_metrics_io_read_kbs; "io_write_kbs", float x.pIF_metrics_io_write_kbs; "carrier", bool x.pIF_metrics_carrier; "vendor_id", string x.pIF_metrics_vendor_id; "vendor_name", string x.pIF_metrics_vendor_name; "device_id", string x.pIF_metrics_device_id; "device_name", string x.pIF_metrics_device_name; "speed", int64 x.pIF_metrics_speed; "duplex", bool x.pIF_metrics_duplex; "pci_bus_path", string x.pIF_metrics_pci_bus_path; "last_updated", datetime x.pIF_metrics_last_updated; "other_config", string_to_string_map x.pIF_metrics_other_config ]
  
  and ref_PIF_metrics : ref_PIF_metrics -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and bond_t : bond_t -> xml =
    fun x -> To.structure [ "uuid", string x.bond_uuid; "master", ref_PIF x.bond_master; "slaves", ref_PIF_set x.bond_slaves; "other_config", string_to_string_map x.bond_other_config; "primary_slave", ref_PIF x.bond_primary_slave; "mode", bond_mode x.bond_mode; "properties", string_to_string_map x.bond_properties; "links_up", int64 x.bond_links_up ]
  
  and ref_PIF_set : ref_PIF_set -> xml =
    fun s -> set ref_PIF s
  
  and bond_mode : bond_mode -> xml =
        fun v -> To.string(match v with
                         `balanceslb -> "balance-slb"
                       | `activebackup -> "active-backup"
                       | `lacp -> "lacp")
  
  and ref_Bond : ref_Bond -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and vLAN_t : vLAN_t -> xml =
    fun x -> To.structure [ "uuid", string x.vLAN_uuid; "tagged_PIF", ref_PIF x.vLAN_tagged_PIF; "untagged_PIF", ref_PIF x.vLAN_untagged_PIF; "tag", int64 x.vLAN_tag; "other_config", string_to_string_map x.vLAN_other_config ]
  
  and ref_VLAN : ref_VLAN -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and sM_t : sM_t -> xml =
    fun x -> To.structure [ "uuid", string x.sM_uuid; "name_label", string x.sM_name_label; "name_description", string x.sM_name_description; "type", string x.sM_type; "vendor", string x.sM_vendor; "copyright", string x.sM_copyright; "version", string x.sM_version; "required_api_version", string x.sM_required_api_version; "configuration", string_to_string_map x.sM_configuration; "capabilities", string_set x.sM_capabilities; "other_config", string_to_string_map x.sM_other_config; "driver_filename", string x.sM_driver_filename ]
  
  and ref_SM : ref_SM -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and sR_t : sR_t -> xml =
    fun x -> To.structure [ "uuid", string x.sR_uuid; "name_label", string x.sR_name_label; "name_description", string x.sR_name_description; "allowed_operations", storage_operations_set x.sR_allowed_operations; "current_operations", string_to_storage_operations_map x.sR_current_operations; "VDIs", ref_VDI_set x.sR_VDIs; "PBDs", ref_PBD_set x.sR_PBDs; "virtual_allocation", int64 x.sR_virtual_allocation; "physical_utilisation", int64 x.sR_physical_utilisation; "physical_size", int64 x.sR_physical_size; "type", string x.sR_type; "content_type", string x.sR_content_type; "shared", bool x.sR_shared; "other_config", string_to_string_map x.sR_other_config; "tags", string_set x.sR_tags; "sm_config", string_to_string_map x.sR_sm_config; "blobs", string_to_ref_blob_map x.sR_blobs; "local_cache_enabled", bool x.sR_local_cache_enabled; "introduced_by", ref_DR_task x.sR_introduced_by ]
  
  and storage_operations_set : storage_operations_set -> xml =
    fun s -> set storage_operations s
  
  and string_to_storage_operations_map : string_to_storage_operations_map -> xml =
    fun m -> map (ToString.string) (storage_operations) m
  
  and storage_operations : storage_operations -> xml =
        fun v -> To.string(match v with
                         `scan -> "scan"
                       | `destroy -> "destroy"
                       | `forget -> "forget"
                       | `plug -> "plug"
                       | `unplug -> "unplug"
                       | `update -> "update"
                       | `vdi_create -> "vdi_create"
                       | `vdi_introduce -> "vdi_introduce"
                       | `vdi_destroy -> "vdi_destroy"
                       | `vdi_resize -> "vdi_resize"
                       | `vdi_clone -> "vdi_clone"
                       | `vdi_snapshot -> "vdi_snapshot"
                       | `pbd_create -> "pbd_create"
                       | `pbd_destroy -> "pbd_destroy")
  
  and string_to_ref_blob_map : string_to_ref_blob_map -> xml =
    fun m -> map (ToString.string) (ref_blob) m
  
  and ref_DR_task : ref_DR_task -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and vDI_t : vDI_t -> xml =
    fun x -> To.structure [ "uuid", string x.vDI_uuid; "name_label", string x.vDI_name_label; "name_description", string x.vDI_name_description; "allowed_operations", vdi_operations_set x.vDI_allowed_operations; "current_operations", string_to_vdi_operations_map x.vDI_current_operations; "SR", ref_SR x.vDI_SR; "VBDs", ref_VBD_set x.vDI_VBDs; "crash_dumps", ref_crashdump_set x.vDI_crash_dumps; "virtual_size", int64 x.vDI_virtual_size; "physical_utilisation", int64 x.vDI_physical_utilisation; "type", vdi_type x.vDI_type; "sharable", bool x.vDI_sharable; "read_only", bool x.vDI_read_only; "other_config", string_to_string_map x.vDI_other_config; "storage_lock", bool x.vDI_storage_lock; "location", string x.vDI_location; "managed", bool x.vDI_managed; "missing", bool x.vDI_missing; "parent", ref_VDI x.vDI_parent; "xenstore_data", string_to_string_map x.vDI_xenstore_data; "sm_config", string_to_string_map x.vDI_sm_config; "is_a_snapshot", bool x.vDI_is_a_snapshot; "snapshot_of", ref_VDI x.vDI_snapshot_of; "snapshots", ref_VDI_set x.vDI_snapshots; "snapshot_time", datetime x.vDI_snapshot_time; "tags", string_set x.vDI_tags; "allow_caching", bool x.vDI_allow_caching; "on_boot", on_boot x.vDI_on_boot; "metadata_of_pool", ref_pool x.vDI_metadata_of_pool; "metadata_latest", bool x.vDI_metadata_latest ]
  
  and vdi_operations_set : vdi_operations_set -> xml =
    fun s -> set vdi_operations s
  
  and string_to_vdi_operations_map : string_to_vdi_operations_map -> xml =
    fun m -> map (ToString.string) (vdi_operations) m
  
  and vdi_operations : vdi_operations -> xml =
        fun v -> To.string(match v with
                         `scan -> "scan"
                       | `clone -> "clone"
                       | `copy -> "copy"
                       | `resize -> "resize"
                       | `resize_online -> "resize_online"
                       | `snapshot -> "snapshot"
                       | `destroy -> "destroy"
                       | `forget -> "forget"
                       | `update -> "update"
                       | `force_unlock -> "force_unlock"
                       | `generate_config -> "generate_config"
                       | `blocked -> "blocked")
  
  and vdi_type : vdi_type -> xml =
        fun v -> To.string(match v with
                         `system -> "system"
                       | `user -> "user"
                       | `ephemeral -> "ephemeral"
                       | `suspend -> "suspend"
                       | `crashdump -> "crashdump"
                       | `ha_statefile -> "ha_statefile"
                       | `metadata -> "metadata"
                       | `redo_log -> "redo_log")
  
  and ref_pool : ref_pool -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and on_boot : on_boot -> xml =
        fun v -> To.string(match v with
                         `reset -> "reset"
                       | `persist -> "persist")
  
  and vBD_t : vBD_t -> xml =
    fun x -> To.structure [ "uuid", string x.vBD_uuid; "allowed_operations", vbd_operations_set x.vBD_allowed_operations; "current_operations", string_to_vbd_operations_map x.vBD_current_operations; "VM", ref_VM x.vBD_VM; "VDI", ref_VDI x.vBD_VDI; "device", string x.vBD_device; "userdevice", string x.vBD_userdevice; "bootable", bool x.vBD_bootable; "mode", vbd_mode x.vBD_mode; "type", vbd_type x.vBD_type; "unpluggable", bool x.vBD_unpluggable; "storage_lock", bool x.vBD_storage_lock; "empty", bool x.vBD_empty; "other_config", string_to_string_map x.vBD_other_config; "currently_attached", bool x.vBD_currently_attached; "status_code", int64 x.vBD_status_code; "status_detail", string x.vBD_status_detail; "runtime_properties", string_to_string_map x.vBD_runtime_properties; "qos_algorithm_type", string x.vBD_qos_algorithm_type; "qos_algorithm_params", string_to_string_map x.vBD_qos_algorithm_params; "qos_supported_algorithms", string_set x.vBD_qos_supported_algorithms; "metrics", ref_VBD_metrics x.vBD_metrics ]
  
  and vbd_operations_set : vbd_operations_set -> xml =
    fun s -> set vbd_operations s
  
  and string_to_vbd_operations_map : string_to_vbd_operations_map -> xml =
    fun m -> map (ToString.string) (vbd_operations) m
  
  and vbd_operations : vbd_operations -> xml =
        fun v -> To.string(match v with
                         `attach -> "attach"
                       | `eject -> "eject"
                       | `insert -> "insert"
                       | `plug -> "plug"
                       | `unplug -> "unplug"
                       | `unplug_force -> "unplug_force"
                       | `pause -> "pause"
                       | `unpause -> "unpause")
  
  and vbd_mode : vbd_mode -> xml =
        fun v -> To.string(match v with
                         `RO -> "RO"
                       | `RW -> "RW")
  
  and vbd_type : vbd_type -> xml =
        fun v -> To.string(match v with
                         `CD -> "CD"
                       | `Disk -> "Disk")
  
  and ref_VBD : ref_VBD -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and vBD_metrics_t : vBD_metrics_t -> xml =
    fun x -> To.structure [ "uuid", string x.vBD_metrics_uuid; "io_read_kbs", float x.vBD_metrics_io_read_kbs; "io_write_kbs", float x.vBD_metrics_io_write_kbs; "last_updated", datetime x.vBD_metrics_last_updated; "other_config", string_to_string_map x.vBD_metrics_other_config ]
  
  and float : float -> xml =
    To.double
  
  and ref_VBD_metrics : ref_VBD_metrics -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and pBD_t : pBD_t -> xml =
    fun x -> To.structure [ "uuid", string x.pBD_uuid; "host", ref_host x.pBD_host; "SR", ref_SR x.pBD_SR; "device_config", string_to_string_map x.pBD_device_config; "currently_attached", bool x.pBD_currently_attached; "other_config", string_to_string_map x.pBD_other_config ]
  
  and ref_SR : ref_SR -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_PBD : ref_PBD -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and crashdump_t : crashdump_t -> xml =
    fun x -> To.structure [ "uuid", string x.crashdump_uuid; "VM", ref_VM x.crashdump_VM; "VDI", ref_VDI x.crashdump_VDI; "other_config", string_to_string_map x.crashdump_other_config ]
  
  and ref_VDI : ref_VDI -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_crashdump : ref_crashdump -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and vTPM_t : vTPM_t -> xml =
    fun x -> To.structure [ "uuid", string x.vTPM_uuid; "VM", ref_VM x.vTPM_VM; "backend", ref_VM x.vTPM_backend ]
  
  and ref_VTPM : ref_VTPM -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and console_t : console_t -> xml =
    fun x -> To.structure [ "uuid", string x.console_uuid; "protocol", console_protocol x.console_protocol; "location", string x.console_location; "VM", ref_VM x.console_VM; "other_config", string_to_string_map x.console_other_config ]
  
  and console_protocol : console_protocol -> xml =
        fun v -> To.string(match v with
                         `vt100 -> "vt100"
                       | `rfb -> "rfb"
                       | `rdp -> "rdp")
  
  and ref_console : ref_console -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and user_t : user_t -> xml =
    fun x -> To.structure [ "uuid", string x.user_uuid; "short_name", string x.user_short_name; "fullname", string x.user_fullname; "other_config", string_to_string_map x.user_other_config ]
  
  and ref_user : ref_user -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and blob_t : blob_t -> xml =
    fun x -> To.structure [ "uuid", string x.blob_uuid; "name_label", string x.blob_name_label; "name_description", string x.blob_name_description; "size", int64 x.blob_size; "public", bool x.blob_public; "last_updated", datetime x.blob_last_updated; "mime_type", string x.blob_mime_type ]
  
  and ref_blob : ref_blob -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and int64 : int64 -> xml =
    fun n -> To.string(Int64.to_string n)
  
  and cls : cls -> xml =
        fun v -> To.string(match v with
                         `VM -> "VM"
                       | `Host -> "Host"
                       | `SR -> "SR"
                       | `Pool -> "Pool"
                       | `VMPP -> "VMPP")
  
  and datetime : datetime -> xml =
    To.datetime
  
  and ref_message : ref_message -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and secret_t : secret_t -> xml =
    fun x -> To.structure [ "uuid", string x.secret_uuid; "value", string x.secret_value; "other_config", string_to_string_map x.secret_other_config ]
  
  and ref_secret : ref_secret -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and tunnel_t : tunnel_t -> xml =
    fun x -> To.structure [ "uuid", string x.tunnel_uuid; "access_PIF", ref_PIF x.tunnel_access_PIF; "transport_PIF", ref_PIF x.tunnel_transport_PIF; "status", string_to_string_map x.tunnel_status; "other_config", string_to_string_map x.tunnel_other_config ]
  
  and ref_PIF : ref_PIF -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_network : ref_network -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_tunnel : ref_tunnel -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and pCI_t : pCI_t -> xml =
    fun x -> To.structure [ "uuid", string x.pCI_uuid; "class_name", string x.pCI_class_name; "vendor_name", string x.pCI_vendor_name; "device_name", string x.pCI_device_name; "host", ref_host x.pCI_host; "pci_id", string x.pCI_pci_id; "dependencies", ref_PCI_set x.pCI_dependencies; "other_config", string_to_string_map x.pCI_other_config ]
  
  and ref_PCI_set : ref_PCI_set -> xml =
    fun s -> set ref_PCI s
  
  and pGPU_t : pGPU_t -> xml =
    fun x -> To.structure [ "uuid", string x.pGPU_uuid; "PCI", ref_PCI x.pGPU_PCI; "GPU_group", ref_GPU_group x.pGPU_GPU_group; "host", ref_host x.pGPU_host; "other_config", string_to_string_map x.pGPU_other_config ]
  
  and ref_PCI : ref_PCI -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_host : ref_host -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_PGPU : ref_PGPU -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and gPU_group_t : gPU_group_t -> xml =
    fun x -> To.structure [ "uuid", string x.gPU_group_uuid; "name_label", string x.gPU_group_name_label; "name_description", string x.gPU_group_name_description; "PGPUs", ref_PGPU_set x.gPU_group_PGPUs; "VGPUs", ref_VGPU_set x.gPU_group_VGPUs; "GPU_types", string_set x.gPU_group_GPU_types; "other_config", string_to_string_map x.gPU_group_other_config ]
  
  and string_set : string_set -> xml =
    fun s -> set string s
  
  and vGPU_t : vGPU_t -> xml =
    fun x -> To.structure [ "uuid", string x.vGPU_uuid; "VM", ref_VM x.vGPU_VM; "GPU_group", ref_GPU_group x.vGPU_GPU_group; "device", string x.vGPU_device; "currently_attached", bool x.vGPU_currently_attached; "other_config", string_to_string_map x.vGPU_other_config ]
  
  and bool : bool -> xml =
    To.boolean
  
  and ref_VM : ref_VM -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and ref_GPU_group : ref_GPU_group -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and string_to_string_map : string_to_string_map -> xml =
    fun m -> map (ToString.string) (string) m
  
  and ref_VGPU : ref_VGPU -> xml =
    fun r -> To.string (Ref.string_of r)
  
  and string : string -> xml =
    To.string
end

module type API = sig

  module Async : sig
    module Session : sig
      val get_all_subject_identifiers : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val logout_subject_identifier : rpc:(xml -> xml) -> session_id:ref_session -> subject_identifier:string -> ref_task
    end
    module Subject : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> subject_identifier:string -> other_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:subject_t -> ref_task
    end
    module Role : sig
    end
    module Task : sig
      val cancel : rpc:(xml -> xml) -> session_id:ref_session -> task:ref_task -> ref_task
    end
    module Event : sig
      val register : rpc:(xml -> xml) -> session_id:ref_session -> classes:string_set -> ref_task
      val unregister : rpc:(xml -> xml) -> session_id:ref_session -> classes:string_set -> ref_task
    end
    module Pool : sig
      val join : rpc:(xml -> xml) -> session_id:ref_session -> master_address:string -> master_username:string -> master_password:string -> ref_task
      val join_force : rpc:(xml -> xml) -> session_id:ref_session -> master_address:string -> master_username:string -> master_password:string -> ref_task
      val eject : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val initial_auth : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val recover_slaves : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val hello : rpc:(xml -> xml) -> session_id:ref_session -> host_uuid:string -> host_address:string -> ref_task
      val create_VLAN : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> vLAN:int64 -> ref_task
      val create_VLAN_from_PIF : rpc:(xml -> xml) -> session_id:ref_session -> pif:ref_PIF -> network:ref_network -> vLAN:int64 -> ref_task
      val slave_network_report : rpc:(xml -> xml) -> session_id:ref_session -> phydevs:string_to_string_map -> dev_to_mac:string_to_string_map -> dev_to_mtu:string_to_int64_map -> slave_host:ref_host -> ref_task
      val enable_ha : rpc:(xml -> xml) -> session_id:ref_session -> heartbeat_srs:ref_SR_set -> configuration:string_to_string_map -> ref_task
      val disable_ha : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val sync_database : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val designate_new_master : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val set_ha_host_failures_to_tolerate : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:int64 -> ref_task
      val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> pool:ref_pool -> name:string -> mime_type:string -> public:bool -> ref_task
      val ha_schedule_plan_recomputation : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val enable_binary_storage : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val disable_binary_storage : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val initialize_wlb : rpc:(xml -> xml) -> session_id:ref_session -> wlb_url:string -> wlb_username:string -> wlb_password:string -> xenserver_username:string -> xenserver_password:string -> ref_task
      val deconfigure_wlb : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val send_wlb_configuration : rpc:(xml -> xml) -> session_id:ref_session -> config:string_to_string_map -> ref_task
      val retrieve_wlb_configuration : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val retrieve_wlb_recommendations : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val send_test_post : rpc:(xml -> xml) -> session_id:ref_session -> host:string -> port:int64 -> body:string -> ref_task
      val certificate_install : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> cert:string -> ref_task
      val certificate_uninstall : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> ref_task
      val certificate_list : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val crl_install : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> cert:string -> ref_task
      val crl_uninstall : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> ref_task
      val crl_list : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val certificate_sync : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val enable_redo_log : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
      val disable_redo_log : rpc:(xml -> xml) -> session_id:ref_session -> ref_task
      val audit_log_append : rpc:(xml -> xml) -> session_id:ref_session -> line:string -> ref_task
      val set_vswitch_controller : rpc:(xml -> xml) -> session_id:ref_session -> address:string -> ref_task
      val enable_local_storage_caching : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> ref_task
      val disable_local_storage_caching : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> ref_task
    end
    module Pool_patch : sig
      val apply : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> host:ref_host -> ref_task
      val pool_apply : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> ref_task
      val precheck : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> host:ref_host -> ref_task
      val clean : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> ref_task
      val pool_clean : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> ref_task
      val clean_on_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> host:ref_host -> ref_task
    end
    module VM : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> user_version:int64 -> is_a_template:bool -> affinity:ref_host -> memory_target:int64 -> memory_static_max:int64 -> memory_dynamic_max:int64 -> memory_dynamic_min:int64 -> memory_static_min:int64 -> vCPUs_params:string_to_string_map -> vCPUs_max:int64 -> vCPUs_at_startup:int64 -> actions_after_shutdown:on_normal_exit -> actions_after_reboot:on_normal_exit -> actions_after_crash:on_crash_behaviour -> pV_bootloader:string -> pV_kernel:string -> pV_ramdisk:string -> pV_args:string -> pV_bootloader_args:string -> pV_legacy_args:string -> hVM_boot_policy:string -> hVM_boot_params:string_to_string_map -> hVM_shadow_multiplier:float -> platform:string_to_string_map -> pCI_bus:string -> other_config:string_to_string_map -> recommendations:string -> xenstore_data:string_to_string_map -> ha_always_run:bool -> ha_restart_priority:string -> tags:string_set -> blocked_operations:vm_operations_to_string_map -> protection_policy:ref_VMPP -> is_snapshot_from_vmpp:bool -> appliance:ref_VM_appliance -> start_delay:int64 -> shutdown_delay:int64 -> order:int64 -> suspend_SR:ref_SR -> version:int64 -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_task
      val snapshot : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> ref_task
      val snapshot_with_quiesce : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> ref_task
      val clone : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> ref_task
      val copy : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> sr:ref_SR -> ref_task
      val revert : rpc:(xml -> xml) -> session_id:ref_session -> snapshot:ref_VM -> ref_task
      val checkpoint : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> ref_task
      val provision : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val start : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> start_paused:bool -> force:bool -> ref_task
      val start_on : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> start_paused:bool -> force:bool -> ref_task
      val pause : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val unpause : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val clean_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val clean_reboot : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val hard_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val power_state_reset : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val hard_reboot : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val suspend : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val csvm : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val resume : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> start_paused:bool -> force:bool -> ref_task
      val hard_reboot_internal : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val resume_on : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> start_paused:bool -> force:bool -> ref_task
      val pool_migrate : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> options:string_to_string_map -> ref_task
      val pool_migrate_complete : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> ref_task
      val set_VCPUs_number_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> nvcpu:int64 -> ref_task
      val add_to_VCPUs_params_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> value:string -> ref_task
      val compute_memory_overhead : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val set_memory_dynamic_range : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> min:int64 -> max:int64 -> ref_task
      val set_memory_static_range : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> min:int64 -> max:int64 -> ref_task
      val set_memory_limits : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> static_min:int64 -> static_max:int64 -> dynamic_min:int64 -> dynamic_max:int64 -> ref_task
      val set_memory_target_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> target:int64 -> ref_task
      val wait_memory_target_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_task
      val get_cooperative : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_task
      val set_shadow_multiplier_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> multiplier:float -> ref_task
      val send_sysrq : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> key:string -> ref_task
      val send_trigger : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> trigger:string -> ref_task
      val maximise_memory : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> total:int64 -> approximate:bool -> ref_task
      val migrate_send : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> dest:string_to_string_map -> live:bool -> vdi_map:ref_VDI_to_ref_SR_map -> vif_map:ref_VIF_to_ref_network_map -> options:string_to_string_map -> ref_task
      val assert_can_migrate : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> dest:string_to_string_map -> live:bool -> vdi_map:ref_VDI_to_ref_SR_map -> vif_map:ref_VIF_to_ref_network_map -> options:string_to_string_map -> ref_task
      val assert_operation_valid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> op:vm_operations -> ref_task
      val update_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_task
      val get_possible_hosts : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val assert_can_boot_here : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> host:ref_host -> ref_task
      val atomic_set_resident_on : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> ref_task
      val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> name:string -> mime_type:string -> public:bool -> ref_task
      val s3_suspend : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val s3_resume : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val assert_agile : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_task
      val update_snapshot_metadata : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> snapshot_of:ref_VM -> snapshot_time:datetime -> transportable_snapshot_id:string -> ref_task
      val retrieve_wlb_recommendations : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_task
      val copy_bios_strings : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> ref_task
      val set_start_delay : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> ref_task
      val set_shutdown_delay : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> ref_task
      val set_order : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> ref_task
      val set_suspend_VDI : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:ref_VDI -> ref_task
      val assert_can_be_recovered : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> session_to:ref_session -> ref_task
      val recover : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> session_to:ref_session -> force:bool -> ref_task
      val import_convert : rpc:(xml -> xml) -> session_id:ref_session -> _type:string -> username:string -> password:string -> sr:ref_SR -> remote_config:string_to_string_map -> ref_task
      val set_appliance : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:ref_VM_appliance -> ref_task
      val query_services : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vM_t -> ref_task
    end
    module VM_metrics : sig
    end
    module VM_guest_metrics : sig
    end
    module VMPP : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> is_policy_enabled:bool -> backup_type:vmpp_backup_type -> backup_retention_value:int64 -> backup_frequency:vmpp_backup_frequency -> backup_schedule:string_to_string_map -> archive_target_type:vmpp_archive_target_type -> archive_target_config:string_to_string_map -> archive_frequency:vmpp_archive_frequency -> archive_schedule:string_to_string_map -> is_alarm_enabled:bool -> alarm_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vMPP_t -> ref_task
    end
    module VM_appliance : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> ref_task
      val start : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> paused:bool -> ref_task
      val clean_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> ref_task
      val hard_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> ref_task
      val shutdown : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> ref_task
      val assert_can_be_recovered : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> session_to:ref_session -> ref_task
      val recover : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> session_to:ref_session -> force:bool -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vM_appliance_t -> ref_task
    end
    module DR_task : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> _type:string -> device_config:string_to_string_map -> whitelist:string_set -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_DR_task -> ref_task
    end
    module Host : sig
      val disable : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val enable : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val shutdown : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val reboot : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val dmesg : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val dmesg_clear : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val get_log : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val send_debug_keys : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> keys:string -> ref_task
      val bugreport_upload : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> url:string -> options:string_to_string_map -> ref_task
      val license_apply : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> contents:string -> ref_task
      val create : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> hostname:string -> address:string -> external_auth_type:string -> external_auth_service_name:string -> external_auth_configuration:string_to_string_map -> license_params:string_to_string_map -> edition:string -> license_server:string_to_string_map -> local_cache_sr:ref_SR -> chipset_info:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_task
      val power_on : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val set_license_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string_to_string_map -> ref_task
      val ha_disarm_fencing : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val preconfigure_ha : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> statefiles:ref_VDI_set -> metadata_vdi:ref_VDI -> generation:string -> ref_task
      val ha_join_liveset : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val ha_disable_failover_decisions : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val ha_wait_for_shutdown_via_statefile : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val ha_stop_daemon : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val ha_release_resources : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val assert_can_evacuate : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val get_vms_which_prevent_evacuation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_task
      val get_uncooperative_resident_VMs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_task
      val get_uncooperative_domains : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_task
      val evacuate : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val notify : rpc:(xml -> xml) -> session_id:ref_session -> ty:string -> params:string -> ref_task
      val syslog_reconfigure : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val management_reconfigure : rpc:(xml -> xml) -> session_id:ref_session -> pif:ref_PIF -> ref_task
      val get_management_interface : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val restart_agent : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val compute_free_memory : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val compute_memory_overhead : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> mime_type:string -> public:bool -> ref_task
      val call_plugin : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> plugin:string -> fn:string -> args:string_to_string_map -> ref_task
      val enable_binary_storage : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val disable_binary_storage : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val retrieve_wlb_evacuate_recommendations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_task
      val certificate_install : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> cert:string -> ref_task
      val certificate_uninstall : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> ref_task
      val certificate_list : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val crl_install : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> crl:string -> ref_task
      val crl_uninstall : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> ref_task
      val crl_list : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val certificate_sync : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val get_server_certificate : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val update_pool_secret : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> pool_secret:string -> ref_task
      val update_master : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> master_address:string -> ref_task
      val attach_static_vdis : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vdi_reason_map:ref_VDI_to_string_map -> ref_task
      val detach_static_vdis : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vdis:ref_VDI_set -> ref_task
      val set_localdb_key : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> key:string -> value:string -> ref_task
      val refresh_pack_info : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val set_power_on_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> power_on_mode:string -> power_on_config:string_to_string_map -> ref_task
      val reset_networking : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val migrate_receive : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> network:ref_network -> options:string_to_string_map -> ref_task
    end
    module Host_crashdump : sig
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> ref_task
      val upload : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> url:string -> options:string_to_string_map -> ref_task
    end
    module Host_patch : sig
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> ref_task
      val apply : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> ref_task
    end
    module Host_metrics : sig
    end
    module Host_cpu : sig
    end
    module Network : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> mTU:int64 -> other_config:string_to_string_map -> tags:string_set -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> ref_task
      val attach : rpc:(xml -> xml) -> session_id:ref_session -> network:ref_network -> host:ref_host -> ref_task
      val pool_introduce : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> mTU:int64 -> other_config:string_to_string_map -> bridge:string -> ref_task
      val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> network:ref_network -> name:string -> mime_type:string -> public:bool -> ref_task
      val set_default_locking_mode : rpc:(xml -> xml) -> session_id:ref_session -> network:ref_network -> value:network_default_locking_mode -> ref_task
      val attach_for_vm : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vm:ref_VM -> ref_task
      val detach_for_vm : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vm:ref_VM -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:network_t -> ref_task
    end
    module VIF : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> vM:ref_VM -> mAC:string -> mTU:int64 -> other_config:string_to_string_map -> qos_algorithm_type:string -> qos_algorithm_params:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> ref_task
      val plug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> ref_task
      val unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> ref_task
      val unplug_force : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> ref_task
      val set_locking_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:vif_locking_mode -> ref_task
      val set_ipv4_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string_set -> ref_task
      val add_ipv4_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> ref_task
      val remove_ipv4_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> ref_task
      val set_ipv6_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string_set -> ref_task
      val add_ipv6_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> ref_task
      val remove_ipv6_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vIF_t -> ref_task
    end
    module VIF_metrics : sig
    end
    module PIF : sig
      val create_VLAN : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> host:ref_host -> vLAN:int64 -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_task
      val reconfigure_ip : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> mode:ip_configuration_mode -> iP:string -> netmask:string -> gateway:string -> dNS:string -> ref_task
      val reconfigure_ipv6 : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> mode:ipv6_configuration_mode -> iPv6:string -> gateway:string -> dNS:string -> ref_task
      val set_primary_address_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> primary_address_type:primary_address_type -> ref_task
      val scan : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_task
      val introduce : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> mAC:string -> device:string -> ref_task
      val forget : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_task
      val unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_task
      val plug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_task
      val pool_introduce : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> host:ref_host -> mAC:string -> mTU:int64 -> vLAN:int64 -> physical:bool -> ip_configuration_mode:ip_configuration_mode -> iP:string -> netmask:string -> gateway:string -> dNS:string -> bond_slave_of:ref_Bond -> vLAN_master_of:ref_VLAN -> management:bool -> other_config:string_to_string_map -> disallow_unplug:bool -> ipv6_configuration_mode:ipv6_configuration_mode -> iPv6:string_set -> ipv6_gateway:string -> primary_address_type:primary_address_type -> ref_task
      val db_introduce : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> host:ref_host -> mAC:string -> mTU:int64 -> vLAN:int64 -> physical:bool -> ip_configuration_mode:ip_configuration_mode -> iP:string -> netmask:string -> gateway:string -> dNS:string -> bond_slave_of:ref_Bond -> vLAN_master_of:ref_VLAN -> management:bool -> other_config:string_to_string_map -> disallow_unplug:bool -> ipv6_configuration_mode:ipv6_configuration_mode -> iPv6:string_set -> ipv6_gateway:string -> primary_address_type:primary_address_type -> ref_task
      val db_forget : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_task
    end
    module PIF_metrics : sig
    end
    module Bond : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> network:ref_network -> members:ref_PIF_set -> mAC:string -> mode:bond_mode -> properties:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> ref_task
      val set_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> value:bond_mode -> ref_task
      val set_property : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> name:string -> value:string -> ref_task
    end
    module VLAN : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> tagged_PIF:ref_PIF -> tag:int64 -> network:ref_network -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> ref_task
    end
    module SM : sig
    end
    module SR : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> device_config:string_to_string_map -> physical_size:int64 -> name_label:string -> name_description:string -> _type:string -> content_type:string -> shared:bool -> sm_config:string_to_string_map -> ref_task
      val introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> _type:string -> content_type:string -> shared:bool -> sm_config:string_to_string_map -> ref_task
      val make : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> device_config:string_to_string_map -> physical_size:int64 -> name_label:string -> name_description:string -> _type:string -> content_type:string -> sm_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
      val forget : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
      val update : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
      val scan : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
      val probe : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> device_config:string_to_string_map -> _type:string -> sm_config:string_to_string_map -> ref_task
      val set_shared : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> value:bool -> ref_task
      val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> value:string -> ref_task
      val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> value:string -> ref_task
      val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> name:string -> mime_type:string -> public:bool -> ref_task
      val assert_can_host_ha_statefile : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
      val assert_supports_database_replication : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
      val enable_database_replication : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
      val disable_database_replication : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> ref_task
    end
    module VDI : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> sR:ref_SR -> virtual_size:int64 -> _type:vdi_type -> sharable:bool -> read_only:bool -> other_config:string_to_string_map -> xenstore_data:string_to_string_map -> sm_config:string_to_string_map -> tags:string_set -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_task
      val snapshot : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> driver_params:string_to_string_map -> ref_task
      val clone : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> driver_params:string_to_string_map -> ref_task
      val resize : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> size:int64 -> ref_task
      val resize_online : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> size:int64 -> ref_task
      val introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> sR:ref_SR -> _type:vdi_type -> sharable:bool -> read_only:bool -> other_config:string_to_string_map -> location:string -> xenstore_data:string_to_string_map -> sm_config:string_to_string_map -> managed:bool -> virtual_size:int64 -> physical_utilisation:int64 -> metadata_of_pool:ref_pool -> is_a_snapshot:bool -> snapshot_time:datetime -> snapshot_of:ref_VDI -> ref_task
      val pool_introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> sR:ref_SR -> _type:vdi_type -> sharable:bool -> read_only:bool -> other_config:string_to_string_map -> location:string -> xenstore_data:string_to_string_map -> sm_config:string_to_string_map -> managed:bool -> virtual_size:int64 -> physical_utilisation:int64 -> metadata_of_pool:ref_pool -> is_a_snapshot:bool -> snapshot_time:datetime -> snapshot_of:ref_VDI -> ref_task
      val db_introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> sR:ref_SR -> _type:vdi_type -> sharable:bool -> read_only:bool -> other_config:string_to_string_map -> location:string -> xenstore_data:string_to_string_map -> sm_config:string_to_string_map -> managed:bool -> virtual_size:int64 -> physical_utilisation:int64 -> metadata_of_pool:ref_pool -> is_a_snapshot:bool -> snapshot_time:datetime -> snapshot_of:ref_VDI -> ref_task
      val db_forget : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> ref_task
      val update : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> ref_task
      val copy : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> sr:ref_SR -> ref_task
      val force_unlock : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> ref_task
      val forget : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> ref_task
      val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string -> ref_task
      val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string -> ref_task
      val generate_config : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vdi:ref_VDI -> ref_task
      val set_on_boot : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:on_boot -> ref_task
      val set_allow_caching : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:bool -> ref_task
      val open_database : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_task
      val checksum : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_task
      val read_database_pool_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_task
      val pool_migrate : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> sr:ref_SR -> options:string_to_string_map -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vDI_t -> ref_task
    end
    module VBD : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> vM:ref_VM -> vDI:ref_VDI -> userdevice:string -> bootable:bool -> mode:vbd_mode -> _type:vbd_type -> unpluggable:bool -> empty:bool -> other_config:string_to_string_map -> qos_algorithm_type:string -> qos_algorithm_params:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_task
      val eject : rpc:(xml -> xml) -> session_id:ref_session -> vbd:ref_VBD -> ref_task
      val insert : rpc:(xml -> xml) -> session_id:ref_session -> vbd:ref_VBD -> vdi:ref_VDI -> ref_task
      val plug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_task
      val unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_task
      val unplug_force : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_task
      val unplug_force_no_safety_check : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_task
      val assert_attachable : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_task
      val pause : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_task
      val unpause : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> token:string -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vBD_t -> ref_task
    end
    module VBD_metrics : sig
    end
    module PBD : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> sR:ref_SR -> device_config:string_to_string_map -> other_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> ref_task
      val plug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> ref_task
      val unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> ref_task
      val set_device_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> value:string_to_string_map -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:pBD_t -> ref_task
    end
    module Crashdump : sig
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> ref_task
    end
    module VTPM : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> vM:ref_VM -> backend:ref_VM -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VTPM -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vTPM_t -> ref_task
    end
    module Console : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> other_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:console_t -> ref_task
    end
    module User : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> short_name:string -> fullname:string -> other_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:user_t -> ref_task
    end
    module Data_source : sig
    end
    module Blob : sig
    end
    module Message : sig
    end
    module Secret : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> value:string -> other_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> ref_task
      val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:secret_t -> ref_task
    end
    module Tunnel : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> transport_PIF:ref_PIF -> network:ref_network -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> ref_task
    end
    module PCI : sig
    end
    module PGPU : sig
    end
    module GPU_group : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> other_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> ref_task
    end
    module VGPU : sig
      val create : rpc:(xml -> xml) -> session_id:ref_session -> vM:ref_VM -> gPU_group:ref_GPU_group -> device:string -> other_config:string_to_string_map -> ref_task
      val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> ref_task
    end
  end
  module Session : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> session_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_session
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> string
    val get_this_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> ref_host
    val get_this_user : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> ref_user
    val get_last_active : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> datetime
    val get_pool : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> bool
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> string_to_string_map
    val get_is_local_superuser : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> bool
    val get_subject : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> ref_subject
    val get_validation_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> datetime
    val get_auth_user_sid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> string
    val get_auth_user_name : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> string
    val get_rbac_permissions : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> string_set
    val get_tasks : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> ref_task_set
    val get_parent : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> ref_session
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_session -> key:string -> unit
    val login_with_password : rpc:(xml -> xml) -> uname:string -> pwd:string -> version:string -> ref_session
    val logout : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val change_password : rpc:(xml -> xml) -> session_id:ref_session -> old_pwd:string -> new_pwd:string -> unit
    val slave_login : rpc:(xml -> xml) -> host:ref_host -> psecret:string -> ref_session
    val slave_local_login : rpc:(xml -> xml) -> psecret:string -> ref_session
    val slave_local_login_with_password : rpc:(xml -> xml) -> uname:string -> pwd:string -> ref_session
    val local_logout : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val get_all_subject_identifiers : rpc:(xml -> xml) -> session_id:ref_session -> string_set
    val logout_subject_identifier : rpc:(xml -> xml) -> session_id:ref_session -> subject_identifier:string -> unit
  end
  module Auth : sig
    val get_subject_identifier : rpc:(xml -> xml) -> session_id:ref_session -> subject_name:string -> string
    val get_subject_information_from_identifier : rpc:(xml -> xml) -> session_id:ref_session -> subject_identifier:string -> string_to_string_map
    val get_group_membership : rpc:(xml -> xml) -> session_id:ref_session -> subject_identifier:string -> string_set
  end
  module Subject : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> subject_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_subject
    val create : rpc:(xml -> xml) -> session_id:ref_session -> subject_identifier:string -> other_config:string_to_string_map -> ref_subject
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> unit
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> string
    val get_subject_identifier : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> string
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> string_to_string_map
    val get_roles : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> ref_role_set
    val add_to_roles : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> role:ref_role -> unit
    val remove_from_roles : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> role:ref_role -> unit
    val get_permissions_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_subject -> string_set
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_subject_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_subject_to_subject_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_subject_to_subject_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:subject_t -> ref_subject
  end
  module Role : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_role -> role_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_role
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_role_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_role -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_role -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_role -> string
    val get_subroles : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_role -> ref_role_set
    val get_permissions : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_role -> ref_role_set
    val get_permissions_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_role -> string_set
    val get_by_permission : rpc:(xml -> xml) -> session_id:ref_session -> permission:ref_role -> ref_role_set
    val get_by_permission_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_role_set
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_role_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_role_to_role_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_role_to_role_t_map
  end
  module Task : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> task_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_task
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_task_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> string
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> task_allowed_operations_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> string_to_task_allowed_operations_map
    val get_created : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> datetime
    val get_finished : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> datetime
    val get_status : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> task_status_type
    val get_resident_on : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> ref_host
    val get_progress : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> float
    val get_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> string
    val get_result : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> string
    val get_error_info : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> string_set
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> string_to_string_map
    val get_subtask_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> ref_task
    val get_subtasks : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> ref_task_set
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> key:string -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> description:string -> ref_task
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_task -> unit
    val cancel : rpc:(xml -> xml) -> session_id:ref_session -> task:ref_task -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_task_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_task_to_task_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_task_to_task_t_map
  end
  module Event : sig
    val register : rpc:(xml -> xml) -> session_id:ref_session -> classes:string_set -> unit
    val unregister : rpc:(xml -> xml) -> session_id:ref_session -> classes:string_set -> unit
    val next : rpc:(xml -> xml) -> session_id:ref_session -> XMLRPC.xmlrpc
    val from : rpc:(xml -> xml) -> session_id:ref_session -> classes:string_set -> token:string -> timeout:float -> XMLRPC.xmlrpc
    val get_current_id : rpc:(xml -> xml) -> session_id:ref_session -> int64
    val inject : rpc:(xml -> xml) -> session_id:ref_session -> _class:string -> ref:string -> string
  end
  module Pool : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> pool_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_pool
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string
    val get_master : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> ref_host
    val get_default_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> ref_SR
    val get_suspend_image_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> ref_SR
    val get_crash_dump_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> ref_SR
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string_to_string_map
    val get_ha_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> bool
    val get_ha_configuration : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string_to_string_map
    val get_ha_statefiles : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string_set
    val get_ha_host_failures_to_tolerate : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> int64
    val get_ha_plan_exists_for : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> int64
    val get_ha_allow_overcommit : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> bool
    val get_ha_overcommitted : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> bool
    val get_blobs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string_to_ref_blob_map
    val get_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string_set
    val get_gui_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string_to_string_map
    val get_wlb_url : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string
    val get_wlb_username : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string
    val get_wlb_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> bool
    val get_wlb_verify_cert : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> bool
    val get_redo_log_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> bool
    val get_redo_log_vdi : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> ref_VDI
    val get_vswitch_controller : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string
    val get_restrictions : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> string_to_string_map
    val get_metadata_VDIs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> ref_VDI_set
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:string -> unit
    val set_default_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:ref_SR -> unit
    val set_suspend_image_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:ref_SR -> unit
    val set_crash_dump_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:ref_SR -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> key:string -> unit
    val set_ha_allow_overcommit : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:bool -> unit
    val set_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:string_set -> unit
    val add_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:string -> unit
    val remove_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:string -> unit
    val set_gui_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:string_to_string_map -> unit
    val add_to_gui_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> key:string -> value:string -> unit
    val remove_from_gui_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> key:string -> unit
    val set_wlb_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:bool -> unit
    val set_wlb_verify_cert : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:bool -> unit
    val join : rpc:(xml -> xml) -> session_id:ref_session -> master_address:string -> master_username:string -> master_password:string -> unit
    val join_force : rpc:(xml -> xml) -> session_id:ref_session -> master_address:string -> master_username:string -> master_password:string -> unit
    val eject : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val initial_auth : rpc:(xml -> xml) -> session_id:ref_session -> string
    val emergency_transition_to_master : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val emergency_reset_master : rpc:(xml -> xml) -> session_id:ref_session -> master_address:string -> unit
    val recover_slaves : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_set
    val hello : rpc:(xml -> xml) -> session_id:ref_session -> host_uuid:string -> host_address:string -> hello_return
    val is_slave : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> bool
    val create_VLAN : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> vLAN:int64 -> ref_PIF_set
    val create_VLAN_from_PIF : rpc:(xml -> xml) -> session_id:ref_session -> pif:ref_PIF -> network:ref_network -> vLAN:int64 -> ref_PIF_set
    val slave_network_report : rpc:(xml -> xml) -> session_id:ref_session -> phydevs:string_to_string_map -> dev_to_mac:string_to_string_map -> dev_to_mtu:string_to_int64_map -> slave_host:ref_host -> ref_PIF_set
    val enable_ha : rpc:(xml -> xml) -> session_id:ref_session -> heartbeat_srs:ref_SR_set -> configuration:string_to_string_map -> unit
    val disable_ha : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val sync_database : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val designate_new_master : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val ha_prevent_restarts_for : rpc:(xml -> xml) -> session_id:ref_session -> seconds:int64 -> unit
    val ha_failover_plan_exists : rpc:(xml -> xml) -> session_id:ref_session -> n:int64 -> bool
    val ha_compute_max_host_failures_to_tolerate : rpc:(xml -> xml) -> session_id:ref_session -> int64
    val ha_compute_hypothetical_max_host_failures_to_tolerate : rpc:(xml -> xml) -> session_id:ref_session -> configuration:ref_VM_to_string_map -> int64
    val ha_compute_vm_failover_plan : rpc:(xml -> xml) -> session_id:ref_session -> failed_hosts:ref_host_set -> failed_vms:ref_VM_set -> ref_VM_to_string_to_string_map_map
    val set_ha_host_failures_to_tolerate : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> value:int64 -> unit
    val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> pool:ref_pool -> name:string -> mime_type:string -> public:bool -> ref_blob
    val ha_schedule_plan_recomputation : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val enable_binary_storage : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val disable_binary_storage : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val enable_external_auth : rpc:(xml -> xml) -> session_id:ref_session -> pool:ref_pool -> config:string_to_string_map -> service_name:string -> auth_type:string -> unit
    val disable_external_auth : rpc:(xml -> xml) -> session_id:ref_session -> pool:ref_pool -> config:string_to_string_map -> unit
    val detect_nonhomogeneous_external_auth : rpc:(xml -> xml) -> session_id:ref_session -> pool:ref_pool -> unit
    val initialize_wlb : rpc:(xml -> xml) -> session_id:ref_session -> wlb_url:string -> wlb_username:string -> wlb_password:string -> xenserver_username:string -> xenserver_password:string -> unit
    val deconfigure_wlb : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val send_wlb_configuration : rpc:(xml -> xml) -> session_id:ref_session -> config:string_to_string_map -> unit
    val retrieve_wlb_configuration : rpc:(xml -> xml) -> session_id:ref_session -> string_to_string_map
    val retrieve_wlb_recommendations : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_to_string_set_map
    val send_test_post : rpc:(xml -> xml) -> session_id:ref_session -> host:string -> port:int64 -> body:string -> string
    val certificate_install : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> cert:string -> unit
    val certificate_uninstall : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> unit
    val certificate_list : rpc:(xml -> xml) -> session_id:ref_session -> string_set
    val crl_install : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> cert:string -> unit
    val crl_uninstall : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> unit
    val crl_list : rpc:(xml -> xml) -> session_id:ref_session -> string_set
    val certificate_sync : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val enable_redo_log : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val disable_redo_log : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val audit_log_append : rpc:(xml -> xml) -> session_id:ref_session -> line:string -> unit
    val set_vswitch_controller : rpc:(xml -> xml) -> session_id:ref_session -> address:string -> unit
    val test_archive_target : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> config:string_to_string_map -> string
    val enable_local_storage_caching : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> unit
    val disable_local_storage_caching : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_pool_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_pool_to_pool_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_pool_to_pool_t_map
  end
  module Pool_patch : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> pool_patch_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_pool_patch
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_pool_patch_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> string
    val get_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> string
    val get_size : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> int64
    val get_pool_applied : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> bool
    val get_host_patches : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> ref_host_patch_set
    val get_after_apply_guidance : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> after_apply_guidance_set
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> key:string -> unit
    val apply : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> host:ref_host -> string
    val pool_apply : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> unit
    val precheck : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> host:ref_host -> string
    val clean : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> unit
    val pool_clean : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> unit
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> unit
    val clean_on_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_pool_patch -> host:ref_host -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_pool_patch_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_pool_patch_to_pool_patch_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_pool_patch_to_pool_patch_t_map
  end
  module VM : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> vM_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VM
    val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> user_version:int64 -> is_a_template:bool -> affinity:ref_host -> memory_target:int64 -> memory_static_max:int64 -> memory_dynamic_max:int64 -> memory_dynamic_min:int64 -> memory_static_min:int64 -> vCPUs_params:string_to_string_map -> vCPUs_max:int64 -> vCPUs_at_startup:int64 -> actions_after_shutdown:on_normal_exit -> actions_after_reboot:on_normal_exit -> actions_after_crash:on_crash_behaviour -> pV_bootloader:string -> pV_kernel:string -> pV_ramdisk:string -> pV_args:string -> pV_bootloader_args:string -> pV_legacy_args:string -> hVM_boot_policy:string -> hVM_boot_params:string_to_string_map -> hVM_shadow_multiplier:float -> platform:string_to_string_map -> pCI_bus:string -> other_config:string_to_string_map -> recommendations:string -> xenstore_data:string_to_string_map -> ha_always_run:bool -> ha_restart_priority:string -> tags:string_set -> blocked_operations:vm_operations_to_string_map -> protection_policy:ref_VMPP -> is_snapshot_from_vmpp:bool -> appliance:ref_VM_appliance -> start_delay:int64 -> shutdown_delay:int64 -> order:int64 -> suspend_SR:ref_SR -> version:int64 -> ref_VM
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> unit
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_VM_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> vm_operations_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_vm_operations_map
    val get_power_state : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> vm_power_state
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_user_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_is_a_template : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> bool
    val get_suspend_VDI : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VDI
    val get_resident_on : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_host
    val get_affinity : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_host
    val get_memory_overhead : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_memory_target : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_memory_static_max : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_memory_dynamic_max : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_memory_dynamic_min : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_memory_static_min : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_VCPUs_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_VCPUs_max : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_VCPUs_at_startup : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_actions_after_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> on_normal_exit
    val get_actions_after_reboot : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> on_normal_exit
    val get_actions_after_crash : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> on_crash_behaviour
    val get_consoles : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_console_set
    val get_VIFs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VIF_set
    val get_VBDs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VBD_set
    val get_crash_dumps : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_crashdump_set
    val get_VTPMs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VTPM_set
    val get_PV_bootloader : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_PV_kernel : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_PV_ramdisk : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_PV_args : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_PV_bootloader_args : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_PV_legacy_args : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_HVM_boot_policy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_HVM_boot_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_HVM_shadow_multiplier : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> float
    val get_platform : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_PCI_bus : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_domid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_domarch : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_last_boot_CPU_flags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_is_control_domain : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> bool
    val get_metrics : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VM_metrics
    val get_guest_metrics : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VM_guest_metrics
    val get_last_booted_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_recommendations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_xenstore_data : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_ha_always_run : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> bool
    val get_ha_restart_priority : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_is_a_snapshot : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> bool
    val get_snapshot_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VM
    val get_snapshots : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VM_set
    val get_snapshot_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> datetime
    val get_transportable_snapshot_id : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_blobs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_ref_blob_map
    val get_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_set
    val get_blocked_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> vm_operations_to_string_map
    val get_snapshot_info : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_snapshot_metadata : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string
    val get_parent : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VM
    val get_children : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VM_set
    val get_bios_strings : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_protection_policy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VMPP
    val get_is_snapshot_from_vmpp : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> bool
    val get_appliance : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VM_appliance
    val get_start_delay : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_shutdown_delay : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_order : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val get_VGPUs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_VGPU_set
    val get_attached_PCIs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_PCI_set
    val get_suspend_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> ref_SR
    val get_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> int64
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_user_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_is_a_template : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:bool -> unit
    val set_affinity : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:ref_host -> unit
    val set_VCPUs_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string_to_string_map -> unit
    val add_to_VCPUs_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> value:string -> unit
    val remove_from_VCPUs_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> unit
    val set_actions_after_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:on_normal_exit -> unit
    val set_actions_after_reboot : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:on_normal_exit -> unit
    val set_actions_after_crash : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:on_crash_behaviour -> unit
    val set_PV_bootloader : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_PV_kernel : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_PV_ramdisk : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_PV_args : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_PV_bootloader_args : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_PV_legacy_args : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_HVM_boot_policy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_HVM_boot_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string_to_string_map -> unit
    val add_to_HVM_boot_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> value:string -> unit
    val remove_from_HVM_boot_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> unit
    val set_platform : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string_to_string_map -> unit
    val add_to_platform : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> value:string -> unit
    val remove_from_platform : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> unit
    val set_PCI_bus : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> unit
    val set_recommendations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_xenstore_data : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string_to_string_map -> unit
    val add_to_xenstore_data : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> value:string -> unit
    val remove_from_xenstore_data : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> unit
    val set_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string_set -> unit
    val add_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val remove_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_blocked_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:vm_operations_to_string_map -> unit
    val add_to_blocked_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:vm_operations -> value:string -> unit
    val remove_from_blocked_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:vm_operations -> unit
    val set_suspend_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:ref_SR -> unit
    val snapshot : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> ref_VM
    val snapshot_with_quiesce : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> ref_VM
    val clone : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> ref_VM
    val copy : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> sr:ref_SR -> ref_VM
    val revert : rpc:(xml -> xml) -> session_id:ref_session -> snapshot:ref_VM -> unit
    val checkpoint : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> new_name:string -> ref_VM
    val provision : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val start : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> start_paused:bool -> force:bool -> unit
    val start_on : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> start_paused:bool -> force:bool -> unit
    val pause : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val unpause : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val clean_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val clean_reboot : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val hard_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val power_state_reset : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val hard_reboot : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val suspend : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val csvm : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_VM
    val resume : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> start_paused:bool -> force:bool -> unit
    val hard_reboot_internal : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val resume_on : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> start_paused:bool -> force:bool -> unit
    val pool_migrate : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> options:string_to_string_map -> unit
    val pool_migrate_complete : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> unit
    val set_VCPUs_number_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> nvcpu:int64 -> unit
    val add_to_VCPUs_params_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> key:string -> value:string -> unit
    val set_ha_restart_priority : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:string -> unit
    val set_ha_always_run : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:bool -> unit
    val compute_memory_overhead : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> int64
    val set_memory_dynamic_max : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_memory_dynamic_min : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_memory_dynamic_range : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> min:int64 -> max:int64 -> unit
    val set_memory_static_max : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_memory_static_min : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_memory_static_range : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> min:int64 -> max:int64 -> unit
    val set_memory_limits : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> static_min:int64 -> static_max:int64 -> dynamic_min:int64 -> dynamic_max:int64 -> unit
    val set_memory_target_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> target:int64 -> unit
    val wait_memory_target_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> unit
    val get_cooperative : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> bool
    val set_HVM_shadow_multiplier : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:float -> unit
    val set_shadow_multiplier_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> multiplier:float -> unit
    val set_VCPUs_max : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_VCPUs_at_startup : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val send_sysrq : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> key:string -> unit
    val send_trigger : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> trigger:string -> unit
    val maximise_memory : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> total:int64 -> approximate:bool -> int64
    val migrate_send : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> dest:string_to_string_map -> live:bool -> vdi_map:ref_VDI_to_ref_SR_map -> vif_map:ref_VIF_to_ref_network_map -> options:string_to_string_map -> unit
    val assert_can_migrate : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> dest:string_to_string_map -> live:bool -> vdi_map:ref_VDI_to_ref_SR_map -> vif_map:ref_VIF_to_ref_network_map -> options:string_to_string_map -> unit
    val get_boot_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> vM_t
    val get_data_sources : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> data_source_t_set
    val record_data_source : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> data_source:string -> unit
    val query_data_source : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> data_source:string -> float
    val forget_data_source_archives : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> data_source:string -> unit
    val assert_operation_valid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> op:vm_operations -> unit
    val update_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> unit
    val get_allowed_VBD_devices : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> string_set
    val get_allowed_VIF_devices : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> string_set
    val get_possible_hosts : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_host_set
    val assert_can_boot_here : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> host:ref_host -> unit
    val atomic_set_resident_on : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> unit
    val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> name:string -> mime_type:string -> public:bool -> ref_blob
    val s3_suspend : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val s3_resume : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> unit
    val assert_agile : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> unit
    val update_snapshot_metadata : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> snapshot_of:ref_VM -> snapshot_time:datetime -> transportable_snapshot_id:string -> unit
    val retrieve_wlb_recommendations : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> ref_host_to_string_set_map
    val copy_bios_strings : rpc:(xml -> xml) -> session_id:ref_session -> vm:ref_VM -> host:ref_host -> unit
    val set_protection_policy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:ref_VMPP -> unit
    val set_start_delay : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_shutdown_delay : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_order : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:int64 -> unit
    val set_suspend_VDI : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:ref_VDI -> unit
    val assert_can_be_recovered : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> session_to:ref_session -> unit
    val recover : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> session_to:ref_session -> force:bool -> unit
    val import_convert : rpc:(xml -> xml) -> session_id:ref_session -> _type:string -> username:string -> password:string -> sr:ref_SR -> remote_config:string_to_string_map -> unit
    val set_appliance : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> value:ref_VM_appliance -> unit
    val query_services : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM -> string_to_string_map
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VM_to_vM_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_to_vM_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vM_t -> ref_VM
  end
  module VM_metrics : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> vM_metrics_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VM_metrics
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> string
    val get_memory_actual : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> int64
    val get_VCPUs_number : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> int64
    val get_VCPUs_utilisation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> int64_to_float_map
    val get_VCPUs_CPU : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> int64_to_int64_map
    val get_VCPUs_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> string_to_string_map
    val get_VCPUs_flags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> int64_to_string_set_map
    val get_state : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> string_set
    val get_start_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> datetime
    val get_install_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> datetime
    val get_last_updated : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> datetime
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_metrics -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_metrics_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VM_metrics_to_vM_metrics_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_metrics_to_vM_metrics_t_map
  end
  module VM_guest_metrics : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> vM_guest_metrics_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VM_guest_metrics
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> string
    val get_os_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> string_to_string_map
    val get_PV_drivers_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> string_to_string_map
    val get_PV_drivers_up_to_date : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> bool
    val get_memory : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> string_to_string_map
    val get_disks : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> string_to_string_map
    val get_networks : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> string_to_string_map
    val get_other : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> string_to_string_map
    val get_last_updated : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> datetime
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> string_to_string_map
    val get_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> bool
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_guest_metrics -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_guest_metrics_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VM_guest_metrics_to_vM_guest_metrics_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_guest_metrics_to_vM_guest_metrics_t_map
  end
  module VMPP : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> vMPP_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VMPP
    val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> is_policy_enabled:bool -> backup_type:vmpp_backup_type -> backup_retention_value:int64 -> backup_frequency:vmpp_backup_frequency -> backup_schedule:string_to_string_map -> archive_target_type:vmpp_archive_target_type -> archive_target_config:string_to_string_map -> archive_frequency:vmpp_archive_frequency -> archive_schedule:string_to_string_map -> is_alarm_enabled:bool -> alarm_config:string_to_string_map -> ref_VMPP
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> unit
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_VMPP_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> string
    val get_is_policy_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> bool
    val get_backup_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> vmpp_backup_type
    val get_backup_retention_value : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> int64
    val get_backup_frequency : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> vmpp_backup_frequency
    val get_backup_schedule : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> string_to_string_map
    val get_is_backup_running : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> bool
    val get_backup_last_run_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> datetime
    val get_archive_target_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> vmpp_archive_target_type
    val get_archive_target_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> string_to_string_map
    val get_archive_frequency : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> vmpp_archive_frequency
    val get_archive_schedule : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> string_to_string_map
    val get_is_archive_running : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> bool
    val get_archive_last_run_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> datetime
    val get_VMs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> ref_VM_set
    val get_is_alarm_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> bool
    val get_alarm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> string_to_string_map
    val get_recent_alerts : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> string_set
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:string -> unit
    val set_is_policy_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:bool -> unit
    val set_backup_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:vmpp_backup_type -> unit
    val protect_now : rpc:(xml -> xml) -> session_id:ref_session -> vmpp:ref_VMPP -> string
    val archive_now : rpc:(xml -> xml) -> session_id:ref_session -> snapshot:ref_VM -> string
    val create_alert : rpc:(xml -> xml) -> session_id:ref_session -> vmpp:ref_VMPP -> name:string -> priority:int64 -> body:string -> data:string -> unit
    val get_alerts : rpc:(xml -> xml) -> session_id:ref_session -> vmpp:ref_VMPP -> hours_from_now:int64 -> string_set
    val set_backup_retention_value : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:int64 -> unit
    val set_is_backup_running : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:bool -> unit
    val set_is_archive_running : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:bool -> unit
    val set_backup_frequency : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:vmpp_backup_frequency -> unit
    val set_backup_schedule : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:string_to_string_map -> unit
    val set_archive_frequency : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:vmpp_archive_frequency -> unit
    val set_archive_schedule : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:string_to_string_map -> unit
    val set_archive_target_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:vmpp_archive_target_type -> unit
    val set_archive_target_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:string_to_string_map -> unit
    val set_is_alarm_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:bool -> unit
    val set_alarm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:string_to_string_map -> unit
    val add_to_backup_schedule : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> key:string -> value:string -> unit
    val add_to_archive_target_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> key:string -> value:string -> unit
    val add_to_archive_schedule : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> key:string -> value:string -> unit
    val add_to_alarm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> key:string -> value:string -> unit
    val remove_from_backup_schedule : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> key:string -> unit
    val remove_from_archive_target_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> key:string -> unit
    val remove_from_archive_schedule : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> key:string -> unit
    val remove_from_alarm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> key:string -> unit
    val set_backup_last_run_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:datetime -> unit
    val set_archive_last_run_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VMPP -> value:datetime -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VMPP_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VMPP_to_vMPP_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VMPP_to_vMPP_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vMPP_t -> ref_VMPP
  end
  module VM_appliance : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> vM_appliance_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VM_appliance
    val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> ref_VM_appliance
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> unit
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_VM_appliance_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> string
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> vm_appliance_operation_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> string_to_vm_appliance_operation_map
    val get_VMs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> ref_VM_set
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> value:string -> unit
    val start : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> paused:bool -> unit
    val clean_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> unit
    val hard_shutdown : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> unit
    val shutdown : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> unit
    val assert_can_be_recovered : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> session_to:ref_session -> unit
    val recover : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VM_appliance -> session_to:ref_session -> force:bool -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_appliance_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VM_appliance_to_vM_appliance_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VM_appliance_to_vM_appliance_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vM_appliance_t -> ref_VM_appliance
  end
  module DR_task : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_DR_task -> dR_task_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_DR_task
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_DR_task -> string
    val get_introduced_SRs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_DR_task -> ref_SR_set
    val create : rpc:(xml -> xml) -> session_id:ref_session -> _type:string -> device_config:string_to_string_map -> whitelist:string_set -> ref_DR_task
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_DR_task -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_DR_task_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_DR_task_to_dR_task_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_DR_task_to_dR_task_t_map
  end
  module Host : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> host_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_host
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_host_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_memory_overhead : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> int64
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> host_allowed_operations_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_host_allowed_operations_map
    val get_API_version_major : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> int64
    val get_API_version_minor : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> int64
    val get_API_version_vendor : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_API_version_vendor_implementation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> bool
    val get_software_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_capabilities : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_set
    val get_cpu_configuration : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_sched_policy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_supported_bootloaders : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_set
    val get_resident_VMs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_VM_set
    val get_logging : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_PIFs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_PIF_set
    val get_suspend_image_sr : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_SR
    val get_crash_dump_sr : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_SR
    val get_crashdumps : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_host_crashdump_set
    val get_patches : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_host_patch_set
    val get_PBDs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_PBD_set
    val get_host_CPUs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_host_cpu_set
    val get_cpu_info : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_hostname : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_address : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_metrics : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_host_metrics
    val get_license_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_ha_statefiles : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_set
    val get_ha_network_peers : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_set
    val get_blobs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_ref_blob_map
    val get_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_set
    val get_external_auth_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_external_auth_service_name : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_external_auth_configuration : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_edition : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_license_server : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_bios_strings : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_power_on_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string
    val get_power_on_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_local_cache_sr : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_SR
    val get_chipset_info : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_to_string_map
    val get_PCIs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_PCI_set
    val get_PGPUs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_PGPU_set
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> key:string -> unit
    val set_logging : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string_to_string_map -> unit
    val add_to_logging : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> key:string -> value:string -> unit
    val remove_from_logging : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> key:string -> unit
    val set_suspend_image_sr : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:ref_SR -> unit
    val set_crash_dump_sr : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:ref_SR -> unit
    val set_hostname : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string -> unit
    val set_address : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string -> unit
    val set_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string_set -> unit
    val add_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string -> unit
    val remove_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string -> unit
    val set_license_server : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string_to_string_map -> unit
    val add_to_license_server : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> key:string -> value:string -> unit
    val remove_from_license_server : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> key:string -> unit
    val disable : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val enable : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val shutdown : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val reboot : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val dmesg : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string
    val dmesg_clear : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string
    val get_log : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string
    val send_debug_keys : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> keys:string -> unit
    val bugreport_upload : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> url:string -> options:string_to_string_map -> unit
    val list_methods : rpc:(xml -> xml) -> session_id:ref_session -> string_set
    val license_apply : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> contents:string -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> hostname:string -> address:string -> external_auth_type:string -> external_auth_service_name:string -> external_auth_configuration:string_to_string_map -> license_params:string_to_string_map -> edition:string -> license_server:string_to_string_map -> local_cache_sr:ref_SR -> chipset_info:string_to_string_map -> ref_host
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> unit
    val power_on : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val set_license_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> value:string_to_string_map -> unit
    val emergency_ha_disable : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val ha_disarm_fencing : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val preconfigure_ha : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> statefiles:ref_VDI_set -> metadata_vdi:ref_VDI -> generation:string -> unit
    val ha_join_liveset : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val ha_disable_failover_decisions : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val ha_wait_for_shutdown_via_statefile : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val ha_stop_daemon : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val ha_release_resources : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val ha_xapi_healthcheck : rpc:(xml -> xml) -> session_id:ref_session -> bool
    val local_assert_healthy : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val request_backup : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> generation:int64 -> force:bool -> unit
    val request_config_file_sync : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> hash:string -> unit
    val propose_new_master : rpc:(xml -> xml) -> session_id:ref_session -> address:string -> manual:bool -> unit
    val commit_new_master : rpc:(xml -> xml) -> session_id:ref_session -> address:string -> unit
    val abort_new_master : rpc:(xml -> xml) -> session_id:ref_session -> address:string -> unit
    val get_data_sources : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> data_source_t_set
    val record_data_source : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> data_source:string -> unit
    val query_data_source : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> data_source:string -> float
    val forget_data_source_archives : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> data_source:string -> unit
    val assert_can_evacuate : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val get_vms_which_prevent_evacuation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_VM_to_string_set_map
    val get_uncooperative_resident_VMs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_VM_set
    val get_uncooperative_domains : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> string_set
    val evacuate : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val signal_networking_change : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val notify : rpc:(xml -> xml) -> session_id:ref_session -> ty:string -> params:string -> unit
    val syslog_reconfigure : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val management_reconfigure : rpc:(xml -> xml) -> session_id:ref_session -> pif:ref_PIF -> unit
    val local_management_reconfigure : rpc:(xml -> xml) -> session_id:ref_session -> interface:string -> unit
    val management_disable : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val get_management_interface : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> ref_PIF
    val get_system_status_capabilities : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string
    val get_diagnostic_timing_stats : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string_to_string_map
    val restart_agent : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val shutdown_agent : rpc:(xml -> xml) -> session_id:ref_session -> unit
    val set_hostname_live : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> hostname:string -> unit
    val is_in_emergency_mode : rpc:(xml -> xml) -> session_id:ref_session -> bool
    val compute_free_memory : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> int64
    val compute_memory_overhead : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> int64
    val tickle_heartbeat : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> stuff:string_to_string_map -> string_to_string_map
    val sync_data : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val backup_rrds : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> delay:float -> unit
    val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> mime_type:string -> public:bool -> ref_blob
    val call_plugin : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> plugin:string -> fn:string -> args:string_to_string_map -> string
    val get_servertime : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> datetime
    val get_server_localtime : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> datetime
    val enable_binary_storage : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val disable_binary_storage : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val enable_external_auth : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> config:string_to_string_map -> service_name:string -> auth_type:string -> unit
    val disable_external_auth : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> config:string_to_string_map -> unit
    val retrieve_wlb_evacuate_recommendations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> ref_VM_to_string_set_map
    val certificate_install : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> cert:string -> unit
    val certificate_uninstall : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> unit
    val certificate_list : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string_set
    val crl_install : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> crl:string -> unit
    val crl_uninstall : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> name:string -> unit
    val crl_list : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string_set
    val certificate_sync : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val get_server_certificate : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string
    val update_pool_secret : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> pool_secret:string -> unit
    val update_master : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> master_address:string -> unit
    val attach_static_vdis : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vdi_reason_map:ref_VDI_to_string_map -> unit
    val detach_static_vdis : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vdis:ref_VDI_set -> unit
    val set_localdb_key : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> key:string -> value:string -> unit
    val apply_edition : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> edition:string -> unit
    val refresh_pack_info : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val set_power_on_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host -> power_on_mode:string -> power_on_config:string_to_string_map -> unit
    val set_cpu_features : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> features:string -> unit
    val reset_cpu_features : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val reset_networking : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val enable_local_storage_caching : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> sr:ref_SR -> unit
    val disable_local_storage_caching : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val get_sm_diagnostics : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string
    val get_thread_diagnostics : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> string
    val sm_dp_destroy : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> dp:string -> allow_leak:bool -> unit
    val sync_vlans : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val sync_tunnels : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val sync_pif_currently_attached : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> bridges:string_set -> unit
    val migrate_receive : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> network:ref_network -> options:string_to_string_map -> string_to_string_map
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_host_to_host_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_to_host_t_map
  end
  module Host_crashdump : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> host_crashdump_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_host_crashdump
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> string
    val get_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> ref_host
    val get_timestamp : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> datetime
    val get_size : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> int64
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> key:string -> unit
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> unit
    val upload : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_crashdump -> url:string -> options:string_to_string_map -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_crashdump_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_host_crashdump_to_host_crashdump_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_crashdump_to_host_crashdump_t_map
  end
  module Host_patch : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> host_patch_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_host_patch
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_host_patch_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> string
    val get_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> string
    val get_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> ref_host
    val get_applied : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> bool
    val get_timestamp_applied : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> datetime
    val get_size : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> int64
    val get_pool_patch : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> ref_pool_patch
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> key:string -> unit
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> unit
    val apply : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_patch -> string
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_patch_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_host_patch_to_host_patch_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_patch_to_host_patch_t_map
  end
  module Host_metrics : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> host_metrics_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_host_metrics
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> string
    val get_memory_total : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> int64
    val get_memory_free : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> int64
    val get_live : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> bool
    val get_last_updated : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> datetime
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_metrics -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_metrics_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_host_metrics_to_host_metrics_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_metrics_to_host_metrics_t_map
  end
  module Host_cpu : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> host_cpu_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_host_cpu
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> string
    val get_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> ref_host
    val get_number : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> int64
    val get_vendor : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> string
    val get_speed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> int64
    val get_modelname : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> string
    val get_family : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> int64
    val get_model : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> int64
    val get_stepping : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> string
    val get_flags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> string
    val get_features : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> string
    val get_utilisation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> float
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_host_cpu -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_cpu_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_host_cpu_to_host_cpu_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_host_cpu_to_host_cpu_t_map
  end
  module Network : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> network_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_network
    val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> mTU:int64 -> other_config:string_to_string_map -> tags:string_set -> ref_network
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> unit
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_network_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> string
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> network_operations_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> string_to_network_operations_map
    val get_VIFs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> ref_VIF_set
    val get_PIFs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> ref_PIF_set
    val get_MTU : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> int64
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> string_to_string_map
    val get_bridge : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> string
    val get_blobs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> string_to_ref_blob_map
    val get_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> string_set
    val get_default_locking_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> network_default_locking_mode
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> value:string -> unit
    val set_MTU : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> value:int64 -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> key:string -> unit
    val set_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> value:string_set -> unit
    val add_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> value:string -> unit
    val remove_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_network -> value:string -> unit
    val attach : rpc:(xml -> xml) -> session_id:ref_session -> network:ref_network -> host:ref_host -> unit
    val pool_introduce : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> mTU:int64 -> other_config:string_to_string_map -> bridge:string -> ref_network
    val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> network:ref_network -> name:string -> mime_type:string -> public:bool -> ref_blob
    val set_default_locking_mode : rpc:(xml -> xml) -> session_id:ref_session -> network:ref_network -> value:network_default_locking_mode -> unit
    val attach_for_vm : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vm:ref_VM -> unit
    val detach_for_vm : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vm:ref_VM -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_network_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_network_to_network_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_network_to_network_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:network_t -> ref_network
  end
  module VIF : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> vIF_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VIF
    val create : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> vM:ref_VM -> mAC:string -> mTU:int64 -> other_config:string_to_string_map -> qos_algorithm_type:string -> qos_algorithm_params:string_to_string_map -> ref_VIF
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> unit
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> vif_operations_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string_to_vif_operations_map
    val get_device : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string
    val get_network : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> ref_network
    val get_VM : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> ref_VM
    val get_MAC : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string
    val get_MTU : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> int64
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string_to_string_map
    val get_currently_attached : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> bool
    val get_status_code : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> int64
    val get_status_detail : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string
    val get_runtime_properties : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string_to_string_map
    val get_qos_algorithm_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string
    val get_qos_algorithm_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string_to_string_map
    val get_qos_supported_algorithms : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string_set
    val get_metrics : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> ref_VIF_metrics
    val get_MAC_autogenerated : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> bool
    val get_locking_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> vif_locking_mode
    val get_ipv4_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string_set
    val get_ipv6_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> string_set
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> key:string -> unit
    val set_qos_algorithm_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> unit
    val set_qos_algorithm_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string_to_string_map -> unit
    val add_to_qos_algorithm_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> key:string -> value:string -> unit
    val remove_from_qos_algorithm_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> key:string -> unit
    val plug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> unit
    val unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> unit
    val unplug_force : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> unit
    val set_locking_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:vif_locking_mode -> unit
    val set_ipv4_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string_set -> unit
    val add_ipv4_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> unit
    val remove_ipv4_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> unit
    val set_ipv6_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string_set -> unit
    val add_ipv6_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> unit
    val remove_ipv6_allowed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF -> value:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VIF_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VIF_to_vIF_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VIF_to_vIF_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vIF_t -> ref_VIF
  end
  module VIF_metrics : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> vIF_metrics_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VIF_metrics
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> string
    val get_io_read_kbs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> float
    val get_io_write_kbs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> float
    val get_last_updated : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> datetime
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VIF_metrics -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VIF_metrics_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VIF_metrics_to_vIF_metrics_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VIF_metrics_to_vIF_metrics_t_map
  end
  module PIF : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> pIF_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_PIF
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string
    val get_device : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string
    val get_network : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_network
    val get_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_host
    val get_MAC : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string
    val get_MTU : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> int64
    val get_VLAN : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> int64
    val get_metrics : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_PIF_metrics
    val get_physical : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> bool
    val get_currently_attached : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> bool
    val get_ip_configuration_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ip_configuration_mode
    val get_IP : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string
    val get_netmask : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string
    val get_gateway : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string
    val get_DNS : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string
    val get_bond_slave_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_Bond
    val get_bond_master_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_Bond_set
    val get_VLAN_master_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_VLAN
    val get_VLAN_slave_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_VLAN_set
    val get_management : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> bool
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string_to_string_map
    val get_disallow_unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> bool
    val get_tunnel_access_PIF_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_tunnel_set
    val get_tunnel_transport_PIF_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ref_tunnel_set
    val get_ipv6_configuration_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> ipv6_configuration_mode
    val get_IPv6 : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string_set
    val get_ipv6_gateway : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> string
    val get_primary_address_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> primary_address_type
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> key:string -> unit
    val set_disallow_unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> value:bool -> unit
    val create_VLAN : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> host:ref_host -> vLAN:int64 -> ref_PIF
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> unit
    val reconfigure_ip : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> mode:ip_configuration_mode -> iP:string -> netmask:string -> gateway:string -> dNS:string -> unit
    val reconfigure_ipv6 : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> mode:ipv6_configuration_mode -> iPv6:string -> gateway:string -> dNS:string -> unit
    val set_primary_address_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> primary_address_type:primary_address_type -> unit
    val scan : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> unit
    val introduce : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> mAC:string -> device:string -> ref_PIF
    val forget : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> unit
    val unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> unit
    val plug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> unit
    val pool_introduce : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> host:ref_host -> mAC:string -> mTU:int64 -> vLAN:int64 -> physical:bool -> ip_configuration_mode:ip_configuration_mode -> iP:string -> netmask:string -> gateway:string -> dNS:string -> bond_slave_of:ref_Bond -> vLAN_master_of:ref_VLAN -> management:bool -> other_config:string_to_string_map -> disallow_unplug:bool -> ipv6_configuration_mode:ipv6_configuration_mode -> iPv6:string_set -> ipv6_gateway:string -> primary_address_type:primary_address_type -> ref_PIF
    val db_introduce : rpc:(xml -> xml) -> session_id:ref_session -> device:string -> network:ref_network -> host:ref_host -> mAC:string -> mTU:int64 -> vLAN:int64 -> physical:bool -> ip_configuration_mode:ip_configuration_mode -> iP:string -> netmask:string -> gateway:string -> dNS:string -> bond_slave_of:ref_Bond -> vLAN_master_of:ref_VLAN -> management:bool -> other_config:string_to_string_map -> disallow_unplug:bool -> ipv6_configuration_mode:ipv6_configuration_mode -> iPv6:string_set -> ipv6_gateway:string -> primary_address_type:primary_address_type -> ref_PIF
    val db_forget : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_PIF_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_PIF_to_pIF_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_PIF_to_pIF_t_map
  end
  module PIF_metrics : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> pIF_metrics_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_PIF_metrics
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> string
    val get_io_read_kbs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> float
    val get_io_write_kbs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> float
    val get_carrier : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> bool
    val get_vendor_id : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> string
    val get_vendor_name : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> string
    val get_device_id : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> string
    val get_device_name : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> string
    val get_speed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> int64
    val get_duplex : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> bool
    val get_pci_bus_path : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> string
    val get_last_updated : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> datetime
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PIF_metrics -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_PIF_metrics_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_PIF_metrics_to_pIF_metrics_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_PIF_metrics_to_pIF_metrics_t_map
  end
  module Bond : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> bond_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_Bond
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> string
    val get_master : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> ref_PIF
    val get_slaves : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> ref_PIF_set
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> string_to_string_map
    val get_primary_slave : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> ref_PIF
    val get_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> bond_mode
    val get_properties : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> string_to_string_map
    val get_links_up : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> int64
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> key:string -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> network:ref_network -> members:ref_PIF_set -> mAC:string -> mode:bond_mode -> properties:string_to_string_map -> ref_Bond
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> unit
    val set_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> value:bond_mode -> unit
    val set_property : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_Bond -> name:string -> value:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_Bond_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_Bond_to_bond_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_Bond_to_bond_t_map
  end
  module VLAN : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> vLAN_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VLAN
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> string
    val get_tagged_PIF : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> ref_PIF
    val get_untagged_PIF : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> ref_PIF
    val get_tag : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> int64
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> key:string -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> tagged_PIF:ref_PIF -> tag:int64 -> network:ref_network -> ref_VLAN
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VLAN -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VLAN_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VLAN_to_vLAN_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VLAN_to_vLAN_t_map
  end
  module SM : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> sM_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_SM
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_SM_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val get_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val get_vendor : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val get_copyright : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val get_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val get_required_api_version : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val get_configuration : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string_to_string_map
    val get_capabilities : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string_set
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string_to_string_map
    val get_driver_filename : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> string
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SM -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_SM_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_SM_to_sM_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_SM_to_sM_t_map
  end
  module SR : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> sR_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_SR
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_SR_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> storage_operations_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string_to_storage_operations_map
    val get_VDIs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> ref_VDI_set
    val get_PBDs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> ref_PBD_set
    val get_virtual_allocation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> int64
    val get_physical_utilisation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> int64
    val get_physical_size : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> int64
    val get_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string
    val get_content_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string
    val get_shared : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> bool
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string_to_string_map
    val get_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string_set
    val get_sm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string_to_string_map
    val get_blobs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> string_to_ref_blob_map
    val get_local_cache_enabled : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> bool
    val get_introduced_by : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> ref_DR_task
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> key:string -> unit
    val set_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> value:string_set -> unit
    val add_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> value:string -> unit
    val remove_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> value:string -> unit
    val set_sm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> value:string_to_string_map -> unit
    val add_to_sm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> key:string -> value:string -> unit
    val remove_from_sm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> key:string -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> device_config:string_to_string_map -> physical_size:int64 -> name_label:string -> name_description:string -> _type:string -> content_type:string -> shared:bool -> sm_config:string_to_string_map -> ref_SR
    val introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> _type:string -> content_type:string -> shared:bool -> sm_config:string_to_string_map -> ref_SR
    val make : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> device_config:string_to_string_map -> physical_size:int64 -> name_label:string -> name_description:string -> _type:string -> content_type:string -> sm_config:string_to_string_map -> string
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val forget : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val update : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val get_supported_types : rpc:(xml -> xml) -> session_id:ref_session -> string_set
    val scan : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val probe : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> device_config:string_to_string_map -> _type:string -> sm_config:string_to_string_map -> string
    val set_shared : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> value:bool -> unit
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> value:string -> unit
    val create_new_blob : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> name:string -> mime_type:string -> public:bool -> ref_blob
    val set_physical_size : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> value:int64 -> unit
    val set_virtual_allocation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> value:int64 -> unit
    val set_physical_utilisation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_SR -> value:int64 -> unit
    val assert_can_host_ha_statefile : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val assert_supports_database_replication : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val enable_database_replication : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val disable_database_replication : rpc:(xml -> xml) -> session_id:ref_session -> sr:ref_SR -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_SR_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_SR_to_sR_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_SR_to_sR_t_map
  end
  module VDI : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> vDI_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VDI
    val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> sR:ref_SR -> virtual_size:int64 -> _type:vdi_type -> sharable:bool -> read_only:bool -> other_config:string_to_string_map -> xenstore_data:string_to_string_map -> sm_config:string_to_string_map -> tags:string_set -> ref_VDI
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> unit
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_VDI_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> vdi_operations_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string_to_vdi_operations_map
    val get_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_SR
    val get_VBDs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_VBD_set
    val get_crash_dumps : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_crashdump_set
    val get_virtual_size : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> int64
    val get_physical_utilisation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> int64
    val get_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> vdi_type
    val get_sharable : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> bool
    val get_read_only : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> bool
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string_to_string_map
    val get_storage_lock : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> bool
    val get_location : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string
    val get_managed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> bool
    val get_missing : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> bool
    val get_parent : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_VDI
    val get_xenstore_data : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string_to_string_map
    val get_sm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string_to_string_map
    val get_is_a_snapshot : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> bool
    val get_snapshot_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_VDI
    val get_snapshots : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_VDI_set
    val get_snapshot_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> datetime
    val get_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string_set
    val get_allow_caching : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> bool
    val get_on_boot : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> on_boot
    val get_metadata_of_pool : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_pool
    val get_metadata_latest : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> bool
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> key:string -> unit
    val set_xenstore_data : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string_to_string_map -> unit
    val add_to_xenstore_data : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> key:string -> value:string -> unit
    val remove_from_xenstore_data : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> key:string -> unit
    val set_sm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string_to_string_map -> unit
    val add_to_sm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> key:string -> value:string -> unit
    val remove_from_sm_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> key:string -> unit
    val set_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string_set -> unit
    val add_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string -> unit
    val remove_tags : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string -> unit
    val snapshot : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> driver_params:string_to_string_map -> ref_VDI
    val clone : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> driver_params:string_to_string_map -> ref_VDI
    val resize : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> size:int64 -> unit
    val resize_online : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> size:int64 -> unit
    val introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> sR:ref_SR -> _type:vdi_type -> sharable:bool -> read_only:bool -> other_config:string_to_string_map -> location:string -> xenstore_data:string_to_string_map -> sm_config:string_to_string_map -> managed:bool -> virtual_size:int64 -> physical_utilisation:int64 -> metadata_of_pool:ref_pool -> is_a_snapshot:bool -> snapshot_time:datetime -> snapshot_of:ref_VDI -> ref_VDI
    val pool_introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> sR:ref_SR -> _type:vdi_type -> sharable:bool -> read_only:bool -> other_config:string_to_string_map -> location:string -> xenstore_data:string_to_string_map -> sm_config:string_to_string_map -> managed:bool -> virtual_size:int64 -> physical_utilisation:int64 -> metadata_of_pool:ref_pool -> is_a_snapshot:bool -> snapshot_time:datetime -> snapshot_of:ref_VDI -> ref_VDI
    val db_introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> name_label:string -> name_description:string -> sR:ref_SR -> _type:vdi_type -> sharable:bool -> read_only:bool -> other_config:string_to_string_map -> location:string -> xenstore_data:string_to_string_map -> sm_config:string_to_string_map -> managed:bool -> virtual_size:int64 -> physical_utilisation:int64 -> metadata_of_pool:ref_pool -> is_a_snapshot:bool -> snapshot_time:datetime -> snapshot_of:ref_VDI -> ref_VDI
    val db_forget : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> unit
    val update : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> unit
    val copy : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> sr:ref_SR -> ref_VDI
    val force_unlock : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> unit
    val set_managed : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:bool -> unit
    val forget : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> unit
    val set_sharable : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:bool -> unit
    val set_read_only : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:bool -> unit
    val set_missing : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:bool -> unit
    val set_virtual_size : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:int64 -> unit
    val set_physical_utilisation : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:int64 -> unit
    val set_is_a_snapshot : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:bool -> unit
    val set_snapshot_of : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:ref_VDI -> unit
    val set_snapshot_time : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:datetime -> unit
    val set_metadata_of_pool : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:ref_pool -> unit
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:string -> unit
    val generate_config : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> vdi:ref_VDI -> string
    val set_on_boot : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:on_boot -> unit
    val set_allow_caching : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> value:bool -> unit
    val open_database : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> ref_session
    val checksum : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string
    val read_database_pool_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VDI -> string
    val pool_migrate : rpc:(xml -> xml) -> session_id:ref_session -> vdi:ref_VDI -> sr:ref_SR -> options:string_to_string_map -> ref_VDI
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VDI_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VDI_to_vDI_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VDI_to_vDI_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vDI_t -> ref_VDI
  end
  module VBD : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> vBD_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VBD
    val create : rpc:(xml -> xml) -> session_id:ref_session -> vM:ref_VM -> vDI:ref_VDI -> userdevice:string -> bootable:bool -> mode:vbd_mode -> _type:vbd_type -> unpluggable:bool -> empty:bool -> other_config:string_to_string_map -> qos_algorithm_type:string -> qos_algorithm_params:string_to_string_map -> ref_VBD
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> unit
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string
    val get_allowed_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> vbd_operations_set
    val get_current_operations : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string_to_vbd_operations_map
    val get_VM : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_VM
    val get_VDI : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_VDI
    val get_device : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string
    val get_userdevice : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string
    val get_bootable : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> bool
    val get_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> vbd_mode
    val get_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> vbd_type
    val get_unpluggable : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> bool
    val get_storage_lock : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> bool
    val get_empty : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> bool
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string_to_string_map
    val get_currently_attached : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> bool
    val get_status_code : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> int64
    val get_status_detail : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string
    val get_runtime_properties : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string_to_string_map
    val get_qos_algorithm_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string
    val get_qos_algorithm_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string_to_string_map
    val get_qos_supported_algorithms : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string_set
    val get_metrics : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> ref_VBD_metrics
    val set_userdevice : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> value:string -> unit
    val set_bootable : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> value:bool -> unit
    val set_mode : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> value:vbd_mode -> unit
    val set_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> value:vbd_type -> unit
    val set_unpluggable : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> value:bool -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> key:string -> unit
    val set_qos_algorithm_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> value:string -> unit
    val set_qos_algorithm_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> value:string_to_string_map -> unit
    val add_to_qos_algorithm_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> key:string -> value:string -> unit
    val remove_from_qos_algorithm_params : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> key:string -> unit
    val eject : rpc:(xml -> xml) -> session_id:ref_session -> vbd:ref_VBD -> unit
    val insert : rpc:(xml -> xml) -> session_id:ref_session -> vbd:ref_VBD -> vdi:ref_VDI -> unit
    val plug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> unit
    val unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> unit
    val unplug_force : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> unit
    val unplug_force_no_safety_check : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> unit
    val assert_attachable : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> unit
    val pause : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> string
    val unpause : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD -> token:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VBD_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VBD_to_vBD_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VBD_to_vBD_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vBD_t -> ref_VBD
  end
  module VBD_metrics : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> vBD_metrics_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VBD_metrics
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> string
    val get_io_read_kbs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> float
    val get_io_write_kbs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> float
    val get_last_updated : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> datetime
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VBD_metrics -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VBD_metrics_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VBD_metrics_to_vBD_metrics_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VBD_metrics_to_vBD_metrics_t_map
  end
  module PBD : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> pBD_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_PBD
    val create : rpc:(xml -> xml) -> session_id:ref_session -> host:ref_host -> sR:ref_SR -> device_config:string_to_string_map -> other_config:string_to_string_map -> ref_PBD
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> unit
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> string
    val get_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> ref_host
    val get_SR : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> ref_SR
    val get_device_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> string_to_string_map
    val get_currently_attached : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> bool
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> key:string -> unit
    val plug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> unit
    val unplug : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> unit
    val set_device_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PBD -> value:string_to_string_map -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_PBD_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_PBD_to_pBD_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_PBD_to_pBD_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:pBD_t -> ref_PBD
  end
  module Crashdump : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> crashdump_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_crashdump
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> string
    val get_VM : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> ref_VM
    val get_VDI : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> ref_VDI
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> key:string -> unit
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_crashdump -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_crashdump_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_crashdump_to_crashdump_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_crashdump_to_crashdump_t_map
  end
  module VTPM : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VTPM -> vTPM_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VTPM
    val create : rpc:(xml -> xml) -> session_id:ref_session -> vM:ref_VM -> backend:ref_VM -> ref_VTPM
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VTPM -> unit
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VTPM -> string
    val get_VM : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VTPM -> ref_VM
    val get_backend : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VTPM -> ref_VM
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:vTPM_t -> ref_VTPM
  end
  module Console : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> console_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_console
    val create : rpc:(xml -> xml) -> session_id:ref_session -> other_config:string_to_string_map -> ref_console
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> unit
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> string
    val get_protocol : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> console_protocol
    val get_location : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> string
    val get_VM : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> ref_VM
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_console -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_console_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_console_to_console_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_console_to_console_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:console_t -> ref_console
  end
  module User : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> user_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_user
    val create : rpc:(xml -> xml) -> session_id:ref_session -> short_name:string -> fullname:string -> other_config:string_to_string_map -> ref_user
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> unit
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> string
    val get_short_name : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> string
    val get_fullname : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> string
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> string_to_string_map
    val set_fullname : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> value:string -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_user -> key:string -> unit
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:user_t -> ref_user
  end
  module Data_source : sig
  end
  module Blob : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> blob_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_blob
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_blob_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> string
    val get_size : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> int64
    val get_public : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> bool
    val get_last_updated : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> datetime
    val get_mime_type : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> string
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> value:string -> unit
    val set_public : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> value:bool -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> mime_type:string -> public:bool -> ref_blob
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_blob -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_blob_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_blob_to_blob_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_blob_to_blob_t_map
  end
  module Message : sig
    val create : rpc:(xml -> xml) -> session_id:ref_session -> name:string -> priority:int64 -> cls:cls -> obj_uuid:string -> body:string -> ref_message
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_message -> unit
    val get : rpc:(xml -> xml) -> session_id:ref_session -> cls:cls -> obj_uuid:string -> since:datetime -> ref_message_to_message_t_map
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_message_set
    val get_since : rpc:(xml -> xml) -> session_id:ref_session -> since:datetime -> ref_message_to_message_t_map
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_message -> message_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_message
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_message_to_message_t_map
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_message_to_message_t_map
  end
  module Secret : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> secret_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_secret
    val create : rpc:(xml -> xml) -> session_id:ref_session -> value:string -> other_config:string_to_string_map -> ref_secret
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> unit
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> string
    val get_value : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> string
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> string_to_string_map
    val set_value : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> value:string -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_secret -> key:string -> unit
    val introduce : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> value:string -> other_config:string_to_string_map -> ref_secret
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_secret_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_secret_to_secret_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_secret_to_secret_t_map
    val create_from_record : rpc:(xml -> xml) -> session_id:ref_session -> value:secret_t -> ref_secret
  end
  module Tunnel : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> tunnel_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_tunnel
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> string
    val get_access_PIF : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> ref_PIF
    val get_transport_PIF : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> ref_PIF
    val get_status : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> string_to_string_map
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> string_to_string_map
    val set_status : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> value:string_to_string_map -> unit
    val add_to_status : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> key:string -> value:string -> unit
    val remove_from_status : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> key:string -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> key:string -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> transport_PIF:ref_PIF -> network:ref_network -> ref_tunnel
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_tunnel -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_tunnel_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_tunnel_to_tunnel_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_tunnel_to_tunnel_t_map
  end
  module PCI : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> pCI_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_PCI
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> string
    val get_class_name : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> string
    val get_vendor_name : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> string
    val get_device_name : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> string
    val get_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> ref_host
    val get_pci_id : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> string
    val get_dependencies : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> ref_PCI_set
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PCI -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_PCI_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_PCI_to_pCI_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_PCI_to_pCI_t_map
  end
  module PGPU : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> pGPU_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_PGPU
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> string
    val get_PCI : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> ref_PCI
    val get_GPU_group : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> ref_GPU_group
    val get_host : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> ref_host
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_PGPU -> key:string -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_PGPU_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_PGPU_to_pGPU_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_PGPU_to_pGPU_t_map
  end
  module GPU_group : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> gPU_group_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_GPU_group
    val get_by_name_label : rpc:(xml -> xml) -> session_id:ref_session -> label:string -> ref_GPU_group_set
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> string
    val get_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> string
    val get_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> string
    val get_PGPUs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> ref_PGPU_set
    val get_VGPUs : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> ref_VGPU_set
    val get_GPU_types : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> string_set
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> string_to_string_map
    val set_name_label : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> value:string -> unit
    val set_name_description : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> value:string -> unit
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> key:string -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> name_label:string -> name_description:string -> other_config:string_to_string_map -> ref_GPU_group
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_GPU_group -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_GPU_group_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_GPU_group_to_gPU_group_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_GPU_group_to_gPU_group_t_map
  end
  module VGPU : sig
    val get_record : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> vGPU_t
    val get_by_uuid : rpc:(xml -> xml) -> session_id:ref_session -> uuid:string -> ref_VGPU
    val get_uuid : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> string
    val get_VM : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> ref_VM
    val get_GPU_group : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> ref_GPU_group
    val get_device : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> string
    val get_currently_attached : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> bool
    val get_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> string_to_string_map
    val set_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> value:string_to_string_map -> unit
    val add_to_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> key:string -> value:string -> unit
    val remove_from_other_config : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> key:string -> unit
    val create : rpc:(xml -> xml) -> session_id:ref_session -> vM:ref_VM -> gPU_group:ref_GPU_group -> device:string -> other_config:string_to_string_map -> ref_VGPU
    val destroy : rpc:(xml -> xml) -> session_id:ref_session -> self:ref_VGPU -> unit
    val get_all : rpc:(xml -> xml) -> session_id:ref_session -> ref_VGPU_set
    val get_all_records_where : rpc:(xml -> xml) -> session_id:ref_session -> expr:string -> ref_VGPU_to_vGPU_t_map
    val get_all_records : rpc:(xml -> xml) -> session_id:ref_session -> ref_VGPU_to_vGPU_t_map
  end

end

