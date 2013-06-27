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
exception Server_error of string * string list

let to_string = function
  | Server_error (name, args) ->
      Printf.sprintf "Server_error(%s, [ %a ])" name (fun () -> String.concat "; ") args
  | e -> Printexc.to_string e

let message_deprecated = "MESSAGE_DEPRECATED"
let message_removed = "MESSAGE_REMOVED"

let permission_denied = "PERMISSION_DENIED"
let internal_error = "INTERNAL_ERROR"
let map_duplicate_key = "MAP_DUPLICATE_KEY"
let db_uniqueness_constraint_violation = "DB_UNIQUENESS_CONSTRAINT_VIOLATION"
let location_not_unique = "LOCATION_NOT_UNIQUE"
let message_method_unknown = "MESSAGE_METHOD_UNKNOWN"
let message_parameter_count_mismatch = "MESSAGE_PARAMETER_COUNT_MISMATCH"
let value_not_supported = "VALUE_NOT_SUPPORTED"
let invalid_value = "INVALID_VALUE"
let memory_constraint_violation = "MEMORY_CONSTRAINT_VIOLATION"
let field_type_error = "FIELD_TYPE_ERROR"
let session_authentication_failed = "SESSION_AUTHENTICATION_FAILED"
let session_invalid = "SESSION_INVALID"
let change_password_rejected = "CHANGE_PASSWORD_REJECTED"
let user_is_not_local_superuser = "USER_IS_NOT_LOCAL_SUPERUSER"
let cannot_contact_host = "CANNOT_CONTACT_HOST"
let not_supported_during_upgrade = "NOT_SUPPORTED_DURING_UPGRADE"

let handle_invalid = "HANDLE_INVALID"
let uuid_invalid = "UUID_INVALID"

let vm_hvm_required = "VM_HVM_REQUIRED"
let vm_no_vcpus = "VM_NO_VCPUS"
let vm_toomany_vcpus = "VM_TOO_MANY_VCPUS"
let vm_is_protected = "VM_IS_PROTECTED"

let host_in_use = "HOST_IN_USE"
let host_in_emergency_mode = "HOST_IN_EMERGENCY_MODE"
let host_cannot_read_metrics = "HOST_CANNOT_READ_METRICS"
let host_disabled = "HOST_DISABLED"
let host_disabled_until_reboot = "HOST_DISABLED_UNTIL_REBOOT"
let host_not_disabled = "HOST_NOT_DISABLED"
let host_not_live = "HOST_NOT_LIVE"
let host_is_live = "HOST_IS_LIVE"
let host_power_on_mode_disabled="HOST_POWER_ON_MODE_DISABLED"
let host_not_enough_free_memory = "HOST_NOT_ENOUGH_FREE_MEMORY"
let no_hosts_available = "NO_HOSTS_AVAILABLE"
let host_offline = "HOST_OFFLINE"
let host_cannot_destroy_self = "HOST_CANNOT_DESTROY_SELF"
let host_is_slave = "HOST_IS_SLAVE"
let host_name_invalid = "HOST_NAME_INVALID"
let host_has_resident_vms = "HOST_HAS_RESIDENT_VMS"
let hosts_failed_to_enable_caching = "HOSTS_FAILED_TO_ENABLE_CACHING"
let hosts_failed_to_disable_caching = "HOSTS_FAILED_TO_DISABLE_CACHING"
let host_cannot_see_SR = "HOST_CANNOT_SEE_SR"

(* Host errors which explain why the host is in emergency mode *)
let host_its_own_slave = "HOST_ITS_OWN_SLAVE"
let host_still_booting = "HOST_STILL_BOOTING"
(* license *)
let host_has_no_management_ip = "HOST_HAS_NO_MANAGEMENT_IP"
let host_master_cannot_talk_back = "HOST_MASTER_CANNOT_TALK_BACK"
let host_unknown_to_master = "HOST_UNKNOWN_TO_MASTER"
(* should be fenced *)
let host_broken = "HOST_BROKEN"
let interface_has_no_ip = "INTERFACE_HAS_NO_IP"
let invalid_ip_address_specified = "INVALID_IP_ADDRESS_SPECIFIED"
let pif_has_no_network_configuration = "PIF_HAS_NO_NETWORK_CONFIGURATION"
let pif_has_no_v6_network_configuration = "PIF_HAS_NO_V6_NETWORK_CONFIGURATION"

let device_attach_timeout = "DEVICE_ATTACH_TIMEOUT"
let device_detach_timeout = "DEVICE_DETACH_TIMEOUT"
let device_detach_rejected = "DEVICE_DETACH_REJECTED"

let operation_not_allowed = "OPERATION_NOT_ALLOWED"
let operation_blocked = "OPERATION_BLOCKED"
let network_already_connected = "NETWORK_ALREADY_CONNECTED"
let cannot_destroy_system_network = "CANNOT_DESTROY_SYSTEM_NETWORK"
let pif_is_physical = "PIF_IS_PHYSICAL"
let pif_is_vlan = "PIF_IS_VLAN"
let pif_vlan_exists = "PIF_VLAN_EXISTS"
let pif_vlan_still_exists = "PIF_VLAN_STILL_EXISTS"
let pif_device_not_found = "PIF_DEVICE_NOT_FOUND"
let pif_already_bonded = "PIF_ALREADY_BONDED"
let pif_cannot_bond_cross_host = "PIF_CANNOT_BOND_CROSS_HOST"
let pif_bond_needs_more_members = "PIF_BOND_NEEDS_MORE_MEMBERS"
let pif_configuration_error = "PIF_CONFIGURATION_ERROR"
let pif_is_management_iface = "PIF_IS_MANAGEMENT_INTERFACE"
let pif_incompatible_primary_address_type = "PIF_INCOMPATIBLE_PRIMARY_ADDRESS_TYPE"
let required_pif_is_unplugged = "REQUIRED_PIF_IS_UNPLUGGED"
let pif_does_not_allow_unplug = "PIF_DOES_NOT_ALLOW_UNPLUG"
let cannot_plug_bond_slave = "CANNOT_PLUG_BOND_SLAVE"
let cannot_add_vlan_to_bond_slave = "CANNOT_ADD_VLAN_TO_BOND_SLAVE"
let cannot_add_tunnel_to_bond_slave = "CANNOT_ADD_TUNNEL_TO_BOND_SLAVE"
let slave_requires_management_iface = "SLAVE_REQUIRES_MANAGEMENT_INTERFACE"
let vif_in_use = "VIF_IN_USE"
let cannot_plug_vif = "CANNOT_PLUG_VIF"
let mac_still_exists = "MAC_STILL_EXISTS"
let mac_does_not_exist = "MAC_DOES_NOT_EXIST"
let mac_invalid = "MAC_INVALID"
let duplicate_pif_device_name = "DUPLICATE_PIF_DEVICE_NAME"
let could_not_find_network_interface_with_specified_device_name_and_mac_address = "COULD_NOT_FIND_NETWORK_INTERFACE_WITH_SPECIFIED_DEVICE_NAME_AND_MAC_ADDRESS"

let openvswitch_not_active = "OPENVSWITCH_NOT_ACTIVE"
let transport_pif_not_configured = "TRANSPORT_PIF_NOT_CONFIGURED"
let is_tunnel_access_pif = "IS_TUNNEL_ACCESS_PIF"
let pif_tunnel_still_exists = "PIF_TUNNEL_STILL_EXISTS"

let vlan_tag_invalid = "VLAN_TAG_INVALID"
let vm_bad_power_state = "VM_BAD_POWER_STATE"
let vm_is_template = "VM_IS_TEMPLATE"
let vm_is_snapshot = "VM_IS_SNAPSHOT"
let other_operation_in_progress = "OTHER_OPERATION_IN_PROGRESS"
let vbd_not_removable_media = "VBD_NOT_REMOVABLE_MEDIA"
let vbd_not_unpluggable = "VBD_NOT_UNPLUGGABLE"
let vbd_not_empty = "VBD_NOT_EMPTY"
let vbd_is_empty = "VBD_IS_EMPTY"
let vbd_tray_locked = "VBD_TRAY_LOCKED"
let vbd_missing = "VBD_MISSING"
let vm_snapshot_failed = "VM_SNAPSHOT_FAILED"
let vm_snapshot_with_quiesce_failed = "VM_SNAPSHOT_WITH_QUIESCE_FAILED"
let vm_snapshot_with_quiesce_timeout = "VM_SNAPSHOT_WITH_QUIESCE_TIMEOUT"
let vm_snapshot_with_quiesce_plugin_does_not_respond = "VM_SNAPSHOT_WITH_QUIESCE_PLUGIN_DEOS_NOT_RESPOND"
let vm_snapshot_with_quiesce_not_supported = "VM_SNAPSHOT_WITH_QUIESCE_NOT_SUPPORTED"
let xen_vss_req_error_init_failed = "XEN_VSS_REQ_ERROR_INIT_FAILED"
let xen_vss_req_error_prov_not_loaded = "XEN_VSS_REQ_ERROR_PROV_NOT_LOADED"
let xen_vss_req_error_no_volumes_supported = "XEN_VSS_REQ_ERROR_NO_VOLUMES_SUPPORTED"
let xen_vss_req_error_start_snapshot_set_failed = "XEN_VSS_REQ_ERROR_START_SNAPSHOT_SET_FAILED"
let xen_vss_req_error_adding_volume_to_snapset_failed = "XEN_VSS_REQ_ERROR_ADDING_VOLUME_TO_SNAPSET_FAILED"
let xen_vss_req_error_preparing_writers = "XEN_VSS_REQ_ERROR_PREPARING_WRITERS"
let xen_vss_req_error_creating_snapshot = "XEN_VSS_REQ_ERROR_CREATING_SNAPSHOT"
let xen_vss_req_error_creating_snapshot_xml_string = "XEN_VSS_REQ_ERROR_CREATING_SNAPSHOT_XML_STRING"
let vm_revert_failed = "VM_REVERT_FAILED"
let vm_checkpoint_suspend_failed = "VM_CHECKPOINT_SUSPEND_FAILED"
let vm_checkpoint_resume_failed = "VM_CHECKPOINT_RESUME_FAILED"
let vm_unsafe_boot = "VM_UNSAFE_BOOT"
let vm_requires_sr = "VM_REQUIRES_SR"
let vm_requires_vdi = "VM_REQUIRES_VDI"
let vm_requires_net = "VM_REQUIRES_NETWORK"
let vm_requires_gpu = "VM_REQUIRES_GPU"
let vm_requires_iommu = "VM_REQUIRES_IOMMU"
let vm_host_incompatible_version = "VM_HOST_INCOMPATIBLE_VERSION"
let vm_has_pci_attached = "VM_HAS_PCI_ATTACHED"
let host_cannot_attach_network = "HOST_CANNOT_ATTACH_NETWORK"
let vm_no_suspend_sr = "VM_NO_SUSPEND_SR"
let vm_no_crashdump_sr = "VM_NO_CRASHDUMP_SR"
let vm_migrate_failed = "VM_MIGRATE_FAILED"
let vm_missing_pv_drivers = "VM_MISSING_PV_DRIVERS"
let vm_failed_shutdown_ack = "VM_FAILED_SHUTDOWN_ACKNOWLEDGMENT"
let vm_old_pv_drivers = "VM_OLD_PV_DRIVERS"
let vm_cannot_delete_default_template = "VM_CANNOT_DELETE_DEFAULT_TEMPLATE"
let vm_memory_size_too_low = "VM_MEMORY_SIZE_TOO_LOW"
let vm_memory_target_wait_timeout = "VM_MEMORY_TARGET_WAIT_TIMEOUT"
let vm_shutdown_timeout = "VM_SHUTDOWN_TIMEOUT"
let vm_duplicate_vbd_device = "VM_DUPLICATE_VBD_DEVICE"
let illegal_vbd_device = "ILLEGAL_VBD_DEVICE"
let vm_not_resident_here = "VM_NOT_RESIDENT_HERE"
let vm_crashed = "VM_CRASHED"
let vm_rebooted = "VM_REBOOTED"
let vm_halted = "VM_HALTED"
let vm_attached_to_more_than_one_vdi_with_timeoffset_marked_as_reset_on_boot = "VM_ATTACHED_TO_MORE_THAN_ONE_VDI_WITH_TIMEOFFSET_MARKED_AS_RESET_ON_BOOT"
let vms_failed_to_cooperate = "VMS_FAILED_TO_COOPERATE"
let domain_exists = "DOMAIN_EXISTS"
let cannot_reset_control_domain = "CANNOT_RESET_CONTROL_DOMAIN"
let not_system_domain = "NOT_SYSTEM_DOMAIN"
let only_provision_template = "PROVISION_ONLY_ALLOWED_ON_TEMPLATE"
let only_revert_snapshot = "REVERT_ONLY_ALLOWED_ON_SNAPSHOT"
let provision_failed_out_of_space = "PROVISION_FAILED_OUT_OF_SPACE"
let bootloader_failed = "BOOTLOADER_FAILED"
let unknown_bootloader = "UNKNOWN_BOOTLOADER"
let operation_not_allowed = "OPERATION_NOT_ALLOWED"
let object_nolonger_exists = "OBJECT_NOLONGER_EXISTS"
let sr_attach_failed = "SR_ATTACH_FAILED"
let sr_full = "SR_FULL"
let sr_has_pbd = "SR_HAS_PBD"
let sr_requires_upgrade = "SR_REQUIRES_UPGRADE"
let sr_is_cache_sr = "SR_IS_CACHE_SR"
let vdi_in_use = "VDI_IN_USE"
let vdi_is_sharable = "VDI_IS_SHARABLE"
let vdi_readonly = "VDI_READONLY"
let vdi_is_a_physical_device = "VDI_IS_A_PHYSICAL_DEVICE"
let vdi_is_not_iso = "VDI_IS_NOT_ISO"
let vbd_cds_must_be_readonly = "VBD_CDS_MUST_BE_READONLY"
(* CA-83260 *)
let disk_vbd_must_be_readwrite_for_hvm = "DISK_VBD_MUST_BE_READWRITE_FOR_HVM"
let host_cd_drive_empty = "HOST_CD_DRIVE_EMPTY"
let vdi_not_available = "VDI_NOT_AVAILABLE"
let vdi_location_missing = "VDI_LOCATION_MISSING"
let vdi_content_id_missing = "VDI_CONTENT_ID_MISSING"
let vdi_missing = "VDI_MISSING"
let vdi_incompatible_type = "VDI_INCOMPATIBLE_TYPE"
let vdi_not_managed = "VDI_NOT_MANAGED"
let vdi_io_error = "VDI_IO_ERROR"
let vdi_on_boot_mode_incompatable_with_operation = "VDI_ON_BOOT_MODE_INCOMPATABLE_WITH_OPERATION"
let vdi_not_in_map = "VDI_NOT_IN_MAP"
let cannot_create_state_file = "CANNOT_CREATE_STATE_FILE"

let operation_partially_failed = "OPERATION_PARTIALLY_FAILED"

let sr_uuid_exists = "SR_UUID_EXISTS"
let sr_no_pbds = "SR_HAS_NO_PBDS"
let sr_has_multiple_pbds = "SR_HAS_MULTIPLE_PBDS"
let sr_backend_failure = "SR_BACKEND_FAILURE"
let sr_unknown_driver = "SR_UNKNOWN_DRIVER"
let sr_vdi_locking_failed = "SR_VDI_LOCKING_FAILED"
let sr_not_empty = "SR_NOT_EMPTY"
let sr_device_in_use = "SR_DEVICE_IN_USE"
let sr_operation_not_supported = "SR_OPERATION_NOT_SUPPORTED"
let sr_not_sharable = "SR_NOT_SHARABLE"
let sr_indestructible = "SR_INDESTRUCTIBLE"

let sm_plugin_communication_failure = "SM_PLUGIN_COMMUNICATION_FAILURE"

let pbd_exists = "PBD_EXISTS"

let not_implemented = "NOT_IMPLEMENTED"
let device_already_attached = "DEVICE_ALREADY_ATTACHED"
let device_already_detached = "DEVICE_ALREADY_DETACHED"
let device_already_exists = "DEVICE_ALREADY_EXISTS"
let device_not_attached = "DEVICE_NOT_ATTACHED"
let network_contains_pif = "NETWORK_CONTAINS_PIF"
let network_contains_vif = "NETWORK_CONTAINS_VIF"
let gpu_group_contains_vgpu = "GPU_GROUP_CONTAINS_VGPU"
let gpu_group_contains_pgpu = "GPU_GROUP_CONTAINS_PGPU"
let invalid_device = "INVALID_DEVICE"
let events_lost = "EVENTS_LOST"
let event_subscription_parse_failure = "EVENT_SUBSCRIPTION_PARSE_FAILURE"
let event_from_token_parse_failure = "EVENT_FROM_TOKEN_PARSE_FAILURE"
let session_not_registered = "SESSION_NOT_REGISTERED"

let import_error_generic = "IMPORT_ERROR"
let import_error_premature_eof = "IMPORT_ERROR_PREMATURE_EOF"
let import_error_some_checksums_failed = "IMPORT_ERROR_SOME_CHECKSUMS_FAILED"
let import_error_cannot_handle_chunked = "IMPORT_ERROR_CANNOT_HANDLE_CHUNKED"
let import_error_failed_to_find_object = "IMPORT_ERROR_FAILED_TO_FIND_OBJECT"
let import_error_attached_disks_not_found = "IMPORT_ERROR_ATTACHED_DISKS_NOT_FOUND"
let import_error_unexpected_file = "IMPORT_ERROR_UNEXPECTED_FILE"
let import_incompatible_version = "IMPORT_INCOMPATIBLE_VERSION"

let restore_incompatible_version = "RESTORE_INCOMPATIBLE_VERSION"
let restore_target_missing_device = "RESTORE_TARGET_MISSING_DEVICE"
let restore_target_mgmt_if_not_in_backup = "RESTORE_TARGET_MGMT_IF_NOT_IN_BACKUP"

let pool_not_in_emergency_mode = "NOT_IN_EMERGENCY_MODE"
let pool_hosts_not_compatible = "HOSTS_NOT_COMPATIBLE"
let pool_hosts_not_homogeneous = "HOSTS_NOT_HOMOGENEOUS"
let pool_joining_host_cannot_contain_shared_SRs = "JOINING_HOST_CANNOT_CONTAIN_SHARED_SRS"
let pool_joining_host_cannot_have_running_or_suspended_VMs = "JOINING_HOST_CANNOT_HAVE_RUNNING_OR_SUSPENDED_VMS"
let pool_joining_host_cannot_have_running_VMs = "JOINING_HOST_CANNOT_HAVE_RUNNING_VMS"
let pool_joining_host_cannot_have_vms_with_current_operations = "JOINING_HOST_CANNOT_HAVE_VMS_WITH_CURRENT_OPERATIONS"
let pool_joining_host_cannot_be_master_of_other_hosts = "JOINING_HOST_CANNOT_BE_MASTER_OF_OTHER_HOSTS"
let pool_joining_host_connection_failed = "JOINING_HOST_CONNECTION_FAILED"
let pool_joining_host_service_failed = "JOINING_HOST_SERVICE_FAILED"
let pool_joining_host_must_have_physical_managment_nic = "POOL_JOINING_HOST_MUST_HAVE_PHYSICAL_MANAGEMENT_NIC"
let pool_joining_external_auth_mismatch = "POOL_JOINING_EXTERNAL_AUTH_MISMATCH"
let pool_joining_host_must_have_same_product_version = "POOL_JOINING_HOST_MUST_HAVE_SAME_PRODUCT_VERSION"

(* workload balancing, deprecated since Clearwater *)
let wlb_not_initialized = "WLB_NOT_INITIALIZED"
let wlb_disabled = "WLB_DISABLED"
let wlb_connection_refused = "WLB_CONNECTION_REFUSED"
let wlb_unknown_host = "WLB_UNKNOWN_HOST"
let wlb_timeout = "WLB_TIMEOUT"
let wlb_authentication_failed = "WLB_AUTHENTICATION_FAILED"
let wlb_malformed_request = "WLB_MALFORMED_REQUEST"
let wlb_malformed_response = "WLB_MALFORMED_RESPONSE"
let wlb_xenserver_connection_refused = "WLB_XENSERVER_CONNECTION_REFUSED"
let wlb_xenserver_unknown_host = "WLB_XENSERVER_UNKNOWN_HOST"
let wlb_xenserver_timeout = "WLB_XENSERVER_TIMEOUT"
let wlb_xenserver_authentication_failed = "WLB_XENSERVER_AUTHENTICATION_FAILED"
let wlb_xenserver_malformed_response = "WLB_XENSERVER_MALFORMED_RESPONSE"
let wlb_internal_error = "WLB_INTERNAL_ERROR"
let wlb_url_invalid = "WLB_URL_INVALID"
let wlb_connection_reset = "WLB_CONNECTION_RESET"


let sr_not_shared = "SR_NOT_SHARED"

let default_sr_not_found = "DEFAULT_SR_NOT_FOUND"

let task_cancelled = "TASK_CANCELLED"

let too_many_pending_tasks = "TOO_MANY_PENDING_TASKS"
let too_busy = "TOO_BUSY"

let out_of_space = "OUT_OF_SPACE"
let invalid_patch = "INVALID_PATCH"
let invalid_patch_with_log = "INVALID_PATCH_WITH_LOG"
let patch_already_exists = "PATCH_ALREADY_EXISTS"
let patch_is_applied = "PATCH_IS_APPLIED"
let cannot_find_patch = "CANNOT_FIND_PATCH"
let cannot_fetch_patch = "CANNOT_FETCH_PATCH"
let patch_already_applied = "PATCH_ALREADY_APPLIED"
let patch_precheck_failed_unknown_error = "PATCH_PRECHECK_FAILED_UNKNOWN_ERROR"
let patch_precheck_failed_prerequisite_missing = "PATCH_PRECHECK_FAILED_PREREQUISITE_MISSING"
let patch_precheck_failed_wrong_server_version = "PATCH_PRECHECK_FAILED_WRONG_SERVER_VERSION"
let patch_precheck_failed_wrong_server_build = "PATCH_PRECHECK_FAILED_WRONG_SERVER_BUILD"
let patch_precheck_failed_vm_running = "PATCH_PRECHECK_FAILED_VM_RUNNING"
let patch_precheck_tools_iso_mounted = "PATCH_PRECHECK_FAILED_ISO_MOUNTED"
let patch_apply_failed = "PATCH_APPLY_FAILED"
let cannot_find_oem_backup_partition = "CANNOT_FIND_OEM_BACKUP_PARTITION"
let only_allowed_on_oem_edition = "ONLY_ALLOWED_ON_OEM_EDITION"
let not_allowed_on_oem_edition = "NOT_ALLOWED_ON_OEM_EDITION"
let cannot_find_state_partition = "CANNOT_FIND_STATE_PARTITION"
let backup_script_failed = "BACKUP_SCRIPT_FAILED"
let restore_script_failed = "RESTORE_SCRIPT_FAILED"

let license_expired = "LICENSE_EXPIRED"
let license_restriction = "LICENCE_RESTRICTION"
let license_does_not_support_pooling = "LICENSE_DOES_NOT_SUPPORT_POOLING"
let license_processing_error = "LICENSE_PROCESSING_ERROR"
let license_cannot_downgrade_in_pool = "LICENSE_CANNOT_DOWNGRADE_WHILE_IN_POOL"
let license_does_not_support_xha = "LICENSE_DOES_NOT_SUPPORT_XHA"

let v6d_failure = "V6D_FAILURE"
let invalid_edition = "INVALID_EDITION"
let missing_connection_details = "MISSING_CONNECTION_DETAILS"
let license_checkout_error = "LICENSE_CHECKOUT_ERROR"
let license_file_deprecated = "LICENSE_FILE_DEPRECATED"
let activation_while_not_free = "ACTIVATION_WHILE_NOT_FREE"

let feature_restricted = "FEATURE_RESTRICTED"

let xmlrpc_unmarshal_failure = "XMLRPC_UNMARSHAL_FAILURE"

let duplicate_vm = "DUPLICATE_VM"
let duplicate_mac_seed = "DUPLICATE_MAC_SEED"

let client_error = "CLIENT_ERROR"

let ballooning_disabled = "BALLOONING_DISABLED"

let ha_host_is_armed = "HA_HOST_IS_ARMED"
let ha_is_enabled = "HA_IS_ENABLED"
let ha_not_enabled = "HA_NOT_ENABLED"
let ha_not_installed = "HA_NOT_INSTALLED"
let ha_host_cannot_see_peers = "HA_HOST_CANNOT_SEE_PEERS"
let ha_too_few_hosts = "HA_TOO_FEW_HOSTS"
let ha_should_be_fenced = "HA_SHOULD_BE_FENCED"

let ha_abort_new_master = "HA_ABORT_NEW_MASTER"

let ha_no_plan = "HA_NO_PLAN"
let ha_lost_statefile = "HA_LOST_STATEFILE"
let ha_pool_is_enabled_but_host_is_disabled = "HA_POOL_IS_ENABLED_BUT_HOST_IS_DISABLED"

let ha_heartbeat_daemon_startup_failed = "HA_HEARTBEAT_DAEMON_STARTUP_FAILED"
let ha_host_cannot_access_statefile = "HA_HOST_CANNOT_ACCESS_STATEFILE"

let ha_failed_to_form_liveset = "HA_FAILED_TO_FORM_LIVESET"

let ha_cannot_change_bond_status_of_mgmt_iface = "HA_CANNOT_CHANGE_BOND_STATUS_OF_MGMT_IFACE"

(* CA-16480: prevent configuration errors which nullify xHA goodness *)
let ha_constraint_violation_sr_not_shared = "HA_CONSTRAINT_VIOLATION_SR_NOT_SHARED"
let ha_constraint_violation_network_not_shared = "HA_CONSTRAINT_VIOLATION_NETWORK_NOT_SHARED"

let ha_operation_would_break_failover_plan = "HA_OPERATION_WOULD_BREAK_FAILOVER_PLAN"

let cannot_evacuate_host = "CANNOT_EVACUATE_HOST"
let host_evacuate_in_progress = "HOST_EVACUATE_IN_PROGRESS"

let system_status_retrieval_failed = "SYSTEM_STATUS_RETRIEVAL_FAILED"
let system_status_must_use_tar_on_oem = "SYSTEM_STATUS_MUST_USE_TAR_ON_OEM"

let xapi_hook_failed = "XAPI_HOOK_FAILED"

let no_local_storage = "NO_LOCAL_STORAGE"

let xenapi_missing_plugin = "XENAPI_MISSING_PLUGIN"
let xenapi_plugin_failure = "XENAPI_PLUGIN_FAILURE"

let sr_attached = "SR_ATTACHED"

let domain_builder_error = "DOMAIN_BUILDER_ERROR"

let auth_already_enabled = "AUTH_ALREADY_ENABLED"
let auth_unknown_type = "AUTH_UNKNOWN_TYPE"
let auth_is_disabled = "AUTH_IS_DISABLED"
let auth_suffix_wrong_credentials = "_WRONG_CREDENTIALS"
let auth_suffix_permission_denied = "_PERMISSION_DENIED"
let auth_suffix_domain_lookup_failed = "_DOMAIN_LOOKUP_FAILED"
let auth_suffix_unavailable = "_UNAVAILABLE"
let auth_suffix_invalid_ou = "_INVALID_OU"
let auth_enable_failed = "AUTH_ENABLE_FAILED"
let auth_enable_failed_wrong_credentials = auth_enable_failed^auth_suffix_wrong_credentials
let auth_enable_failed_permission_denied = auth_enable_failed^auth_suffix_permission_denied
let auth_enable_failed_domain_lookup_failed = auth_enable_failed^auth_suffix_domain_lookup_failed
let auth_enable_failed_unavailable = auth_enable_failed^auth_suffix_unavailable
let auth_enable_failed_invalid_ou = auth_enable_failed^auth_suffix_invalid_ou
let auth_disable_failed = "AUTH_DISABLE_FAILED"
let auth_disable_failed_wrong_credentials = auth_disable_failed^auth_suffix_wrong_credentials
let auth_disable_failed_permission_denied = auth_disable_failed^auth_suffix_permission_denied
let pool_auth_already_enabled = "POOL_AUTH_ALREADY_ENABLED"
let pool_auth_prefix = "POOL_"
let pool_auth_enable_failed = pool_auth_prefix^auth_enable_failed
let pool_auth_enable_failed_wrong_credentials = pool_auth_enable_failed^auth_suffix_wrong_credentials
let pool_auth_enable_failed_permission_denied = pool_auth_enable_failed^auth_suffix_permission_denied
let pool_auth_enable_failed_domain_lookup_failed = pool_auth_enable_failed^auth_suffix_domain_lookup_failed
let pool_auth_enable_failed_unavailable = pool_auth_enable_failed^auth_suffix_unavailable
let pool_auth_enable_failed_unavailable = pool_auth_enable_failed^auth_suffix_invalid_ou
let pool_auth_enable_failed_duplicate_hostname = pool_auth_enable_failed^"_DUPLICATE_HOSTNAME"
let pool_auth_disable_failed = pool_auth_prefix^auth_disable_failed
let pool_auth_disable_failed_wrong_credentials = pool_auth_disable_failed^auth_suffix_wrong_credentials
let pool_auth_disable_failed_permission_denied = pool_auth_disable_failed^auth_suffix_permission_denied
let subject_cannot_be_resolved = "SUBJECT_CANNOT_BE_RESOLVED"
let auth_service_error = "AUTH_SERVICE_ERROR"
let subject_already_exists = "SUBJECT_ALREADY_EXISTS"
let role_not_found = "ROLE_NOT_FOUND"
let role_already_exists = "ROLE_ALREADY_EXISTS"
let rbac_permission_denied = "RBAC_PERMISSION_DENIED"

let certificate_does_not_exist = "CERTIFICATE_DOES_NOT_EXIST"
let certificate_already_exists = "CERTIFICATE_ALREADY_EXISTS"
let certificate_name_invalid = "CERTIFICATE_NAME_INVALID"
let certificate_corrupt = "CERTIFICATE_CORRUPT"
let certificate_library_corrupt = "CERTIFICATE_LIBRARY_CORRUPT"
let crl_does_not_exist = "CRL_DOES_NOT_EXIST"
let crl_already_exists = "CRL_ALREADY_EXISTS"
let crl_name_invalid = "CRL_NAME_INVALID"
let crl_corrupt = "CRL_CORRUPT"

let vmpp_has_vm = "VMPP_HAS_VM"
let vmpp_archive_more_frequent_than_backup = "VMPP_ARCHIVE_MORE_FREQUENT_THAN_BACKUP"
let vm_assigned_to_protection_policy = "VM_ASSIGNED_TO_PROTECTION_POLICY"

let ssl_verify_error = "SSL_VERIFY_ERROR"

let cannot_enable_redo_log = "CANNOT_ENABLE_REDO_LOG"
let redo_log_is_enabled = "REDO_LOG_IS_ENABLED"

let vm_bios_strings_already_set = "VM_BIOS_STRINGS_ALREADY_SET"

let invalid_feature_string = "INVALID_FEATURE_STRING"
let cpu_feature_masking_not_supported = "CPU_FEATURE_MASKING_NOT_SUPPORTED"

let feature_requires_hvm = "FEATURE_REQUIRES_HVM"

(* Disaster recovery *)
let vdi_contains_metadata_of_this_pool = "VDI_CONTAINS_METADATA_OF_THIS_POOL"
let no_more_redo_logs_allowed = "NO_MORE_REDO_LOGS_ALLOWED"
let could_not_import_database = "COULD_NOT_IMPORT_DATABASE"
let vm_incompatible_with_this_host = "VM_INCOMPATIBLE_WITH_THIS_HOST"
let cannot_destroy_disaster_recovery_task = "CANNOT_DESTROY_DISASTER_RECOVERY_TASK"
let vm_is_part_of_an_appliance = "VM_IS_PART_OF_AN_APPLIANCE"
let vm_to_import_is_not_newer_version = "VM_TO_IMPORT_IS_NOT_NEWER_VERSION"
let suspend_vdi_replacement_is_not_identical = "SUSPEND_VDI_REPLACEMENT_IS_NOT_IDENTICAL"
let vdi_copy_failed = "VDI_COPY_FAILED"

let vdi_needs_vm_for_migrate = "VDI_NEEDS_VM_FOR_MIGRATE"
let vm_has_too_many_snapshots = "VM_HAS_TOO_MANY_SNAPSHOTS"
let vm_has_checkpoint = "VM_HAS_CHECKPOINT"

let mirror_failed = "MIRROR_FAILED"
let too_many_storage_migrates = "TOO_MANY_STORAGE_MIGRATES"
let unimplemented_in_sm_backend = "UNIMPLEMENTED_IN_SM_BACKEND"
