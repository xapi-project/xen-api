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

let ( $ ) f x = f x

let errors = ref []

let add_error error =
  errors := error :: !errors ;
  error

let to_string = function
  | Server_error (name, args) ->
      Printf.sprintf "Server_error(%s, [ %a ])" name
        (fun () -> String.concat "; ")
        args
  | e ->
      Printexc.to_string e

let _ =
  Printexc.register_printer (function
    | Server_error (_, _) as e ->
        Some (to_string e)
    | _ ->
        None
    )

let message_deprecated = add_error "MESSAGE_DEPRECATED"

let message_removed = add_error "MESSAGE_REMOVED"

let permission_denied = add_error "PERMISSION_DENIED"

let internal_error = add_error "INTERNAL_ERROR"

let map_duplicate_key = add_error "MAP_DUPLICATE_KEY"

let db_uniqueness_constraint_violation =
  add_error "DB_UNIQUENESS_CONSTRAINT_VIOLATION"

let location_not_unique = add_error "LOCATION_NOT_UNIQUE"

let message_method_unknown = add_error "MESSAGE_METHOD_UNKNOWN"

let message_parameter_count_mismatch =
  add_error "MESSAGE_PARAMETER_COUNT_MISMATCH"

let value_not_supported = add_error "VALUE_NOT_SUPPORTED"

let invalid_value = add_error "INVALID_VALUE"

let memory_constraint_violation = add_error "MEMORY_CONSTRAINT_VIOLATION"

let memory_constraint_violation_order =
  add_error "MEMORY_CONSTRAINT_VIOLATION_ORDER"

let memory_constraint_violation_maxpin =
  add_error "MEMORY_CONSTRAINT_VIOLATION_MAXPIN"

let field_type_error = add_error "FIELD_TYPE_ERROR"

let session_authentication_failed = add_error "SESSION_AUTHENTICATION_FAILED"

let session_authorization_failed = add_error "SESSION_AUTHORIZATION_FAILED"

let session_invalid = add_error "SESSION_INVALID"

let change_password_rejected = add_error "CHANGE_PASSWORD_REJECTED"

let user_is_not_local_superuser = add_error "USER_IS_NOT_LOCAL_SUPERUSER"

let cannot_contact_host = add_error "CANNOT_CONTACT_HOST"

let tls_connection_failed = add_error "TLS_CONNECTION_FAILED"

let not_supported_during_upgrade = add_error "NOT_SUPPORTED_DURING_UPGRADE"

let handle_invalid = add_error "HANDLE_INVALID"

let uuid_invalid = add_error "UUID_INVALID"

let vm_hvm_required = add_error "VM_HVM_REQUIRED"

let vm_no_vcpus = add_error "VM_NO_VCPUS"

let vm_toomany_vcpus = add_error "VM_TOO_MANY_VCPUS"

let vm_is_protected = add_error "VM_IS_PROTECTED"

let vm_is_immobile = add_error "VM_IS_IMMOBILE"

let vm_is_using_nested_virt = add_error "VM_IS_USING_NESTED_VIRT"

let host_in_use = add_error "HOST_IN_USE"

let host_in_emergency_mode = add_error "HOST_IN_EMERGENCY_MODE"

let host_cannot_read_metrics = add_error "HOST_CANNOT_READ_METRICS"

let host_disabled = add_error "HOST_DISABLED"

let host_disabled_until_reboot = add_error "HOST_DISABLED_UNTIL_REBOOT"

let host_not_disabled = add_error "HOST_NOT_DISABLED"

let host_not_live = add_error "HOST_NOT_LIVE"

let host_is_live = add_error "HOST_IS_LIVE"

let host_power_on_mode_disabled = add_error "HOST_POWER_ON_MODE_DISABLED"

let host_not_enough_free_memory = add_error "HOST_NOT_ENOUGH_FREE_MEMORY"

let host_not_enough_pcpus = add_error "HOST_NOT_ENOUGH_PCPUS"

let no_hosts_available = add_error "NO_HOSTS_AVAILABLE"

let host_offline = add_error "HOST_OFFLINE"

let host_cannot_destroy_self = add_error "HOST_CANNOT_DESTROY_SELF"

let host_is_slave = add_error "HOST_IS_SLAVE"

let host_name_invalid = add_error "HOST_NAME_INVALID"

let host_has_resident_vms = add_error "HOST_HAS_RESIDENT_VMS"

let hosts_failed_to_enable_caching = add_error "HOSTS_FAILED_TO_ENABLE_CACHING"

let hosts_failed_to_disable_caching =
  add_error "HOSTS_FAILED_TO_DISABLE_CACHING"

let host_cannot_see_SR = add_error "HOST_CANNOT_SEE_SR"

(* Host errors which explain why the host is in emergency mode *)
let host_its_own_slave = add_error "HOST_ITS_OWN_SLAVE"

let host_still_booting = add_error "HOST_STILL_BOOTING"

(* license *)
let host_has_no_management_ip = add_error "HOST_HAS_NO_MANAGEMENT_IP"

let host_master_cannot_talk_back = add_error "HOST_MASTER_CANNOT_TALK_BACK"

let host_unknown_to_master = add_error "HOST_UNKNOWN_TO_MASTER"

let host_xapi_version_higher_than_coordinator =
  add_error "HOST_XAPI_VERSION_HIGHER_THAN_COORDINATOR"

(* should be fenced *)
let host_broken = add_error "HOST_BROKEN"

let interface_has_no_ip = add_error "INTERFACE_HAS_NO_IP"

let invalid_ip_address_specified = add_error "INVALID_IP_ADDRESS_SPECIFIED"

let invalid_cidr_address_specified = add_error "INVALID_CIDR_ADDRESS_SPECIFIED"

let address_violates_locking_constraint =
  add_error "ADDRESS_VIOLATES_LOCKING_CONSTRAINT"

let pif_has_no_network_configuration =
  add_error "PIF_HAS_NO_NETWORK_CONFIGURATION"

let pif_has_no_v6_network_configuration =
  add_error "PIF_HAS_NO_V6_NETWORK_CONFIGURATION"

let device_attach_timeout = add_error "DEVICE_ATTACH_TIMEOUT"

let device_detach_timeout = add_error "DEVICE_DETACH_TIMEOUT"

let device_detach_rejected = add_error "DEVICE_DETACH_REJECTED"

let network_sriov_insufficient_capacity =
  add_error "NETWORK_SRIOV_INSUFFICIENT_CAPACITY"

let network_sriov_already_enabled = add_error "NETWORK_SRIOV_ALREADY_ENABLED"

let network_sriov_enable_failed = add_error "NETWORK_SRIOV_ENABLE_FAILED"

let network_sriov_disable_failed = add_error "NETWORK_SRIOV_DISABLE_FAILED"

let network_incompatible_with_sriov =
  add_error "NETWORK_INCOMPATIBLE_WITH_SRIOV"

let network_incompatible_with_vlan_on_bridge =
  add_error "NETWORK_INCOMPATIBLE_WITH_VLAN_ON_BRIDGE"

let network_incompatible_with_vlan_on_sriov =
  add_error "NETWORK_INCOMPATIBLE_WITH_VLAN_ON_SRIOV"

let network_incompatible_with_bond = add_error "NETWORK_INCOMPATIBLE_WITH_BOND"

let network_incompatible_with_tunnel =
  add_error "NETWORK_INCOMPATIBLE_WITH_TUNNEL"

let network_has_incompatible_sriov_pifs =
  add_error "NETWORK_HAS_INCOMPATIBLE_SRIOV_PIFS"

let network_has_incompatible_vlan_on_sriov_pifs =
  add_error "NETWORK_HAS_INCOMPATIBLE_VLAN_ON_SRIOV_PIFS"

let operation_not_allowed = add_error "OPERATION_NOT_ALLOWED"

let operation_blocked = add_error "OPERATION_BLOCKED"

let network_already_connected = add_error "NETWORK_ALREADY_CONNECTED"

let network_unmanaged = add_error "NETWORK_UNMANAGED"

let network_incompatible_purposes = add_error "NETWORK_INCOMPATIBLE_PURPOSES"

let cannot_destroy_system_network = add_error "CANNOT_DESTROY_SYSTEM_NETWORK"

let pif_is_physical = add_error "PIF_IS_PHYSICAL"

let pif_is_not_physical = add_error "PIF_IS_NOT_PHYSICAL"

let pif_is_vlan = add_error "PIF_IS_VLAN"

let pif_is_sriov_logical = add_error "PIF_IS_SRIOV_LOGICAL"

let pif_vlan_exists = add_error "PIF_VLAN_EXISTS"

let pif_vlan_still_exists = add_error "PIF_VLAN_STILL_EXISTS"

let vlan_in_use = add_error "VLAN_IN_USE"

let pif_device_not_found = add_error "PIF_DEVICE_NOT_FOUND"

let pif_already_bonded = add_error "PIF_ALREADY_BONDED"

let pif_cannot_bond_cross_host = add_error "PIF_CANNOT_BOND_CROSS_HOST"

let pif_bond_needs_more_members = add_error "PIF_BOND_NEEDS_MORE_MEMBERS"

let pif_bond_more_than_one_ip = add_error "PIF_BOND_MORE_THAN_ONE_IP"

let pif_configuration_error = add_error "PIF_CONFIGURATION_ERROR"

let pif_is_management_iface = add_error "PIF_IS_MANAGEMENT_INTERFACE"

let pif_incompatible_primary_address_type =
  add_error "PIF_INCOMPATIBLE_PRIMARY_ADDRESS_TYPE"

let required_pif_is_unplugged = add_error "REQUIRED_PIF_IS_UNPLUGGED"

let pif_not_present = add_error "PIF_NOT_PRESENT"

let pif_does_not_allow_unplug = add_error "PIF_DOES_NOT_ALLOW_UNPLUG"

let pif_allows_unplug = add_error "PIF_ALLOWS_UNPLUG"

let pif_has_fcoe_sr_in_use = add_error "PIF_HAS_FCOE_SR_IN_USE"

let pif_unmanaged = add_error "PIF_UNMANAGED"

let pif_is_not_sriov_capable = add_error "PIF_IS_NOT_SRIOV_CAPABLE"

let pif_sriov_still_exists = add_error "PIF_SRIOV_STILL_EXISTS"

let cannot_plug_bond_slave = add_error "CANNOT_PLUG_BOND_SLAVE"

let cannot_add_vlan_to_bond_slave = add_error "CANNOT_ADD_VLAN_TO_BOND_SLAVE"

let cannot_add_tunnel_to_bond_slave =
  add_error "CANNOT_ADD_TUNNEL_TO_BOND_SLAVE"

let cannot_add_tunnel_to_sriov_logical =
  add_error "CANNOT_ADD_TUNNEL_TO_SRIOV_LOGICAL"

let cannot_add_tunnel_to_vlan_on_sriov_logical =
  add_error "CANNOT_ADD_TUNNEL_TO_VLAN_ON_SRIOV_LOGICAL"

let cannot_change_pif_properties = add_error "CANNOT_CHANGE_PIF_PROPERTIES"

let cannot_forget_sriov_logical = add_error "CANNOT_FORGET_SRIOV_LOGICAL"

let incompatible_pif_properties = add_error "INCOMPATIBLE_PIF_PROPERTIES"

let slave_requires_management_iface =
  add_error "SLAVE_REQUIRES_MANAGEMENT_INTERFACE"

let vif_in_use = add_error "VIF_IN_USE"

let cannot_plug_vif = add_error "CANNOT_PLUG_VIF"

let mac_still_exists = add_error "MAC_STILL_EXISTS"

let mac_does_not_exist = add_error "MAC_DOES_NOT_EXIST"

let mac_invalid = add_error "MAC_INVALID"

let duplicate_pif_device_name = add_error "DUPLICATE_PIF_DEVICE_NAME"

let could_not_find_network_interface_with_specified_device_name_and_mac_address
    =
  add_error
    "COULD_NOT_FIND_NETWORK_INTERFACE_WITH_SPECIFIED_DEVICE_NAME_AND_MAC_ADDRESS"

let openvswitch_not_active = add_error "OPENVSWITCH_NOT_ACTIVE"

let transport_pif_not_configured = add_error "TRANSPORT_PIF_NOT_CONFIGURED"

let is_tunnel_access_pif = add_error "IS_TUNNEL_ACCESS_PIF"

let pif_tunnel_still_exists = add_error "PIF_TUNNEL_STILL_EXISTS"

let bridge_not_available = add_error "BRIDGE_NOT_AVAILABLE"

let bridge_name_exists = add_error "BRIDGE_NAME_EXISTS"

let vlan_tag_invalid = add_error "VLAN_TAG_INVALID"

let vm_bad_power_state = add_error "VM_BAD_POWER_STATE"

let vm_is_template = add_error "VM_IS_TEMPLATE"

let vm_is_snapshot = add_error "VM_IS_SNAPSHOT"

let other_operation_in_progress = add_error "OTHER_OPERATION_IN_PROGRESS"

let vbd_not_removable_media = add_error "VBD_NOT_REMOVABLE_MEDIA"

let vbd_not_unpluggable = add_error "VBD_NOT_UNPLUGGABLE"

let vbd_not_empty = add_error "VBD_NOT_EMPTY"

let vbd_is_empty = add_error "VBD_IS_EMPTY"

let vbd_tray_locked = add_error "VBD_TRAY_LOCKED"

let vbd_missing = add_error "VBD_MISSING"

let vm_no_empty_cd_vbd = add_error "VM_NO_EMPTY_CD_VBD"

let vm_snapshot_failed = add_error "VM_SNAPSHOT_FAILED"

let vm_snapshot_with_quiesce_failed =
  add_error "VM_SNAPSHOT_WITH_QUIESCE_FAILED"

let vm_snapshot_with_quiesce_timeout =
  add_error "VM_SNAPSHOT_WITH_QUIESCE_TIMEOUT"

let vm_snapshot_with_quiesce_plugin_does_not_respond =
  add_error "VM_SNAPSHOT_WITH_QUIESCE_PLUGIN_DEOS_NOT_RESPOND"

let vm_snapshot_with_quiesce_not_supported =
  add_error "VM_SNAPSHOT_WITH_QUIESCE_NOT_SUPPORTED"

let xen_vss_req_error_init_failed = add_error "XEN_VSS_REQ_ERROR_INIT_FAILED"

let xen_vss_req_error_prov_not_loaded =
  add_error "XEN_VSS_REQ_ERROR_PROV_NOT_LOADED"

let xen_vss_req_error_no_volumes_supported =
  add_error "XEN_VSS_REQ_ERROR_NO_VOLUMES_SUPPORTED"

let xen_vss_req_error_start_snapshot_set_failed =
  add_error "XEN_VSS_REQ_ERROR_START_SNAPSHOT_SET_FAILED"

let xen_vss_req_error_adding_volume_to_snapset_failed =
  add_error "XEN_VSS_REQ_ERROR_ADDING_VOLUME_TO_SNAPSET_FAILED"

let xen_vss_req_error_preparing_writers =
  add_error "XEN_VSS_REQ_ERROR_PREPARING_WRITERS"

let xen_vss_req_error_creating_snapshot =
  add_error "XEN_VSS_REQ_ERROR_CREATING_SNAPSHOT"

let xen_vss_req_error_creating_snapshot_xml_string =
  add_error "XEN_VSS_REQ_ERROR_CREATING_SNAPSHOT_XML_STRING"

let vm_revert_failed = add_error "VM_REVERT_FAILED"

let vm_checkpoint_suspend_failed = add_error "VM_CHECKPOINT_SUSPEND_FAILED"

let vm_checkpoint_resume_failed = add_error "VM_CHECKPOINT_RESUME_FAILED"

let vm_unsafe_boot = add_error "VM_UNSAFE_BOOT"

let vm_requires_sr = add_error "VM_REQUIRES_SR"

let vm_requires_vdi = add_error "VM_REQUIRES_VDI"

let vm_requires_net = add_error "VM_REQUIRES_NETWORK"

let vm_requires_gpu = add_error "VM_REQUIRES_GPU"

let vm_requires_vgpu = add_error "VM_REQUIRES_VGPU"

let vm_requires_iommu = add_error "VM_REQUIRES_IOMMU"

let vm_host_incompatible_version_migrate =
  add_error "VM_HOST_INCOMPATIBLE_VERSION_MIGRATE"

let vm_host_incompatible_version = add_error "VM_HOST_INCOMPATIBLE_VERSION"

let vm_host_incompatible_virtual_hardware_platform_version =
  add_error "VM_HOST_INCOMPATIBLE_VIRTUAL_HARDWARE_PLATFORM_VERSION"

let vm_has_pci_attached = add_error "VM_HAS_PCI_ATTACHED"

let vm_has_vgpu = add_error "VM_HAS_VGPU"

let vm_has_sriov_vif = add_error "VM_HAS_SRIOV_VIF"

let vm_has_no_suspend_vdi = add_error "VM_HAS_NO_SUSPEND_VDI"

let host_cannot_attach_network = add_error "HOST_CANNOT_ATTACH_NETWORK"

let vm_no_suspend_sr = add_error "VM_NO_SUSPEND_SR"

let vm_no_crashdump_sr = add_error "VM_NO_CRASHDUMP_SR"

let vm_migrate_failed = add_error "VM_MIGRATE_FAILED"

let vm_migrate_contact_remote_service_failed =
  add_error "VM_MIGRATE_CONTACT_REMOTE_SERVICE_FAILED"

let vm_missing_pv_drivers = add_error "VM_MISSING_PV_DRIVERS"

let vm_failed_shutdown_ack = add_error "VM_FAILED_SHUTDOWN_ACKNOWLEDGMENT"

let vm_failed_suspend_ack = add_error "VM_FAILED_SUSPEND_ACKNOWLEDGMENT"

let vm_old_pv_drivers = add_error "VM_OLD_PV_DRIVERS"

let vm_lacks_feature = add_error "VM_LACKS_FEATURE"

let vm_lacks_feature_shutdown = add_error "VM_LACKS_FEATURE_SHUTDOWN"

let vm_lacks_feature_suspend = add_error "VM_LACKS_FEATURE_SUSPEND"

let vm_lacks_feature_vcpu_hotplug = add_error "VM_LACKS_FEATURE_VCPU_HOTPLUG"

let vm_lacks_feature_static_ip_setting =
  add_error "VM_LACKS_FEATURE_STATIC_IP_SETTING"

let vm_cannot_delete_default_template =
  add_error "VM_CANNOT_DELETE_DEFAULT_TEMPLATE"

let vm_memory_size_too_low = add_error "VM_MEMORY_SIZE_TOO_LOW"

let vm_memory_target_wait_timeout = add_error "VM_MEMORY_TARGET_WAIT_TIMEOUT"

let vm_shutdown_timeout = add_error "VM_SHUTDOWN_TIMEOUT"

let vm_suspend_timeout = add_error "VM_SUSPEND_TIMEOUT"

let vm_duplicate_vbd_device = add_error "VM_DUPLICATE_VBD_DEVICE"

let illegal_vbd_device = add_error "ILLEGAL_VBD_DEVICE"

let vm_not_resident_here = add_error "VM_NOT_RESIDENT_HERE"

let vm_crashed = add_error "VM_CRASHED"

let vm_rebooted = add_error "VM_REBOOTED"

let vm_halted = add_error "VM_HALTED"

let vm_attached_to_more_than_one_vdi_with_timeoffset_marked_as_reset_on_boot =
  add_error
    "VM_ATTACHED_TO_MORE_THAN_ONE_VDI_WITH_TIMEOFFSET_MARKED_AS_RESET_ON_BOOT"

let vms_failed_to_cooperate = add_error "VMS_FAILED_TO_COOPERATE"

let vm_pv_drivers_in_use = add_error "VM_PV_DRIVERS_IN_USE"

let domain_exists = add_error "DOMAIN_EXISTS"

let cannot_reset_control_domain = add_error "CANNOT_RESET_CONTROL_DOMAIN"

let not_system_domain = add_error "NOT_SYSTEM_DOMAIN"

let only_provision_template = add_error "PROVISION_ONLY_ALLOWED_ON_TEMPLATE"

let only_revert_snapshot = add_error "REVERT_ONLY_ALLOWED_ON_SNAPSHOT"

let provision_failed_out_of_space = add_error "PROVISION_FAILED_OUT_OF_SPACE"

let bootloader_failed = add_error "BOOTLOADER_FAILED"

let unknown_bootloader = add_error "UNKNOWN_BOOTLOADER"

let failed_to_start_emulator = add_error "FAILED_TO_START_EMULATOR"

let object_nolonger_exists = add_error "OBJECT_NOLONGER_EXISTS"

let sr_attach_failed = add_error "SR_ATTACH_FAILED"

let sr_full = add_error "SR_FULL"

let sr_source_space_insufficient = add_error "SR_SOURCE_SPACE_INSUFFICIENT"

let sr_has_pbd = add_error "SR_HAS_PBD"

let sr_requires_upgrade = add_error "SR_REQUIRES_UPGRADE"

let sr_is_cache_sr = add_error "SR_IS_CACHE_SR"

let sr_unhealthy = add_error "SR_UNHEALTHY"

let vdi_in_use = add_error "VDI_IN_USE"

let vdi_is_sharable = add_error "VDI_IS_SHARABLE"

let vdi_readonly = add_error "VDI_READONLY"

let vdi_too_small = add_error "VDI_TOO_SMALL"

let vdi_too_large = add_error "VDI_TOO_LARGE"

let vdi_not_sparse = add_error "VDI_NOT_SPARSE"

let vdi_is_a_physical_device = add_error "VDI_IS_A_PHYSICAL_DEVICE"

let vdi_is_not_iso = add_error "VDI_IS_NOT_ISO"

let vbd_cds_must_be_readonly = add_error "VBD_CDS_MUST_BE_READONLY"

let vm_requires_vusb = add_error "VM_REQUIRES_VUSB"

(* CA-83260 *)
let disk_vbd_must_be_readwrite_for_hvm =
  add_error "DISK_VBD_MUST_BE_READWRITE_FOR_HVM"

let host_cd_drive_empty = add_error "HOST_CD_DRIVE_EMPTY"

let vdi_not_available = add_error "VDI_NOT_AVAILABLE"

let vdi_has_rrds = add_error "VDI_HAS_RRDS"

let vdi_location_missing = add_error "VDI_LOCATION_MISSING"

let vdi_content_id_missing = add_error "VDI_CONTENT_ID_MISSING"

let vdi_missing = add_error "VDI_MISSING"

let vdi_incompatible_type = add_error "VDI_INCOMPATIBLE_TYPE"

let vdi_not_managed = add_error "VDI_NOT_MANAGED"

let vdi_io_error = add_error "VDI_IO_ERROR"

let vdi_on_boot_mode_incompatible_with_operation =
  add_error "VDI_ON_BOOT_MODE_INCOMPATIBLE_WITH_OPERATION"

let vdi_not_in_map = add_error "VDI_NOT_IN_MAP"

let vdi_cbt_enabled = add_error "VDI_CBT_ENABLED"

let vdi_no_cbt_metadata = add_error "VDI_NO_CBT_METADATA"

let vdi_is_encrypted = add_error "VDI_IS_ENCRYPTED"

let vif_not_in_map = add_error "VIF_NOT_IN_MAP"

let cannot_create_state_file = add_error "CANNOT_CREATE_STATE_FILE"

let operation_partially_failed = add_error "OPERATION_PARTIALLY_FAILED"

let sr_uuid_exists = add_error "SR_UUID_EXISTS"

let sr_no_pbds = add_error "SR_HAS_NO_PBDS"

let sr_has_multiple_pbds = add_error "SR_HAS_MULTIPLE_PBDS"

let sr_backend_failure = add_error "SR_BACKEND_FAILURE"

let sr_unknown_driver = add_error "SR_UNKNOWN_DRIVER"

let sr_vdi_locking_failed = add_error "SR_VDI_LOCKING_FAILED"

let sr_not_empty = add_error "SR_NOT_EMPTY"

let sr_device_in_use = add_error "SR_DEVICE_IN_USE"

let sr_operation_not_supported = add_error "SR_OPERATION_NOT_SUPPORTED"

let sr_not_sharable = add_error "SR_NOT_SHARABLE"

let sr_indestructible = add_error "SR_INDESTRUCTIBLE"

let clustered_sr_degraded = add_error "CLUSTERED_SR_DEGRADED"

let sm_plugin_communication_failure =
  add_error "SM_PLUGIN_COMMUNICATION_FAILURE"

let pbd_exists = add_error "PBD_EXISTS"

let not_implemented = add_error "NOT_IMPLEMENTED"

let device_already_attached = add_error "DEVICE_ALREADY_ATTACHED"

let device_already_detached = add_error "DEVICE_ALREADY_DETACHED"

let device_already_exists = add_error "DEVICE_ALREADY_EXISTS"

let device_not_attached = add_error "DEVICE_NOT_ATTACHED"

let network_contains_pif = add_error "NETWORK_CONTAINS_PIF"

let network_contains_vif = add_error "NETWORK_CONTAINS_VIF"

let gpu_group_contains_vgpu = add_error "GPU_GROUP_CONTAINS_VGPU"

let gpu_group_contains_pgpu = add_error "GPU_GROUP_CONTAINS_PGPU"

let gpu_group_contains_no_pgpus = add_error "GPU_GROUP_CONTAINS_NO_PGPUS"

let invalid_device = add_error "INVALID_DEVICE"

let events_lost = add_error "EVENTS_LOST"

let event_subscription_parse_failure =
  add_error "EVENT_SUBSCRIPTION_PARSE_FAILURE"

let event_from_token_parse_failure = add_error "EVENT_FROM_TOKEN_PARSE_FAILURE"

let session_not_registered = add_error "SESSION_NOT_REGISTERED"

let pgpu_in_use_by_vm = add_error "PGPU_IN_USE_BY_VM"

let pgpu_not_compatible_with_gpu_group =
  add_error "PGPU_NOT_COMPATIBLE_WITH_GPU_GROUP"

let pgpu_insufficient_capacity_for_vgpu =
  add_error "PGPU_INSUFFICIENT_CAPACITY_FOR_VGPU"

let vgpu_type_not_enabled = add_error "VGPU_TYPE_NOT_ENABLED"

let vgpu_type_not_supported = add_error "VGPU_TYPE_NOT_SUPPORTED"

let vgpu_type_no_longer_supported = add_error "VGPU_TYPE_NO_LONGER_SUPPORTED"

let vgpu_type_not_compatible_with_running_type =
  add_error "VGPU_TYPE_NOT_COMPATIBLE_WITH_RUNNING_TYPE"

let vgpu_type_not_compatible = add_error "VGPU_TYPE_NOT_COMPATIBLE"

let vgpu_destination_incompatible = add_error "VGPU_DESTINATION_INCOMPATIBLE"

let vgpu_suspension_not_supported = add_error "VGPU_SUSPENSION_NOT_SUPPORTED"

let vgpu_guest_driver_limit = add_error "VGPU_GUEST_DRIVER_LIMIT"

let nvidia_tools_error = add_error "NVIDIA_TOOLS_ERROR"

let nvidia_sriov_misconfigured = add_error "NVIDIA_SRIOV_MISCONFIGURED"

let vm_pci_bus_full = add_error "VM_PCI_BUS_FULL"

let import_error_generic = add_error "IMPORT_ERROR"

let import_error_premature_eof = add_error "IMPORT_ERROR_PREMATURE_EOF"

let import_error_some_checksums_failed =
  add_error "IMPORT_ERROR_SOME_CHECKSUMS_FAILED"

let import_error_cannot_handle_chunked =
  add_error "IMPORT_ERROR_CANNOT_HANDLE_CHUNKED"

let import_error_failed_to_find_object =
  add_error "IMPORT_ERROR_FAILED_TO_FIND_OBJECT"

let import_error_attached_disks_not_found =
  add_error "IMPORT_ERROR_ATTACHED_DISKS_NOT_FOUND"

let import_error_unexpected_file = add_error "IMPORT_ERROR_UNEXPECTED_FILE"

let import_incompatible_version = add_error "IMPORT_INCOMPATIBLE_VERSION"

let restore_incompatible_version = add_error "RESTORE_INCOMPATIBLE_VERSION"

let restore_target_missing_device = add_error "RESTORE_TARGET_MISSING_DEVICE"

let restore_target_mgmt_if_not_in_backup =
  add_error "RESTORE_TARGET_MGMT_IF_NOT_IN_BACKUP"

let pool_not_in_emergency_mode = add_error "NOT_IN_EMERGENCY_MODE"

let pool_hosts_not_compatible = add_error "HOSTS_NOT_COMPATIBLE"

let pool_hosts_not_homogeneous = add_error "HOSTS_NOT_HOMOGENEOUS"

let pool_joining_host_cannot_contain_shared_SRs =
  add_error "JOINING_HOST_CANNOT_CONTAIN_SHARED_SRS"

let pool_joining_host_cannot_have_running_or_suspended_VMs =
  add_error "JOINING_HOST_CANNOT_HAVE_RUNNING_OR_SUSPENDED_VMS"

let pool_joining_host_cannot_have_running_VMs =
  add_error "JOINING_HOST_CANNOT_HAVE_RUNNING_VMS"

let pool_joining_host_cannot_have_vms_with_current_operations =
  add_error "JOINING_HOST_CANNOT_HAVE_VMS_WITH_CURRENT_OPERATIONS"

let pool_joining_host_cannot_be_master_of_other_hosts =
  add_error "JOINING_HOST_CANNOT_BE_MASTER_OF_OTHER_HOSTS"

let pool_joining_host_connection_failed =
  add_error "JOINING_HOST_CONNECTION_FAILED"

let pool_joining_host_service_failed = add_error "JOINING_HOST_SERVICE_FAILED"

let pool_joining_host_must_have_physical_management_nic =
  add_error "POOL_JOINING_HOST_MUST_HAVE_PHYSICAL_MANAGEMENT_NIC"

let pool_joining_external_auth_mismatch =
  add_error "POOL_JOINING_EXTERNAL_AUTH_MISMATCH"

let pool_joining_host_must_have_same_product_version =
  add_error "POOL_JOINING_HOST_MUST_HAVE_SAME_PRODUCT_VERSION"

let pool_joining_host_must_have_same_api_version =
  add_error "POOL_JOINING_HOST_MUST_HAVE_SAME_API_VERSION"

let pool_joining_host_must_have_same_db_schema =
  add_error "POOL_JOINING_HOST_MUST_HAVE_SAME_DB_SCHEMA"

let pool_joining_host_must_only_have_physical_pifs =
  add_error "POOL_JOINING_HOST_MUST_ONLY_HAVE_PHYSICAL_PIFS"

let pool_joining_host_management_vlan_does_not_match =
  add_error "POOL_JOINING_HOST_MANAGEMENT_VLAN_DOES_NOT_MATCH"

let pool_joining_host_has_non_management_vlans =
  add_error "POOL_JOINING_HOST_HAS_NON_MANAGEMENT_VLANS"

let pool_joining_host_has_bonds = add_error "POOL_JOINING_HOST_HAS_BONDS"

let pool_joining_host_has_tunnels = add_error "POOL_JOINING_HOST_HAS_TUNNELS"

let pool_joining_host_has_network_sriovs =
  add_error "POOL_JOINING_HOST_HAS_NETWORK_SRIOVS"

let pool_joining_host_tls_verification_mismatch =
  add_error "POOL_JOINING_HOST_TLS_VERIFICATION_MISMATCH"

let pool_joining_host_ca_certificates_conflict =
  add_error "POOL_JOINING_HOST_CA_CERTIFICATES_CONFLICT"

let pool_joining_sm_features_incompatible =
  add_error "POOL_JOINING_SM_FEATURES_INCOMPATIBLE"

(*workload balancing*)
let wlb_not_initialized = add_error "WLB_NOT_INITIALIZED"

let wlb_disabled = add_error "WLB_DISABLED"

let wlb_connection_refused = add_error "WLB_CONNECTION_REFUSED"

let wlb_unknown_host = add_error "WLB_UNKNOWN_HOST"

let wlb_timeout = add_error "WLB_TIMEOUT"

let wlb_authentication_failed = add_error "WLB_AUTHENTICATION_FAILED"

let wlb_malformed_request = add_error "WLB_MALFORMED_REQUEST"

let wlb_malformed_response = add_error "WLB_MALFORMED_RESPONSE"

let wlb_xenserver_connection_refused =
  add_error "WLB_XENSERVER_CONNECTION_REFUSED"

let wlb_xenserver_unknown_host = add_error "WLB_XENSERVER_UNKNOWN_HOST"

let wlb_xenserver_timeout = add_error "WLB_XENSERVER_TIMEOUT"

let wlb_xenserver_authentication_failed =
  add_error "WLB_XENSERVER_AUTHENTICATION_FAILED"

let wlb_xenserver_malformed_response =
  add_error "WLB_XENSERVER_MALFORMED_RESPONSE"

let wlb_internal_error = add_error "WLB_INTERNAL_ERROR"

let wlb_url_invalid = add_error "WLB_URL_INVALID"

let wlb_connection_reset = add_error "WLB_CONNECTION_RESET"

let sr_not_shared = add_error "SR_NOT_SHARED"

let default_sr_not_found = add_error "DEFAULT_SR_NOT_FOUND"

let task_cancelled = add_error "TASK_CANCELLED"

let too_many_pending_tasks = add_error "TOO_MANY_PENDING_TASKS"

let too_busy = add_error "TOO_BUSY"

let out_of_space = add_error "OUT_OF_SPACE"

let invalid_patch = add_error "INVALID_PATCH"

let invalid_update = add_error "INVALID_UPDATE"

let invalid_patch_with_log = add_error "INVALID_PATCH_WITH_LOG"

let patch_already_exists = add_error "PATCH_ALREADY_EXISTS"

let update_already_exists = add_error "UPDATE_ALREADY_EXISTS"

let patch_is_applied = add_error "PATCH_IS_APPLIED"

let update_is_applied = add_error "UPDATE_IS_APPLIED"

let cannot_find_patch = add_error "CANNOT_FIND_PATCH"

let cannot_find_update = add_error "CANNOT_FIND_UPDATE"

let cannot_fetch_patch = add_error "CANNOT_FETCH_PATCH"

let patch_already_applied = add_error "PATCH_ALREADY_APPLIED"

let update_already_applied = add_error "UPDATE_ALREADY_APPLIED"

let update_already_applied_in_pool = add_error "UPDATE_ALREADY_APPLIED_IN_POOL"

let update_pool_apply_failed = add_error "UPDATE_POOL_APPLY_FAILED"

let could_not_update_igmp_snooping_everywhere =
  add_error "COULD_NOT_UPDATE_IGMP_SNOOPING_EVERYWHERE"

let update_apply_failed = add_error "UPDATE_APPLY_FAILED"

let update_precheck_failed_unknown_error =
  add_error "UPDATE_PRECHECK_FAILED_UNKNOWN_ERROR"

let update_precheck_failed_prerequisite_missing =
  add_error "UPDATE_PRECHECK_FAILED_PREREQUISITE_MISSING"

let update_precheck_failed_conflict_present =
  add_error "UPDATE_PRECHECK_FAILED_CONFLICT_PRESENT"

let update_precheck_failed_wrong_server_version =
  add_error "UPDATE_PRECHECK_FAILED_WRONG_SERVER_VERSION"

let update_precheck_failed_gpgkey_not_imported =
  add_error "UPDATE_PRECHECK_FAILED_GPGKEY_NOT_IMPORTED"

let patch_precheck_failed_unknown_error =
  add_error "PATCH_PRECHECK_FAILED_UNKNOWN_ERROR"

let patch_precheck_failed_prerequisite_missing =
  add_error "PATCH_PRECHECK_FAILED_PREREQUISITE_MISSING"

let patch_precheck_failed_wrong_server_version =
  add_error "PATCH_PRECHECK_FAILED_WRONG_SERVER_VERSION"

let patch_precheck_failed_wrong_server_build =
  add_error "PATCH_PRECHECK_FAILED_WRONG_SERVER_BUILD"

let patch_precheck_failed_vm_running =
  add_error "PATCH_PRECHECK_FAILED_VM_RUNNING"

let patch_precheck_failed_out_of_space =
  add_error "PATCH_PRECHECK_FAILED_OUT_OF_SPACE"

let update_precheck_failed_out_of_space =
  add_error "UPDATE_PRECHECK_FAILED_OUT_OF_SPACE"

let patch_precheck_tools_iso_mounted =
  add_error "PATCH_PRECHECK_FAILED_ISO_MOUNTED"

let patch_apply_failed = add_error "PATCH_APPLY_FAILED"

let patch_apply_failed_backup_files_exist =
  add_error "PATCH_APPLY_FAILED_BACKUP_FILES_EXIST"

let cannot_find_oem_backup_partition =
  add_error "CANNOT_FIND_OEM_BACKUP_PARTITION"

let only_allowed_on_oem_edition = add_error "ONLY_ALLOWED_ON_OEM_EDITION"

let not_allowed_on_oem_edition = add_error "NOT_ALLOWED_ON_OEM_EDITION"

let cannot_find_state_partition = add_error "CANNOT_FIND_STATE_PARTITION"

let backup_script_failed = add_error "BACKUP_SCRIPT_FAILED"

let restore_script_failed = add_error "RESTORE_SCRIPT_FAILED"

let license_expired = add_error "LICENSE_EXPIRED"

let license_restriction = add_error "LICENCE_RESTRICTION"

let license_does_not_support_pooling =
  add_error "LICENSE_DOES_NOT_SUPPORT_POOLING"

let license_host_pool_mismatch = add_error "LICENSE_HOST_POOL_MISMATCH"

let license_processing_error = add_error "LICENSE_PROCESSING_ERROR"

let license_cannot_downgrade_in_pool =
  add_error "LICENSE_CANNOT_DOWNGRADE_WHILE_IN_POOL"

let license_does_not_support_xha = add_error "LICENSE_DOES_NOT_SUPPORT_XHA"

let v6d_failure = add_error "V6D_FAILURE"

let invalid_edition = add_error "INVALID_EDITION"

let missing_connection_details = add_error "MISSING_CONNECTION_DETAILS"

let license_checkout_error = add_error "LICENSE_CHECKOUT_ERROR"

let license_file_deprecated = add_error "LICENSE_FILE_DEPRECATED"

let activation_while_not_free = add_error "ACTIVATION_WHILE_NOT_FREE"

let feature_restricted = add_error "FEATURE_RESTRICTED"

let xmlrpc_unmarshal_failure = add_error "XMLRPC_UNMARSHAL_FAILURE"

let duplicate_vm = add_error "DUPLICATE_VM"

let duplicate_mac_seed = add_error "DUPLICATE_MAC_SEED"

let client_error = add_error "CLIENT_ERROR"

let ballooning_disabled = add_error "BALLOONING_DISABLED"

let ballooning_timeout_before_migration =
  add_error "BALLOONING_TIMEOUT_BEFORE_MIGRATION"

let ha_host_is_armed = add_error "HA_HOST_IS_ARMED"

let ha_is_enabled = add_error "HA_IS_ENABLED"

let ha_not_enabled = add_error "HA_NOT_ENABLED"

let ha_enable_in_progress = add_error "HA_ENABLE_IN_PROGRESS"

let ha_disable_in_progress = add_error "HA_DISABLE_IN_PROGRESS"

let ha_not_installed = add_error "HA_NOT_INSTALLED"

let ha_host_cannot_see_peers = add_error "HA_HOST_CANNOT_SEE_PEERS"

let ha_too_few_hosts = add_error "HA_TOO_FEW_HOSTS"

let ha_should_be_fenced = add_error "HA_SHOULD_BE_FENCED"

let ha_abort_new_master = add_error "HA_ABORT_NEW_MASTER"

let ha_no_plan = add_error "HA_NO_PLAN"

let ha_lost_statefile = add_error "HA_LOST_STATEFILE"

let ha_pool_is_enabled_but_host_is_disabled =
  add_error "HA_POOL_IS_ENABLED_BUT_HOST_IS_DISABLED"

let ha_heartbeat_daemon_startup_failed =
  add_error "HA_HEARTBEAT_DAEMON_STARTUP_FAILED"

let ha_host_cannot_access_statefile =
  add_error "HA_HOST_CANNOT_ACCESS_STATEFILE"

let ha_failed_to_form_liveset = add_error "HA_FAILED_TO_FORM_LIVESET"

let ha_cannot_change_bond_status_of_mgmt_iface =
  add_error "HA_CANNOT_CHANGE_BOND_STATUS_OF_MGMT_IFACE"

(* CA-16480: prevent configuration errors which nullify xHA goodness *)
let ha_constraint_violation_sr_not_shared =
  add_error "HA_CONSTRAINT_VIOLATION_SR_NOT_SHARED"

let ha_constraint_violation_network_not_shared =
  add_error "HA_CONSTRAINT_VIOLATION_NETWORK_NOT_SHARED"

let ha_operation_would_break_failover_plan =
  add_error "HA_OPERATION_WOULD_BREAK_FAILOVER_PLAN"

let incompatible_statefile_sr = add_error "INCOMPATIBLE_STATEFILE_SR"

let incompatible_cluster_stack_active =
  add_error "INCOMPATIBLE_CLUSTER_STACK_ACTIVE"

let cannot_evacuate_host = add_error "CANNOT_EVACUATE_HOST"

let host_evacuate_in_progress = add_error "HOST_EVACUATE_IN_PROGRESS"

let system_status_retrieval_failed = add_error "SYSTEM_STATUS_RETRIEVAL_FAILED"

let system_status_must_use_tar_on_oem =
  add_error "SYSTEM_STATUS_MUST_USE_TAR_ON_OEM"

let xapi_hook_failed = add_error "XAPI_HOOK_FAILED"

let no_local_storage = add_error "NO_LOCAL_STORAGE"

let xenapi_missing_plugin = add_error "XENAPI_MISSING_PLUGIN"

let xenapi_plugin_failure = add_error "XENAPI_PLUGIN_FAILURE"

let sr_attached = add_error "SR_ATTACHED"

let sr_not_attached = add_error "SR_NOT_ATTACHED"

let domain_builder_error = add_error "DOMAIN_BUILDER_ERROR"

let auth_already_enabled = add_error "AUTH_ALREADY_ENABLED"

let auth_unknown_type = add_error "AUTH_UNKNOWN_TYPE"

let auth_is_disabled = add_error "AUTH_IS_DISABLED"

let auth_suffix_wrong_credentials = "_WRONG_CREDENTIALS"

let auth_suffix_permission_denied = "_PERMISSION_DENIED"

let auth_suffix_domain_lookup_failed = "_DOMAIN_LOOKUP_FAILED"

let auth_suffix_unavailable = "_UNAVAILABLE"

let auth_suffix_invalid_ou = "_INVALID_OU"

let auth_suffix_invalid_account = "_INVALID_ACCOUNT"

let auth_enable_failed = add_error "AUTH_ENABLE_FAILED"

let auth_enable_failed_wrong_credentials =
  add_error $ auth_enable_failed ^ auth_suffix_wrong_credentials

let auth_enable_failed_permission_denied =
  add_error $ auth_enable_failed ^ auth_suffix_permission_denied

let auth_enable_failed_domain_lookup_failed =
  add_error $ auth_enable_failed ^ auth_suffix_domain_lookup_failed

let auth_enable_failed_unavailable =
  add_error $ auth_enable_failed ^ auth_suffix_unavailable

let auth_enable_failed_invalid_ou =
  add_error $ auth_enable_failed ^ auth_suffix_invalid_ou

let auth_enable_failed_invalid_account =
  add_error $ auth_enable_failed ^ auth_suffix_invalid_account

let auth_disable_failed = add_error "AUTH_DISABLE_FAILED"

let auth_disable_failed_wrong_credentials =
  add_error $ auth_disable_failed ^ auth_suffix_wrong_credentials

let auth_disable_failed_permission_denied =
  add_error $ auth_disable_failed ^ auth_suffix_permission_denied

let pool_auth_already_enabled = add_error "POOL_AUTH_ALREADY_ENABLED"

let pool_auth_prefix = "POOL_"

let pool_auth_enable_failed = add_error $ pool_auth_prefix ^ auth_enable_failed

let pool_auth_enable_failed_wrong_credentials =
  add_error $ pool_auth_enable_failed ^ auth_suffix_wrong_credentials

let pool_auth_enable_failed_permission_denied =
  add_error $ pool_auth_enable_failed ^ auth_suffix_permission_denied

let pool_auth_enable_failed_domain_lookup_failed =
  add_error $ pool_auth_enable_failed ^ auth_suffix_domain_lookup_failed

let pool_auth_enable_failed_unavailable =
  add_error $ pool_auth_enable_failed ^ auth_suffix_unavailable

let pool_auth_enable_failed_invalid_ou =
  add_error $ pool_auth_enable_failed ^ auth_suffix_invalid_ou

let pool_auth_enable_failed_invalid_account =
  add_error $ pool_auth_enable_failed ^ auth_suffix_invalid_account

let pool_auth_enable_failed_duplicate_hostname =
  add_error $ pool_auth_enable_failed ^ "_DUPLICATE_HOSTNAME"

let pool_auth_disable_failed =
  add_error $ pool_auth_prefix ^ auth_disable_failed

let pool_auth_disable_failed_wrong_credentials =
  add_error $ pool_auth_disable_failed ^ auth_suffix_wrong_credentials

let pool_auth_disable_failed_permission_denied =
  add_error $ pool_auth_disable_failed ^ auth_suffix_permission_denied

let pool_auth_disable_failed_invalid_account =
  add_error $ pool_auth_disable_failed ^ auth_suffix_invalid_account

let subject_cannot_be_resolved = add_error "SUBJECT_CANNOT_BE_RESOLVED"

let auth_service_error = add_error "AUTH_SERVICE_ERROR"

let subject_already_exists = add_error "SUBJECT_ALREADY_EXISTS"

let role_not_found = add_error "ROLE_NOT_FOUND"

let role_already_exists = add_error "ROLE_ALREADY_EXISTS"

let rbac_permission_denied = add_error "RBAC_PERMISSION_DENIED"

let certificate_does_not_exist = add_error "CERTIFICATE_DOES_NOT_EXIST"

let certificate_already_exists = add_error "CERTIFICATE_ALREADY_EXISTS"

let certificate_name_invalid = add_error "CERTIFICATE_NAME_INVALID"

let certificate_corrupt = add_error "CERTIFICATE_CORRUPT"

let certificate_library_corrupt = add_error "CERTIFICATE_LIBRARY_CORRUPT"

let crl_does_not_exist = add_error "CRL_DOES_NOT_EXIST"

let crl_already_exists = add_error "CRL_ALREADY_EXISTS"

let crl_name_invalid = add_error "CRL_NAME_INVALID"

let crl_corrupt = add_error "CRL_CORRUPT"

let server_certificate_key_invalid = add_error "SERVER_CERTIFICATE_KEY_INVALID"

let server_certificate_key_algorithm_not_supported =
  add_error "SERVER_CERTIFICATE_KEY_ALGORITHM_NOT_SUPPORTED"

let server_certificate_key_rsa_length_not_supported =
  add_error "SERVER_CERTIFICATE_KEY_RSA_LENGTH_NOT_SUPPORTED"

let server_certificate_key_rsa_multi_not_supported =
  add_error "SERVER_CERTIFICATE_KEY_RSA_MULTI_NOT_SUPPORTED"

let server_certificate_invalid = add_error "SERVER_CERTIFICATE_INVALID"

let ca_certificate_invalid = add_error "CA_CERTIFICATE_INVALID"

let server_certificate_key_mismatch =
  add_error "SERVER_CERTIFICATE_KEY_MISMATCH"

let server_certificate_not_valid_yet =
  add_error "SERVER_CERTIFICATE_NOT_VALID_YET"

let ca_certificate_not_valid_yet = add_error "CA_CERTIFICATE_NOT_VALID_YET"

let server_certificate_expired = add_error "SERVER_CERTIFICATE_EXPIRED"

let ca_certificate_expired = add_error "CA_CERTIFICATE_EXPIRED"

let server_certificate_signature_not_supported =
  add_error "SERVER_CERTIFICATE_SIGNATURE_NOT_SUPPORTED"

let server_certificate_chain_invalid =
  add_error "SERVER_CERTIFICATE_CHAIN_INVALID"

let vmpp_has_vm = add_error "VMPP_HAS_VM"

let vmpp_archive_more_frequent_than_backup =
  add_error "VMPP_ARCHIVE_MORE_FREQUENT_THAN_BACKUP"

let vm_assigned_to_protection_policy =
  add_error "VM_ASSIGNED_TO_PROTECTION_POLICY"

let vmss_has_vm = add_error "VMSS_HAS_VM"

let vm_assigned_to_snapshot_schedule =
  add_error "VM_ASSIGNED_TO_SNAPSHOT_SCHEDULE"

let ssl_verify_error = add_error "SSL_VERIFY_ERROR"

let cannot_enable_redo_log = add_error "CANNOT_ENABLE_REDO_LOG"

let redo_log_is_enabled = add_error "REDO_LOG_IS_ENABLED"

let vm_bios_strings_already_set = add_error "VM_BIOS_STRINGS_ALREADY_SET"

let invalid_feature_string = add_error "INVALID_FEATURE_STRING"

let cpu_feature_masking_not_supported =
  add_error "CPU_FEATURE_MASKING_NOT_SUPPORTED"

let feature_requires_hvm = add_error "FEATURE_REQUIRES_HVM"

(* Disaster recovery *)
let vdi_contains_metadata_of_this_pool =
  add_error "VDI_CONTAINS_METADATA_OF_THIS_POOL"

let no_more_redo_logs_allowed = add_error "NO_MORE_REDO_LOGS_ALLOWED"

let could_not_import_database = add_error "COULD_NOT_IMPORT_DATABASE"

let vm_incompatible_with_this_host = add_error "VM_INCOMPATIBLE_WITH_THIS_HOST"

let cannot_destroy_disaster_recovery_task =
  add_error "CANNOT_DESTROY_DISASTER_RECOVERY_TASK"

let vm_is_part_of_an_appliance = add_error "VM_IS_PART_OF_AN_APPLIANCE"

let vm_to_import_is_not_newer_version =
  add_error "VM_TO_IMPORT_IS_NOT_NEWER_VERSION"

let suspend_vdi_replacement_is_not_identical =
  add_error "SUSPEND_VDI_REPLACEMENT_IS_NOT_IDENTICAL"

let vdi_copy_failed = add_error "VDI_COPY_FAILED"

let vdi_needs_vm_for_migrate = add_error "VDI_NEEDS_VM_FOR_MIGRATE"

let vm_has_too_many_snapshots = add_error "VM_HAS_TOO_MANY_SNAPSHOTS"

let vm_has_checkpoint = add_error "VM_HAS_CHECKPOINT"

let mirror_failed = add_error "MIRROR_FAILED"

let too_many_storage_migrates = add_error "TOO_MANY_STORAGE_MIGRATES"

let sr_does_not_support_migration = add_error "SR_DOES_NOT_SUPPORT_MIGRATION"

let unimplemented_in_sm_backend = add_error "UNIMPLEMENTED_IN_SM_BACKEND"

let vm_call_plugin_rate_limit = add_error "VM_CALL_PLUGIN_RATE_LIMIT"

let suspend_image_not_accessible = add_error "SUSPEND_IMAGE_NOT_ACCESSIBLE"

(* PVS *)
let pvs_site_contains_running_proxies =
  add_error "PVS_SITE_CONTAINS_RUNNING_PROXIES"

let pvs_site_contains_servers = add_error "PVS_SITE_CONTAINS_SERVERS"

let pvs_cache_storage_already_present =
  add_error "PVS_CACHE_STORAGE_ALREADY_PRESENT"

let pvs_cache_storage_is_in_use = add_error "PVS_CACHE_STORAGE_IS_IN_USE"

let pvs_proxy_already_present = add_error "PVS_PROXY_ALREADY_PRESENT"

let pvs_server_address_in_use = add_error "PVS_SERVER_ADDRESS_IN_USE"

let pvs_vif_must_be_first_device = add_error "PVS_VIF_MUST_BE_FIRST_DEVICE"

let pvs_proxy_present_on_higher_vif_device =
  add_error "PVS_PROXY_PRESENT_ON_HIGHER_VIF_DEVICE"

let extension_protocol_failure = add_error "EXTENSION_PROTOCOL_FAILURE"

let usb_group_contains_vusb = add_error "USB_GROUP_CONTAINS_VUSB"

let usb_group_contains_pusb = add_error "USB_GROUP_CONTAINS_PUSB"

let usb_group_contains_no_pusbs = add_error "USB_GROUP_CONTAINS_NO_PUSBS"

let usb_group_conflict = add_error "USB_GROUP_CONFLICT"

let usb_already_attached = add_error "USB_ALREADY_ATTACHED"

let too_many_vusbs = add_error "TOO_MANY_VUSBS"

let pusb_vdi_conflict = add_error "PUSB_VDI_CONFLICT"

let vm_has_vusbs = add_error "VM_HAS_VUSBS"

let cluster_has_no_certificate = add_error "CLUSTER_HAS_NO_CERTIFICATE"

let cluster_create_in_progress = add_error "CLUSTER_CREATE_IN_PROGRESS"

let cluster_already_exists = add_error "CLUSTER_ALREADY_EXISTS"

let clustering_enabled = add_error "CLUSTERING_ENABLED"

let clustering_disabled = add_error "CLUSTERING_DISABLED"

let cluster_does_not_have_one_node = add_error "CLUSTER_DOES_NOT_HAVE_ONE_NODE"

let cluster_host_is_last = add_error "CLUSTER_HOST_IS_LAST"

let no_compatible_cluster_host = add_error "NO_COMPATIBLE_CLUSTER_HOST"

let cluster_force_destroy_failed = add_error "CLUSTER_FORCE_DESTROY_FAILED"

let cluster_stack_in_use = add_error "CLUSTER_STACK_IN_USE"

let invalid_cluster_stack = add_error "INVALID_CLUSTER_STACK"

let pif_not_attached_to_host = add_error "PIF_NOT_ATTACHED_TO_HOST"

let cluster_host_not_joined = add_error "CLUSTER_HOST_NOT_JOINED"

let no_cluster_hosts_reachable = add_error "NO_CLUSTER_HOSTS_REACHABLE"

let xen_incompatible = add_error "XEN_INCOMPATIBLE"

let vcpu_max_not_cores_per_socket_multiple =
  add_error "VCPU_MAX_NOT_CORES_PER_SOCKET_MULTIPLE"

let designate_new_master_in_progress =
  add_error "DESIGNATE_NEW_MASTER_IN_PROGRESS"

let pool_secret_rotation_pending = add_error "POOL_SECRET_ROTATION_PENDING"

let tls_verification_enable_in_progress =
  add_error "TLS_VERIFICATION_ENABLE_IN_PROGRESS"

let cert_refresh_in_progress = add_error "CERT_REFRESH_IN_PROGRESS"

let configure_repositories_in_progress =
  add_error "CONFIGURE_REPOSITORIES_IN_PROGRESS"

let invalid_base_url = add_error "INVALID_BASE_URL"

let invalid_gpgkey_path = add_error "INVALID_GPGKEY_PATH"

let repository_already_exists = add_error "REPOSITORY_ALREADY_EXISTS"

let bundle_repository_already_exists =
  add_error "BUNDLE_REPOSITORY_ALREADY_EXISTS"

let bundle_unpack_failed = add_error "BUNDLE_UNPACK_FAILED"

let bundle_repo_not_enabled = add_error "BUNDLE_REPO_NOT_ENABLED"

let can_not_sync_updates = add_error "CAN_NOT_SYNC_UPDATES"

let bundle_repo_should_be_single_enabled =
  add_error "BUNDLE_REPO_SHOULD_BE_SINGLE_ENABLED"

let repository_is_in_use = add_error "REPOSITORY_IS_IN_USE"

let repository_cleanup_failed = add_error "REPOSITORY_CLEANUP_FAILED"

let no_repository_enabled = add_error "NO_REPOSITORY_ENABLED"

let multiple_update_repositories_enabled =
  add_error "MULTIPLE_UPDATE_REPOSITORIES_ENABLED"

let sync_updates_in_progress = add_error "SYNC_UPDATES_IN_PROGRESS"

let sync_bundle_in_progress = add_error "SYNC_BUNDLE_IN_PROGRESS"

let reposync_failed = add_error "REPOSYNC_FAILED"

let bundle_sync_failed = add_error "BUNDLE_SYNC_FAILED"

let createrepo_failed = add_error "CREATEREPO_FAILED"

let invalid_updateinfo_xml = add_error "INVALID_UPDATEINFO_XML"

let get_host_updates_failed = add_error "GET_HOST_UPDATES_FAILED"

let invalid_repomd_xml = add_error "INVALID_REPOMD_XML"

let get_updates_failed = add_error "GET_UPDATES_FAILED"

let get_updates_in_progress = add_error "GET_UPDATES_IN_PROGRESS"

let apply_updates_in_progress = add_error "APPLY_UPDATES_IN_PROGRESS"

let apply_updates_failed = add_error "APPLY_UPDATES_FAILED"

let apply_guidance_failed = add_error "APPLY_GUIDANCE_FAILED"

let updateinfo_hash_mismatch = add_error "UPDATEINFO_HASH_MISMATCH"

let cannot_restart_device_model = add_error "CANNOT_RESTART_DEVICE_MODEL"

let invalid_repository_proxy_url = add_error "INVALID_REPOSITORY_PROXY_URL"

let invalid_repository_proxy_credential =
  add_error "INVALID_REPOSITORY_PROXY_CREDENTIAL"

let invalid_repository_domain_allowlist =
  add_error "INVALID_REPOSITORY_DOMAIN_ALLOWLIST"

let apply_livepatch_failed = add_error "APPLY_LIVEPATCH_FAILED"

let invalid_update_sync_day = add_error "INVALID_UPDATE_SYNC_DAY"

let no_repositories_configured = add_error "NO_REPOSITORIES_CONFIGURED"

let host_pending_mandatory_guidances_not_empty =
  add_error "HOST_PENDING_MANDATORY_GUIDANCE_NOT_EMPTY"

let host_evacuation_is_required = add_error "HOST_EVACUATION_IS_REQUIRED"

(* VTPMs *)

let vtpm_max_amount_reached = add_error "VTPM_MAX_AMOUNT_REACHED"

(* Telemetry *)
let telemetry_next_collection_too_late =
  add_error "TELEMETRY_NEXT_COLLECTION_TOO_LATE"

(* FIPS/CC_PREPARATIONS *)
let illegal_in_fips_mode = add_error "ILLEGAL_IN_FIPS_MODE"

let too_many_groups = add_error "TOO_MANY_GROUPS"
