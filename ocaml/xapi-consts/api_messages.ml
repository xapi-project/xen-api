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

(*
Priority Name                  Description
-------- --------------------- -----------------------------------------------------------------------------
1        Data-loss imminent    Take action now or your data may be permanently lost (e.g. corrupted)
2        Service-loss imminent Take action now or some service(s) may fail (e.g. host / VM crash)
3        Service degraded      Take action now or some service may suffer (e.g. NIC bond degraded without HA)
4        Service recovered     Notice that something just improved (e.g. NIC bond repaired)
5        Informational         More day-to-day stuff (e.g. VM started, suspended, shutdown, rebooted etc)
*)

let msgList = ref []

let addMessage name priority =
  let msg = (name, priority) in
  let _ = msgList := msg :: !msgList in
  msg

let ha_statefile_lost = addMessage "HA_STATEFILE_LOST" 2L

let ha_heartbeat_approaching_timeout = addMessage "HA_HEARTBEAT_APPROACHING_TIMEOUT" 5L
let ha_statefile_approaching_timeout = addMessage "HA_STATEFILE_APPROACHING_TIMEOUT" 5L
let ha_xapi_healthcheck_approaching_timeout = addMessage "HA_XAPI_HEALTHCHECK_APPROACHING_TIMEOUT" 5L

let ha_network_bonding_error = addMessage "HA_NETWORK_BONDING_ERROR" 3L

let ha_pool_overcommitted = addMessage "HA_POOL_OVERCOMMITTED" 3L (* GUI maximizes ntol, which means this is often subsumed by DROP_IN_PLAN_EXISTS_FOR; hence low priority *)

let ha_pool_drop_in_plan_exists_for = addMessage "HA_POOL_DROP_IN_PLAN_EXISTS_FOR" 3L
let ha_protected_vm_restart_failed = addMessage "HA_PROTECTED_VM_RESTART_FAILED" 2L

let ha_host_failed = addMessage "HA_HOST_FAILED" 3L

let ha_host_was_fenced = addMessage "HA_HOST_WAS_FENCED" 4L

let redo_log_healthy = addMessage "METADATA_LUN_HEALTHY" 4L
let redo_log_broken = addMessage "METADATA_LUN_BROKEN" 3L

let ip_configured_pif_can_unplug = addMessage "IP_CONFIGURED_PIF_CAN_UNPLUG" 3L

let vif_qos_failed = addMessage "VIF_QOS_FAILED" 3L (* Used in idl/datamodel.ml *)
let vbd_qos_failed = addMessage "VBD_QOS_FAILED" 3L (* Used in idl/datamodel.ml *)
let vcpu_qos_failed = addMessage "VCPU_QOS_FAILED" 3L (* Used in idl/datamodel.ml *)

let vm_started = addMessage "VM_STARTED" 5L
let vm_shutdown = addMessage "VM_SHUTDOWN" 5L (* Name conflict with Api_errors *)
let vm_rebooted = addMessage "VM_REBOOTED" 5L (* Name conflict with Api_errors *)
let vm_suspended = addMessage "VM_SUSPENDED" 5L
let vm_resumed = addMessage "VM_RESUMED" 5L
let vm_crashed = addMessage "VM_CRASHED" 2L (* Name conflict with Api_errors; unused in xen-api *)
let vm_cloned = addMessage "VM_CLONED" 5L (* Prviously missing from table *)

let host_sync_data_failed = addMessage "HOST_SYNC_DATA_FAILED" 3L (* Kept for backward compatibility; used in XenCenter *)
let host_clock_skew_detected = addMessage "HOST_CLOCK_SKEW_DETECTED" 3L
let host_clock_went_backwards = addMessage "HOST_CLOCK_WENT_BACKWARDS" 1L (* Unused in xen-api *)

let pool_master_transition = addMessage "POOL_MASTER_TRANSITION" 4L

let pbd_plug_failed_on_server_start = addMessage "PBD_PLUG_FAILED_ON_SERVER_START" 3L

let alarm = addMessage "ALARM" 1L (* Previously missing from table; unused in xen-api *)

let wlb_failed = addMessage "WLB_CONSULTATION_FAILED" 3L
let wlb_optimization_alert = addMessage "WLB_OPTIMIZATION_ALERT" 3L (* Used in XenCenter *)

let auth_external_init_failed = addMessage "EXTAUTH_INIT_IN_HOST_FAILED" 2L
let auth_external_pool_non_homogeneous = addMessage "EXTAUTH_IN_POOL_IS_NON_HOMOGENEOUS" 2L

let multipath_periodic_alert = addMessage "MULTIPATH_PERIODIC_ALERT" 3L

(* Licensing alerts *)
let license_does_not_support_pooling = addMessage "LICENSE_DOES_NOT_SUPPORT_POOLING" 2L (* Name conflict with Api_errors; unused in xen-api *)
let license_expires_soon = addMessage "LICENSE_EXPIRES_SOON" 2L
let license_expired = addMessage "LICENSE_EXPIRED" 2L
let v6_server_up = addMessage "LICENSE_SERVER_CONNECTED" 4L (* Used in XenCenter *)
let v6_server_down = addMessage "LICENSE_SERVER_UNAVAILABLE" 3L (* Used in XenCenter *)
let v6_grace_license = addMessage "GRACE_LICENSE" 3L
let v6_rejected = addMessage "LICENSE_NOT_AVAILABLE" 2L
let v6_comm_error = addMessage "LICENSE_SERVER_UNREACHABLE" 2L
let v6_license_server_version_obsolete = addMessage "LICENSE_SERVER_VERSION_OBSOLETE" 2L

(* PVS alerts *)
let pvs_proxy_no_cache_sr_available = addMessage "PVS_PROXY_NO_CACHE_SR_AVAILABLE" 3L (* No cache storage available for pvs site on the host *)
let pvs_proxy_setup_failed = addMessage "PVS_PROXY_SETUP_FAILED" 3L (* Setting up pvs proxy rules or pvs proxy daemon initialisation failed internally *)
let pvs_proxy_no_server_available = addMessage "PVS_PROXY_NO_SERVER_AVAILABLE" 3L (* No pvs server available for pvs site on host *)
let pvs_proxy_sr_out_of_space = addMessage "PVS_PROXY_SR_OUT_OF_SPACE" 3L (* PVS cache storage size exceeds available space on SR *)

(* VMPP message types *)
let vmpp_snapshot_lock_failed = addMessage "VMPP_SNAPSHOT_LOCK_FAILED" 3L (*'The snapshot phase is already executing for this protection policy. Please try again later'*)
let vmpp_snapshot_succeeded = addMessage "VMPP_SNAPSHOT_SUCCEEDED" 5L (*'Successfully performed the snapshot phase of the protection policy'*)
let vmpp_snapshot_failed = addMessage "VMPP_SNAPSHOT_FAILED" 3L (*'The snapshot phase of the protection policy failed.'*)
let vmpp_archive_lock_failed = addMessage "VMPP_ARCHIVE_LOCK_FAILED" 3L (*'The archive sub-policy is already executing for some protection policy in the pool.Please try again later'*)
let vmpp_archive_failed_0 = addMessage "VMPP_ARCHIVE_FAILED_0" 3L (*'The archive phase failed for this protection policy'*)
let vmpp_archive_suceeded = addMessage "VMPP_ARCHIVE_SUCCEEDED" 5L (*'Successfully performed the archive phase of the protection policy'*)
let vmpp_archive_target_mount_failed = addMessage "VMPP_ARCHIVE_TARGET_MOUNT_FAILED" 3L (*'Failed to mount the archive target. Please check the archive target configuration settings'*)
let vmpp_archive_target_unmount_failed = addMessage "VMPP_ARCHIVE_TARGET_UNMOUNT_FAILED" 3L (*'Failed to unmount the archive target. Please make sure than the local directory was mounted successfully and has no open handles'*)
let vmpp_license_error = addMessage "VMPP_LICENSE_ERROR" 3L (*'This operation is not allowed under your license.  Please contact your support representative'*)
let vmpp_xapi_logon_failure = addMessage "VMPP_XAPI_LOGON_FAILURE" 3L (*'Could not login to API session.'*)
let vmpp_snapshot_missed_event = addMessage "VMPP_SNAPSHOT_MISSED_EVENT" 3L (*'A scheduled snapshot event was missed due to another on-going scheduled snapshot run. This is unexpected behaviour, please re-configure your snapshot sub-policy',*)
let vmpp_archive_missed_event = addMessage "VMPP_ARCHIVE_MISSED_EVENT" 3L (*'A scheduled archive event was missed due to another on-going scheduled archive run. This is unexpected behaviour, please re-configure your archive sub-policy'*)
let vmpp_snapshot_archive_already_exists = addMessage "VMPP_SNAPSHOT_ARCHIVE_ALREADY_EXISTS" 3L (*'Failed to archive the snapshot, it has already been archived on the specified target'*)

(* VMSS message types *)
let vmss_snapshot_lock_failed = addMessage "VMSS_SNAPSHOT_LOCK_FAILED" 3L (*'The snapshot is already executing for schedule snapshot. Please try again later'*)
let vmss_snapshot_succeeded = addMessage "VMSS_SNAPSHOT_SUCCEEDED" 5L (*'Successfully performed the snapshot of the schedule snapshot'*)
let vmss_snapshot_failed = addMessage "VMSS_SNAPSHOT_FAILED" 3L (*'The snapshot of the schedule snapshot has failed.'*)
let vmss_license_error = addMessage "VMSS_LICENSE_ERROR" 3L (*'This operation is not allowed under your license.  Please contact your support representative'*)
let vmss_xapi_logon_failure = addMessage "VMSS_XAPI_LOGON_FAILURE" 3L (*'Could not login to API session.'*)
let vmss_snapshot_missed_event = addMessage "VMSS_SNAPSHOT_MISSED_EVENT" 3L (*'A scheduled snapshot event was missed due to another on-going scheduled snapshot run. This is unexpected behaviour, please re-configure your schedule snapshot',*)

(* VDI message types *)
let vdi_cbt_metadata_inconsistent = addMessage "VDI_CBT_METADATA_INCONSISTENT" 3L (*'The VDI's cbt metadata is inconsistent'*)
let vdi_cbt_snapshot_failed = addMessage "VDI_CBT_SNAPSHOT_FAILED" 3L (*'Failed to snapshot the cbt enabled VDI'*)
let vdi_cbt_resize_failed = addMessage "VDI_CBT_RESIZE_FAILED" 3L (*'Resizing of the cbt enabled VDI failed'*)

let bond_status_changed = addMessage "BOND_STATUS_CHANGED" 3L (* A link in a bond went down or came back up *) (* Previously missing from table *)

let host_cpu_features_down = addMessage "HOST_CPU_FEATURES_DOWN" 3L
let host_cpu_features_up = addMessage "HOST_CPU_FEATURES_UP" 5L
let pool_cpu_features_down = addMessage "POOL_CPU_FEATURES_DOWN" 5L
let pool_cpu_features_up = addMessage "POOL_CPU_FEATURES_UP" 5L
let host_low_memory = addMessage "HOST_LOW_MEMORY" 2L

(* Cluster messages *)
let cluster_host_enable_failed = addMessage "CLUSTER_HOST_ENABLE_FAILED" 3L

(* raised by external script in clustering daemon, do not delete this: it is not dead code *)
let cluster_host_fencing = addMessage "CLUSTER_HOST_FENCING" 2L
