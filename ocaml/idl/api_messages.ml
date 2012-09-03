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
let msgList = ref []

let addMessage x =
	let _ = msgList := x::!msgList in
	x

let license_does_not_support_pooling = addMessage "LICENSE_DOES_NOT_SUPPORT_POOLING"
(* Fired by license-check.py *)
let license_expires_soon = addMessage "LICENSE_EXPIRES_SOON"

let ha_statefile_lost = addMessage "HA_STATEFILE_LOST"
let ha_statefile_lost_priority = 10L

let ha_heartbeat_approaching_timeout = addMessage "HA_HEARTBEAT_APPROACHING_TIMEOUT"
let ha_statefile_approaching_timeout = addMessage "HA_STATEFILE_APPROACHING_TIMEOUT"
let ha_xapi_healthcheck_approaching_timeout = addMessage "HA_XAPI_HEALTHCHECK_APPROACHING_TIMEOUT"

let ha_network_bonding_error = addMessage "HA_NETWORK_BONDING_ERROR"
let ha_network_bonding_error_priority = 10L

let ha_pool_overcommitted = addMessage "HA_POOL_OVERCOMMITTED"
let ha_pool_overcommitted_priority = 1L (* GUI maximizes ntol, which means this is often subsumed by DROP_IN_PLAN_EXISTS_FOR; hence low priority *)

let ha_pool_drop_in_plan_exists_for = addMessage "HA_POOL_DROP_IN_PLAN_EXISTS_FOR"
let ha_protected_vm_restart_failed = addMessage "HA_PROTECTED_VM_RESTART_FAILED"

let ha_host_failed = addMessage "HA_HOST_FAILED"
let ha_host_failed_priority = 10L

let ha_host_was_fenced = addMessage "HA_HOST_WAS_FENCED"

let redo_log_healthy = addMessage "METADATA_LUN_HEALTHY"
let redo_log_broken = addMessage "METADATA_LUN_BROKEN"

let ip_configured_pif_can_unplug = addMessage "IP_CONFIGURED_PIF_CAN_UNPLUG"

let vif_qos_failed = addMessage "VIF_QOS_FAILED"
let vbd_qos_failed = addMessage "VBD_QOS_FAILED"
let vcpu_qos_failed = addMessage "VCPU_QOS_FAILED"

let vm_started = addMessage "VM_STARTED"
let vm_shutdown = addMessage "VM_SHUTDOWN"
let vm_rebooted = addMessage "VM_REBOOTED"
let vm_suspended = addMessage "VM_SUSPENDED"
let vm_resumed = addMessage "VM_RESUMED"
let vm_crashed = addMessage "VM_CRASHED"
let vm_cloned = addMessage "VM_CLONED"
let host_sync_data_failed = addMessage "HOST_SYNC_DATA_FAILED" (* Kept for backward compatibility. *)
let host_clock_skew_detected = addMessage "HOST_CLOCK_SKEW_DETECTED"
let host_clock_skew_detected_priority = 10L
let host_clock_went_backwards = addMessage "HOST_CLOCK_WENT_BACKWARDS"
let host_clock_went_backwards_priority = 10L

let pool_master_transition = addMessage "POOL_MASTER_TRANSITION"

let pbd_plug_failed_on_server_start = addMessage "PBD_PLUG_FAILED_ON_SERVER_START"

let alarm = addMessage "ALARM"

let wlb_failed = addMessage "WLB_CONSULTATION_FAILED"
let wlb_optimization_alert = addMessage "WLB_OPTIMIZATION_ALERT"

let auth_external_init_failed = addMessage "EXTAUTH_INIT_IN_HOST_FAILED"
let auth_external_pool_non_homogeneous = addMessage "EXTAUTH_IN_POOL_IS_NON_HOMOGENEOUS"

let multipath_periodic_alert = addMessage "MULTIPATH_PERIODIC_ALERT"

let v6_server_up = addMessage "LICENSE_SERVER_CONNECTED"
let v6_server_down = addMessage "LICENSE_SERVER_UNAVAILABLE"
let v6_license_expired = addMessage "LICENSE_EXPIRED"
let v6_grace_license = addMessage "GRACE_LICENSE"
let v6_rejected = addMessage "LICENSE_NOT_AVAILABLE"
let v6_comm_error = addMessage "LICENSE_SERVER_UNREACHABLE"

(* VMPP message types *)
let vmpp_backup_lock_failed = addMessage "VMPP_SNAPSHOT_LOCK_FAILED" (*'The snapshot phase is already executing for this protection policy. Please try again later'*)
let vmpp_backup_succeeded = addMessage "VMPP_SNAPSHOT_SUCCEEDED" (*'Successfully performed the snapshot phase of the protection policy'*)
let vmpp_archive_lock_failed = addMessage "VMPP_ARCHIVE_LOCK_FAILED" (*'The archive sub-policy is already executing for some protection policy in the pool.Please try again later'*)
let vmpp_archive_failed_0 = addMessage "VMPP_ARCHIVE_FAILED_0" (*'The archive phase failed for this protection policy'*)
let vmpp_archive_suceeded = addMessage "VMPP_ARCHIVE_SUCCEEDED" (*'Successfully performed the archive phase of the protection policy'*)
let vmpp_archive_target_mount_failed = addMessage "VMPP_ARCHIVE_TARGET_MOUNT_FAILED" (*'Failed to mount the archive target. Please check the archive target configuration settings'*)
let vmpp_archive_target_unmount_failed = addMessage "VMPP_ARCHIVE_TARGET_UNMOUNT_FAILED" (*'Failed to unmount the archive target. Please make sure than the local directory was mounted successfully and has no open handles'*)
let vmpp_license_error = addMessage "VMPP_LICENSE_ERROR" (*'This operation is not allowed under your license.  Please contact your support representative'*)
let vmpp_xapi_logon_failure = addMessage "VMPP_XAPI_LOGON_FAILURE" (*'Could not login to API session.'*)
let vmpp_backup_missed_event = addMessage "VMPP_SNAPSHOT_MISSED_EVENT" (*'A scheduled snapshot event was missed due to another on-going scheduled snapshot run. This is unexpected behaviour, please re-configure your snapshot sub-policy',*)
let vmpp_archive_missed_event = addMessage "VMPP_ARCHIVE_MISSED_EVENT" (*'A scheduled archive event was missed due to another on-going scheduled archive run. This is unexpected behaviour, please re-configure your archive sub-policy'*)
let vmpp_backup_failed = addMessage "VMPP_SNAPSHOT_FAILED" (*'The snapshot phase of the protection policy failed.'*)
let vmpp_snapshot_archive_already_exists = addMessage "VMPP_SNAPSHOT_ARCHIVE_ALREADY_EXISTS" (*'Failed to archive the snapshot, it has already been archived on the specified target'*)

let bond_status_changed = addMessage "BOND_STATUS_CHANGED" (* A link in a bond went down or came back up *)
