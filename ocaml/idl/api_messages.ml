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

let addMsg name priority =
    let msg = (name, priority) in
    let _ = msgList := msg :: !msgList in
    msg

let license_does_not_support_pooling = addMsg "LICENSE_DOES_NOT_SUPPORT_POOLING" 2L (* Unused in xen-api *)
let license_expires_soon = addMsg "LICENSE_EXPIRES_SOON" 2L (* Used by license-check.py, which may be unused? *)

let ha_statefile_lost = addMsg "HA_STATEFILE_LOST" 2L

let ha_heartbeat_approaching_timeout = addMsg "HA_HEARTBEAT_APPROACHING_TIMEOUT" 5L
let ha_statefile_approaching_timeout = addMsg "HA_STATEFILE_APPROACHING_TIMEOUT" 5L
let ha_xapi_healthcheck_approaching_timeout = addMsg "HA_XAPI_HEALTHCHECK_APPROACHING_TIMEOUT" 5L (* Previously missing from table *)

let ha_network_bonding_error = addMsg "HA_NETWORK_BONDING_ERROR" 3L

let ha_pool_overcommitted = addMsg "HA_POOL_OVERCOMMITTED" 3L (* GUI maximizes ntol, which means this is often subsumed by DROP_IN_PLAN_EXISTS_FOR; hence low priority *)

let ha_pool_drop_in_plan_exists_for = addMsg "HA_POOL_DROP_IN_PLAN_EXISTS_FOR" 3L
let ha_protected_vm_restart_failed = addMsg "HA_PROTECTED_VM_RESTART_FAILED" 2L

let ha_host_failed = addMsg "HA_HOST_FAILED" 3L

let ha_host_was_fenced = addMsg "HA_HOST_WAS_FENCED" 4L

let redo_log_healthy = addMsg "METADATA_LUN_HEALTHY" 4L
let redo_log_broken = addMsg "METADATA_LUN_BROKEN" 3L

let ip_configured_pif_can_unplug = addMsg "IP_CONFIGURED_PIF_CAN_UNPLUG" 3L

let vif_qos_failed = addMsg "VIF_QOS_FAILED" 3L (* Used in idl/datamodel.ml *)
let vbd_qos_failed = addMsg "VBD_QOS_FAILED" 3L (* Used in idl/datamodel.ml *)
let vcpu_qos_failed = addMsg "VCPU_QOS_FAILED" 3L (* Used in idl/datamodel.ml *)

let vm_started = addMsg "VM_STARTED" 5L (* Previously missing from table *)
let vm_shutdown = addMsg "VM_SHUTDOWN" 3L (* Previously missing from table *)
let vm_rebooted = addMsg "VM_REBOOTED" 3L (* Previously missing from table *)
let vm_suspended = addMsg "VM_SUSPENDED" 5L (* Previously missing from table *)
let vm_resumed = addMsg "VM_RESUMED" 5L (* Previously missing from table *)
let vm_crashed = addMsg "VM_CRASHED" 3L (* Previously missing from table; unused in xen-api *)
let vm_cloned = addMsg "VM_CLONED" 5L (* Prviously missing from table *)

let host_sync_data_failed = addMsg "HOST_SYNC_DATA_FAILED" 1L (* Kept for backward compatibility; used in XenCenter *)
let host_clock_skew_detected = addMsg "HOST_CLOCK_SKEW_DETECTED" 3L
let host_clock_went_backwards = addMsg "HOST_CLOCK_WENT_BACKWARDS" 1L (* Unused in xen-api *)

let pool_master_transition = addMsg "POOL_MASTER_TRANSITION" 4L

let pbd_plug_failed_on_server_start = addMsg "PBD_PLUG_FAILED_ON_SERVER_START" 3L

let alarm = addMsg "ALARM" 1L (* Previously missing from table; unused in xen-api *)

let wlb_failed = addMsg "WLB_CONSULTATION_FAILED" 3L
let wlb_optimization_alert = addMsg "WLB_OPTIMIZATION_ALERT" 3L (* Used in XenCenter *)

let auth_external_init_failed = addMsg "EXTAUTH_INIT_IN_HOST_FAILED" 2L
let auth_external_pool_non_homogeneous = addMsg "EXTAUTH_IN_POOL_IS_NON_HOMOGENEOUS" 2L

let multipath_periodic_alert = addMsg "MULTIPATH_PERIODIC_ALERT" 3L

let v6_server_up = addMsg "LICENSE_SERVER_CONNECTED" 4L (* Used in XenCenter *)
let v6_server_down = addMsg "LICENSE_SERVER_UNAVAILABLE" 3L (* Used in XenCenter *)
let v6_license_expired = addMsg "LICENSE_EXPIRED" 2L (* Used in XenCenter *)
let v6_grace_license = addMsg "GRACE_LICENSE" 3L
let v6_rejected = addMsg "LICENSE_NOT_AVAILABLE" 2L
let v6_comm_error = addMsg "LICENSE_SERVER_UNREACHABLE" 2L

(* VMPP message types *)
let vmpp_snapshot_lock_failed = addMsg "VMPP_SNAPSHOT_LOCK_FAILED" 3L (*'The snapshot phase is already executing for this protection policy. Please try again later'*)
let vmpp_snapshot_succeeded = addMsg "VMPP_SNAPSHOT_SUCCEEDED" 5L (*'Successfully performed the snapshot phase of the protection policy'*)
let vmpp_snapshot_failed = addMsg "VMPP_SNAPSHOT_FAILED" 3L (*'The snapshot phase of the protection policy failed.'*)
let vmpp_archive_lock_failed = addMsg "VMPP_ARCHIVE_LOCK_FAILED" 3L (*'The archive sub-policy is already executing for some protection policy in the pool.Please try again later'*)
let vmpp_archive_failed_0 = addMsg "VMPP_ARCHIVE_FAILED_0" 3L (*'The archive phase failed for this protection policy'*)
let vmpp_archive_suceeded = addMsg "VMPP_ARCHIVE_SUCCEEDED" 5L (*'Successfully performed the archive phase of the protection policy'*)
let vmpp_archive_target_mount_failed = addMsg "VMPP_ARCHIVE_TARGET_MOUNT_FAILED" 3L (*'Failed to mount the archive target. Please check the archive target configuration settings'*)
let vmpp_archive_target_unmount_failed = addMsg "VMPP_ARCHIVE_TARGET_UNMOUNT_FAILED" 3L (*'Failed to unmount the archive target. Please make sure than the local directory was mounted successfully and has no open handles'*)
let vmpp_license_error = addMsg "VMPP_LICENSE_ERROR" 3L (*'This operation is not allowed under your license.  Please contact your support representative'*)
let vmpp_xapi_logon_failure = addMsg "VMPP_XAPI_LOGON_FAILURE" 3L (*'Could not login to API session.'*)
let vmpp_snapshot_missed_event = addMsg "VMPP_SNAPSHOT_MISSED_EVENT" 3L (*'A scheduled snapshot event was missed due to another on-going scheduled snapshot run. This is unexpected behaviour, please re-configure your snapshot sub-policy',*)
let vmpp_archive_missed_event = addMsg "VMPP_ARCHIVE_MISSED_EVENT" 3L (*'A scheduled archive event was missed due to another on-going scheduled archive run. This is unexpected behaviour, please re-configure your archive sub-policy'*)
let vmpp_snapshot_archive_already_exists = addMsg "VMPP_SNAPSHOT_ARCHIVE_ALREADY_EXISTS" 3L (*'Failed to archive the snapshot, it has already been archived on the specified target'*)

let bond_status_changed = addMsg "BOND_STATUS_CHANGED" 3L (* A link in a bond went down or came back up *) (* Previously missing from table *)
