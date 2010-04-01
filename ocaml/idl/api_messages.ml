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

let vm_uncooperative = addMessage "VM_UNCOOPERATIVE"
let v6_server_up = addMessage "LICENSE_SERVER_CONNECTED"
let v6_server_down = addMessage "LICENSE_SERVER_UNAVAILABLE"
let v6_license_expired = addMessage "LICENSE_EXPIRED"
let v6_grace_license = addMessage "GRACE_LICENSE"
let v6_rejected = addMessage "LICENSE_NOT_AVAILABLE"
let v6_comm_error = addMessage "LICENSE_SERVER_UNREACHABLE"
