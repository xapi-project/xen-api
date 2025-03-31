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
(* constants which are global across all the tools *)

let services_uri = "/services"

(* ocaml/xapi/xapi_services.ml *)

let xenops_uri = "/services/xenops"

(* ocaml/xapi/xapi_services.ml *)

let sm_uri = "/services/SM"

(* ocaml/xapi/xapi_services.ml *)

let import_vdi_uri = "/import_vdi"

(* Currently unused *)

let import_raw_vdi_uri = "/import_raw_vdi" (* ocaml/xapi/import_raw_vdi.ml *)

let export_raw_vdi_uri = "/export_raw_vdi" (* ocaml/xapi/export_raw_vdi.ml *)

let export_uri = "/export"

(* ocaml/xapi/export.ml *)

let export_metadata_uri = "/export_metadata"

let import_uri = "/import"

(* ocaml/xapi/import.ml *)

let import_metadata_uri = "/import_metadata"

(* ocaml/xapi/xapi_vm_migrate.ml *)

let console_uri = "/console"

(* ocaml/xapi/console.ml *)

let host_backup_uri = "/host_backup" (* ocaml/xapi/xapi_host_backup.ml *)

let host_restore_uri = "/host_restore" (* ocaml/xapi/xapi_host_backup.ml *)

let host_logs_download_uri = "/host_logs_download"

(* ocaml/xapi/xapi_logs_download.ml *)

let pool_patch_upload_uri = "/pool_patch_upload"

(* ocaml/xapi/xapi_pool_patch.ml *)

let config_sync_uri = "/sync_config_files" (* ocaml/xapi/config_file_sync.ml *)

let pool_xml_db_sync = "/pool/xmldbdump" (* ocaml/xapi/pool_db_backup.ml *)

let vncsnapshot_uri = "/vncsnapshot" (* ocaml/xapi/xapi_vncsnapshot.ml *)

let system_status_uri = "/system-status" (* ocaml/xapi/system_status.ml *)

let remote_db_access_uri = "/remote_db_access" (* ocaml/xapi/xapi.ml *)

let remote_db_access_uri_v2 = "/remote_db_access_v2" (* ocaml/xapi/xapi.ml *)

let remote_stats_uri = "/remote_stats" (* ocaml/xapi/xapi.ml *)

let json_uri = "/json"

(* ocaml/xapi/xapi.ml *)

let jsonrpc_uri = "/jsonrpc"

(* ocaml/xapi/xapi.ml *)

let cli_uri = "/cli"

(* ocaml/xapi/xapi_cli.ml *)

let get_vm_rrd = "vm_rrd"

(* ocaml/xapi/xapi.ml *)

let get_vm_rrd_uri = "/" ^ get_vm_rrd (* ocaml/xapi/xapi.ml *)

let get_host_rrd = "host_rrd"

(* ocaml/xapi/xapi.ml *)

let get_host_rrd_uri = "/" ^ get_host_rrd (* ocaml/xapi/xapi.ml *)

let get_sr_rrd = "sr_rrd"

(* ocaml/xapi/xapi.ml *)

let get_sr_rrd_uri = "/" ^ get_sr_rrd (* ocaml/xapi/xapi.ml *)

let get_rrd_updates = "rrd_updates"

(* ocaml/xapi/xapi.ml *)

let get_rrd_updates_uri = "/" ^ get_rrd_updates (* ocaml/xapi/xapi.ml *)

let put_rrd = "rrd"

(* ocaml/xapi/xapi.ml *)

let put_rrd_uri = "/" ^ put_rrd

(* ocaml/xapi/xapi.ml *)

let rrd_unarchive = "rrd_unarchive"

(* ocaml/xapi/rrdd_proxy.ml *)

let rrd_unarchive_uri = "/" ^ rrd_unarchive (* ocaml/xapi/rrdd_proxy.ml *)

let blob_uri = "/blob"

(* ocaml/xapi/xapi_blob.ml *)

let remotecmd_uri = "/remotecmd"

(* ocaml/xapi/xapi_remotecmd.ml *)

let message_put_uri = "/messages"

(* ocaml/xapi/xapi_message.ml *)

let wlb_report_uri = "/wlb_report"

(* ocaml/xapi/wlb_reports.ml *)

let wlb_diagnostics_uri = "/wlb_diagnostics" (* ocaml/xapi/wlb_reports.ml *)

let audit_log_uri = "/audit_log"

(* ocaml/xapi/audit.ml *)

let get_pool_update_download_uri = "/update/"

(* ocaml/xapi/xapi_pool_update.ml *)

let get_repository_uri = "/repository" (* ocaml/xapi/repository.ml *)

let get_enabled_repository_uri =
  "/repository/enabled" (* ocaml/xapi/repository.ml *)

let get_host_updates_uri = "/host_updates" (* ocaml/xapi/repository.ml *)

let get_updates_uri = "/updates" (* ocaml/xapi/repository.ml *)

let put_bundle_uri = "/bundle" (* ocaml/xapi/xapi_pool.ml *)

let default_usb_speed = -1.

let use_compression = "use_compression"

(* If VM.HVM_boot_policy is set to this then we boot using qemu-dm *)
let hvm_default_boot_policy = "BIOS order"

(* Key we expect to find in VM.HVM_boot_params if VM.HVM_boot_policy = BIOS_order.
   Value is the boot string we send to qemu-dm (eg cd, dc, dcn, etc) *)
let hvm_boot_params_order = "order"

let hvm_default_boot_order = "cd"

(* Key we put in VM.other_config when we upgrade a VM from Zurich/Geneva to Rio *)
let vm_upgrade_time = "upgraded at"

(* Keys in the local config database *)
let ha_armed = "ha.armed"

let ha_disable_failover_decisions = "ha.disable_failover_decisions"

let ha_restart = "restart"

let ha_restart_best_effort = "best-effort"

let ha_valid_restart_priorities = [ha_restart; ha_restart_best_effort; ""]

let ha_base_t = "ha_base_t"

let ballooning_enabled = "ballooning.enabled"

let redo_log_enabled = "redo_log.enabled"

(* Valid cluster stack values *)
let ha_cluster_stack = "ha_cluster_stack"

let default_smapiv3_cluster_stack = "corosync"

(* Note: default without clustering is in !Xapi_globs.default_cluster_stack *)
let supported_smapiv3_cluster_stacks = ["corosync"]

(* Set in the local db to cause us to emit an alert when we come up as a master after
   a transition or HA failover *)
let this_node_just_became_master = "this_node_just_became_master"

(* Environment variables used to communicate with HA reconfigure script *)
let ha_peers = "ha_peers"

(* Stores whether we've executed the master scripts *)
let master_scripts = "master_scripts"

(* This flag is set when we commit to rebooting or shutting down the host when HA is enabled.
   This will prevent anyone from re-enabling the host and starting VMs on it during shutdown. *)
let host_disabled_until_reboot = "host_disabled_until_reboot"

(* Set when shutting down and rebooting. If we come up and finds no new crashdump and HA is enabled,
   we assume the host was fenced. *)
let host_restarted_cleanly = "host_restarted_cleanly"

(* This flag is for setting the interval at which the metrics updates happen. The possible
   values are 0=never 1=every 5 seconds, 2=every 60 seconds *)
let rrd_update_interval = "rrd_update_interval"

(* Standard name of a XenAPI plugin which can power on remote hosts *)
let power_on_plugin = "power-on-host"

let power_on_fn = "main"

(* Key used in local storage to list local PBDs with should have currently_attached = true (on this boot) *)
let local_currently_attached_pbds = "currently_attached_pbds"

(* The unique static rbac ref for the pool-admin role in the roles table *)
let rbac_pool_admin_uuid = "0165f154-ba3e-034e-6b27-5d271af109ba"

let _services = "services"

let _SM = "SM"

let _driver = "driver"

let path xs = "/" ^ String.concat "/" xs

(* Used to identify remote VDIs that are mirrors. Stored in VDI.other_config *)
let storage_migrate_vdi_map_key = "maps_to"

(* Used to specify mapping of VIFs to networks on the remote machine. Stored in VIF.other_config *)
let storage_migrate_vif_map_key = "maps_to"

(* Abstract size value for tracking PGPU utilisation. *)
let pgpu_default_size = Int64.mul 1024L 1024L

(* Used to specify mapping of vGPUs to pGPU groups on the remote machine. Stored in VGPU.other_config *)
let storage_migrate_vgpu_map_key = "maps_to"

(* Corosync timeout default values *)
let default_token_timeout_s = 20.0

let default_token_timeout_coefficient_s = 1.0

(* Minimum threshold for token timeout parameters *)
let minimum_token_timeout_s = 1.0

let minimum_token_timeout_coefficient_s = 0.65

(* Maximum VHD Size *)
let max_vhd_size = Sizes.(2040L ** gib)

(* Limit the max CLI upload size to 32MiB *)
let max_cli_upload_bytes = 33554432

let owner_key = "owner"

(* set in VBD other-config to indicate that clients can delete the attached VDI on VM uninstall if they want.. *)

(* xapi-cli-server doesn't link xapi-globs *)
let use_event_next = ref true

(* the time taken to wait before restarting in a different mode for pool eject/join operations *)
let fuse_time = ref 10.

(* Names of storage parameters *)
let _sm_vm_hint = "vmhint"

let _sm_epoch_hint = "epochhint"

let _sm_initial_allocation = "initial_allocation"

let default_cleartext_port = 80

let default_ssl_port = 443

let http_port = default_cleartext_port

let https_port = ref default_ssl_port

(** Type 11 strings that are always included *)
let standard_type11_strings =
  [
    ("oem-1", "Xen")
  ; ("oem-2", "MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d")
  ]

(** Generic BIOS strings *)
let generic_bios_strings =
  [
    ("bios-vendor", "Xen")
  ; ("bios-version", "")
  ; ("system-manufacturer", "Xen")
  ; ("system-product-name", "HVM domU")
  ; ("system-version", "")
  ; ("system-serial-number", "")
  ; ("baseboard-manufacturer", "")
  ; ("baseboard-product-name", "")
  ; ("baseboard-version", "")
  ; ("baseboard-serial-number", "")
  ; ("baseboard-asset-tag", "")
  ; ("baseboard-location-in-chassis", "")
  ; ("enclosure-asset-tag", "")
  ; ("hp-rombios", "")
  ]
  @ standard_type11_strings

(* bios_string length is limited to 512 characters *)

(** {2 BIOS strings} *)
let bios_string_limit_size = 512

(** List of user-settable VM BIOS strings keys *)
let settable_vm_bios_string_keys =
  [
    "bios-vendor"
  ; "bios-version"
  ; "system-manufacturer"
  ; "system-product-name"
  ; "system-version"
  ; "system-serial-number"
  ; "baseboard-manufacturer"
  ; "baseboard-product-name"
  ; "baseboard-version"
  ; "baseboard-serial-number"
  ; "baseboard-asset-tag"
  ; "baseboard-location-in-chassis"
  ; "enclosure-asset-tag"
  ]

(** BIOS strings of the old (XS 5.5) Dell Edition *)
let old_dell_bios_strings =
  [
    ("bios-vendor", "Dell Inc.")
  ; ("bios-version", "1.9.9")
  ; ("system-manufacturer", "Dell Inc.")
  ; ("system-product-name", "PowerEdge")
  ; ("system-version", "")
  ; ("system-serial-number", "3.3.1")
  ; ("oem-1", "Dell System")
  ; ("oem-2", "5[0000]")
  ; ("oem-3", "MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d")
  ; ("hp-rombios", "")
  ]

(** BIOS strings of the old (XS 5.5) HP Edition *)
let old_hp_bios_strings =
  [
    ("bios-vendor", "Xen")
  ; ("bios-version", "3.3.1")
  ; ("system-manufacturer", "HP")
  ; ("system-product-name", "ProLiant Virtual Platform")
  ; ("system-version", "3.3.1")
  ; ("system-serial-number", "e30aecc3-e587-5a95-9537-7c306759bced")
  ; ("oem-1", "Xen")
  ; ("oem-2", "MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d")
  ; ("hp-rombios", "COMPAQ")
  ]

(* the time taken to wait before restarting after restoring db backup *)
let db_restore_fuse_time = ref 30.

(* Note the following constant has an equivalent in the db layer *)
let http_limit_max_rpc_size = 300 * 1024 (* 300K *)

let http_limit_max_cli_size = 200 * 1024 (* 200K *)

(* Path to the pool configuration file. *)
let pool_config_file = ref (Filename.concat "/etc/xensource" "pool.conf")

let gencert = ref "/opt/xensource/libexec/gencert"

let openssl_path = ref "/usr/bin/openssl"

let good_ciphersuites =
  String.concat ":"
    ["ECDHE-RSA-AES256-GCM-SHA384"; "ECDHE-RSA-AES128-GCM-SHA256"]

let verify_certificates_path = "/var/xapi/verify-certificates"

let python3_path = "/usr/bin/python3"

let observer_config_dir = "/etc/xensource/observer"

let observer_component_xapi = "xapi"

let observer_component_xenopsd = "xenopsd"

let observer_component_xapi_clusterd = "xapi-clusterd"

let observer_component_smapi = "smapi"

let observer_components_all =
  [
    observer_component_xapi
  ; observer_component_xenopsd
  ; observer_component_xapi_clusterd
  ; observer_component_smapi
  ]

let tgroups_enabled = ref false

let when_tgroups_enabled f = if !tgroups_enabled then f () else ()

let runtime_sched = ref false
