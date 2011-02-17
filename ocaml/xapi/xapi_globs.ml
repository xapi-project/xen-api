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
 
(** A central location for settings related to xapi *)
 
open Stringext
open Printf
open Util_globs_inventory

(* xapi process returns this code on exit when it wants to be restarted *)
let restart_return_code = 123

let pool_secret_path = "/etc/xensource/ptoken"
let pool_secret = ref ""

let localhost_ref : [`host] Ref.t ref = ref Ref.null

(* xapi version *)
let version_major = 1
let version_minor = 3
let xapi_user_agent = "xapi/"^(string_of_int version_major)^"."^(string_of_int version_minor)

(* api version *)
let api_version_major = 1L
let api_version_minor = 8L
let api_version_string =
  Printf.sprintf "%Ld.%Ld" api_version_major api_version_minor
let api_version_vendor = "XenSource"
let api_version_vendor_implementation = []

(* version of latest tools ISO in filesystem *)
let tools_version = ref (-1, -1, -1, -1)

(* client min/max version range *)
let xencenter_min_verstring = "1.8"
let xencenter_max_verstring = "1.8"

(* linux pack vsn key in host.software_version (used for a pool join restriction *)
let linux_pack_vsn_key = "xs:linux"
let packs_dir = "/etc/xensource/installed-repos"

let ssl_pid = ref 0

let default_cleartext_port = 80
let default_ssl_port = 443

let ips_to_listen_on = "0.0.0.0"

let http_port = ref default_cleartext_port
let https_port = ref default_ssl_port

let xapi_gc_debug = ref true

let unix_domain_socket = "/var/xapi/xapi"
let local_database = "/var/xapi/local.db"

(* amount of time to retry master_connection before (if restart_on_connection_timeout is set) restarting xapi; -ve means don't timeout: *)
let master_connect_retry_timeout = -1. (* never timeout *)

(* the time taken to wait before restarting in a different mode for pool eject/join operations *)
let fuse_time = 10

(* the time taken to wait before restarting after restoring db backup *)
let db_restore_fuse_time = 30

(* if a slave in emergency "cannot see master mode" then this flag is set *)
let slave_emergency_mode = ref false

(** Whenever in emergency mode we stash an error here so the user can determine what's wrong
    without trawling through logfiles *)
let emergency_mode_error = ref (Api_errors.Server_error(Api_errors.host_still_booting, []))

(* Interval between host heartbeats *)
let host_heartbeat_interval = 30.0
(* If we haven't heard a heartbeat from a host for this interval then the host is assumed dead *)
let host_assumed_dead_interval = 600.0 (* 10 minutes *)

let http_realm = "xapi"

(* Special XS entry looked for by the XenSource PV drivers (see xenagentd.hg:src/xad.c) *)
let xe_key = "/mh/XenSource-TM_XenEnterprise-TM"
let xe_val = "XenSource(TM) and XenEnterprise(TM) are registered trademarks of XenSource Inc."

let config_file = ref "/etc/xensource/xapi.conf"
let log_config_file = ref "/etc/xensource/log.conf"
let db_conf_path = "/etc/xensource/db.conf"
let remote_db_conf_fragment_path = "/etc/xensource/remote.db.conf"
let simulator_config_file = ref "/etc/XenServer-simulator.conf"
let pool_config_file = "/etc/xensource/pool.conf"
let cpu_info_file = "/etc/xensource/boot_time_cpus"
let initial_host_free_memory_file = "/etc/xensource/boot_time_memory"
let using_rrds = ref false

let ready_file = ref ""
let init_complete = ref ""

let hg_changeset = ref "unknown"

(* Keys used in both the software_version (string -> string map) and in the import/export code *)
let _hostname = "hostname"
let _date = "date"
let _product_version = "product_version"
let _product_version_text = "product_version_text"
let _product_version_text_short = "product_version_text_short"
let _product_brand = "product_brand"
let _build_number = "build_number"
let _hg_id = "hg_id"
let _api_major = "API_major"
let _api_minor = "API_minor"
let _api_vendor = "API_vendor"
let _api_vendor_implementation = "API_vendor_implementation"
let _xapi_major = "xapi_major"
let _xapi_minor = "xapi_minor"
let _export_vsn = "export_vsn"
let _dbv = "dbv"

(* Used to differentiate between 
   Rio beta2 (0) [no inline checksums, end-of-tar checksum table],
   Rio GA (1) [inline checksums, end-of-tar checksum table]
   and Miami GA (2) [inline checksums, no end-of-tar checksum table] *)
let export_vsn = 2

let software_version = [ _product_version, Version.product_version;
			_product_version_text,       Version.product_version_text;
			_product_version_text_short, Version.product_version_text_short;
			 _product_brand,   Version.product_brand;
			 _build_number,    Version.build_number;
			 _hg_id,           Version.hg_id;
			 _hostname,        Version.hostname;
			 _date,            Version.date]

let pygrub_path = "/usr/bin/pygrub"
let eliloader_path = "/usr/bin/eliloader"
let supported_bootloaders = [ "pygrub", pygrub_path;
			      "eliloader", eliloader_path ]

(* Deprecated: *)
let is_guest_installer_network = "is_guest_installer_network"

let is_host_internal_management_network = "is_host_internal_management_network"

let auto_scan = "auto-scan" (* if set in SR.other_config, scan the SR in the background *)
let auto_scan_interval = "auto-scan-interval" (* maybe set in Host.other_config *)

let cd_tray_ejector = "cd_tray_ejector"

let host_console_vncport = 5900 (* guaranteed by the startup scripts *)

let vhd_parent = "vhd-parent" (* set in VDIs backed by VHDs *)

let owner_key = "owner" (* set in VBD other-config to indicate that clients can delete the attached VDI on VM uninstall if they want.. *)

let using_vdi_locking_key = "using-vdi-locking" (* set in Pool other-config to indicate that we should use storage-level (eg VHD) locking *)

let mac_seed = "mac_seed" (* set in a VM to generate MACs by hash chaining *)

let ( ** ) = Int64.mul
let vm_minimum_memory = 16L ** 1024L ** 1024L (* don't start VMs with less than 16 Mb *)

let grant_api_access = "grant_api_access"

(* From Miami GA onward we identify the tools SR with the SR.other_config key: *)
let tools_sr_tag = "xenserver_tools_sr"

(* Rio and Miami beta1 and beta2 used the following name-labels: *)
let rio_tools_sr_name = "XenSource Tools"
let miami_tools_sr_name = "XenServer Tools"

let tools_sr_dir = "/opt/xensource/packages/iso"

let default_template_key = "default_template"
let linux_template_key = "linux_template"

let vbd_task_key = "task_id" (* set on dom0 block-attached VBDs to indicate the task they're associated with *)

(* Set to true on the P2V server template and the tools SR *)
let xensource_internal = "xensource_internal"

let logrot_max = ref (1024*16*1024)
(* CA-12242: use this script because otherwise logrotate has a habit of closing its own fds *)
(* logrotate is called without a stdin, and when it fork-and-execs gzip, it opens the src *)
(* getting fd 0, opens the dest getting fd 3, then forks, then dups 0 to 0, dups 3 to 1 and *)
(* then closes 0 and 3! *)
let logrot_cmd = "/opt/xensource/libexec/logrotate.sh" 
let logrot_arg = [ ]

(* Error codes for internal storage backends -- these have counterparts in sm.hg/drivers/XE_SR_ERRORCODES.xml *)
let sm_error_ISODconfMissingLocation = 1000
let sm_error_ISOMustHaveISOExtension = 1001
let sm_error_ISOMountFailure = 1002
let sm_error_ISOUnmountFailure = 1002
let sm_error_generic_VDI_create_failure = 78
let sm_error_generic_VDI_delete_failure = 80

(* temporary restore path for db *)
let db_temporary_restore_path = "/var/xapi/restore_db.db"

(* temporary path for the HA metadata database *)
let ha_metadata_db = "/var/xapi/ha_metadata.db"

(* temporary path for the general metadata database *)
let gen_metadata_db = "/var/xapi/gen_metadata.db"

(* temporary path for opening a foreign metadata database *)
let foreign_metadata_db = "/var/xapi/foreign.db"

let migration_failure_test_key = "migration_wings_fall_off" (* set in other-config to simulate migration failures *)

(* A comma-separated list of extra xenstore paths to watch in the migration code during
   the disk flushing *)
let migration_extra_paths_key = "migration_extra_paths"

(* If a session has a last_active older than this we delete it *)
let inactive_session_timeout = 24. *. 60. *. 60. (* 24 hrs in seconds *) 

(* After this we start to delete completed tasks (never pending ones) *)
let max_tasks = 200

(* After this we start to invalidate older sessions *)
(* We must allow for more sessions than running tasks *)
let max_sessions = max_tasks * 2

let completed_task_timeout = 65. *. 60. (* 65 mins *)

let pending_task_timeout = 24. *. 60. *. 60. (* 24 hrs in seconds *) 

(* After this we start to delete alerts *)
let alert_timeout = completed_task_timeout +. 1.

(* Don't reboot a domain which crashes too quickly: *)
let minimum_time_between_bounces = 120. (* 2 minutes *)

(* If a domain is rebooted (from inside) in less than this time since it last started, then insert an artificial delay: *)
let minimum_time_between_reboot_with_no_added_delay = 60. (* 1 minute *)
(* the size of the artificial delay is: *)
let artificial_reboot_delay = 30.

(* The Unix.time that represents the maximum time in the future that a 32 bit time can cope with *)
let the_future = 2147483647.0

(* Indicates whether the master thinks the host is running in HA mode. The real data is stored
   on the slave so might be out of date. *)
let host_ha_armed = "ha_armed"

(* The set of hosts this host believes it can see *)
let host_ha_membership_set = "ha_membership_set"

(* Indicates whether over-committing (via API) is allowed *)
let pool_ha_allow_overcommitting = "ha_allow_overcommitting"

(* the other-config key used (for now) to store VM.always_run *)
let vm_ha_always_run = "ha_always_run"

(* the other-config key used (for now) to store VM.restart_priority *)
let vm_ha_restart_priority = "ha_restart_priority"

(* the other-config key used (for now) to store Pool.ha_tolerated_host_failures *)
let pool_ha_num_host_failures = "ha_tolerated_host_failures"

(* the other-config key that reflects whether the pool is overprovisioned *)
let pool_ha_currently_over_provisioned = "ha_currently_over_provisioned"

let ha_monitor_timer = 20. (* seconds *)

let ha_monitor_startup_timeout = 30. *. 60. (* seconds *)

(* Unconditionally replan every once in a while just in case the overcommit protection is buggy and we don't notice *)
let ha_monitor_plan_timer = 30. *. 60. (* seconds *)

let backup_db = "/var/xapi/state-backup.db"

(* Place where database XML backups are kept *)
let backup_db_xml = "/var/xapi/state-backup.xml"

(* Time to wait before fencing in the case when this host isn't a master, isn't in 
   emergency mode and has no running VMs before fencing. This is intended to give
   the admin some time to fix a broken configuration.*)
let noncritical_fence_timeout = 5. *. 60. (* 5 minutes *)

(* Directory containing scripts which are executed when a node becomes master
   and when a node gives up the master role *)
let master_scripts_dir = "/etc/xensource/master.d"

(* Indicates whether we should allow clones of suspended VMs via VM.clone *)
let pool_allow_clone_suspended_vm = "allow_clone_suspended_vm"

(* Size of a VDI to store the shared database on *)
let shared_db_vdi_size = 134217728L (* 128 * 1024 * 1024 = 128 megs *)

(* Mount point for the shared DB *)
let shared_db_mount_point = "/var/xapi/shared_db"
let snapshot_db = "/var/xapi/snapshot.db"

(* Device for shared DB VBD *)
let shared_db_device = "15"
let shared_db_device_path = "/dev/xvdp"

(* Pool other-config key for shared_db_sr -- existence implies pool is using a shared db *)
let shared_db_pool_key = "shared_db_sr"

(* Names of storage parameters *)
let _sm_vm_hint = "vmhint"
let _sm_epoch_hint = "epochhint"

let i18n_key = "i18n-key"
let i18n_original_value_prefix = "i18n-original-value-"

(* Primitive access control mechanism: CA-12313 *)
let _sm_session = "_sm_session"

let snapshot_of = "snapshot_of"
let snapshot_time = "snapshot_time"

(* Mark objects created by an import for CA-11743 on their 'other-config' field *)
let import_task = "import_task"

(* other-config key names where we hack install-time and last-boot-time, to work around the fact that these are not persisted on metrics fields in 4.1
   - see CA-7582 *)
let _install_time_key = "install_time"
let _start_time_key = "start_time"

(* Sync switches *)
(* WARNING WARNING - take great care setting these - it could lead to xapi failing miserably! *)

let sync_switch_off = "nosync" (* Set the following keys to this value to disable the dbsync operation *)

(* dbsync_slave *)
let sync_local_vdi_activations = "sync_local_vdi_activations"
let sync_create_localhost = "sync_create_localhost"
let sync_enable_localhost = "sync_enable_localhost"
let sync_refresh_localhost_info = "sync_refresh_localhost_info"
let sync_record_host_memory_properties = "sync_record_host_memory_properties"
let sync_copy_license_to_db = "sync_copy_license_to_db"
let sync_create_host_cpu = "sync_create_host_cpu"
let sync_create_domain_zero = "sync_create_domain_zero"
let sync_crashdump_resynchronise = "sync_crashdump_resynchronise"
let sync_update_vms = "sync_update_vms"
let sync_remove_leaked_vbds = "sync_remove_leaked_vbds"
let sync_pif_params = "sync_pif_params"
let sync_patch_update_db = "sync_patch_update_db"
let sync_pbd_reset = "sync_pbd_reset"
let sync_bios_strings = "sync_bios_strings"
let sync_chipset_info = "sync_chipset_info"
let sync_pci_devices = "sync_pci_devices"
let sync_gpus = "sync_gpus"
let sync_fix_bonds = "sync_fix_bonds"

(* create_storage *)
let sync_create_pbds = "sync_create_pbds"

(* Set this key to "true" on pool.other_config to disable the network gcing thread *)
let gc_network_disable = "gc_network_disable"

(* Set on the Pool.other_config to signal that the pool is currently in a mixed-mode
   rolling upgrade state. *)
let rolling_upgrade_in_progress = "rolling_upgrade_in_progress"

(* Set on Pool.other_config to override the base HA timeout in a persistent fashion *)
let default_ha_timeout = "default_ha_timeout"

(* Executed during startup when the API/database is online but before storage or networks
   are fully initialised. *)
let startup_script_hook = "/opt/xensource/libexec/xapi-startup-script"

(* Executed when a rolling upgrade is detected starting or stopping *)
let rolling_upgrade_script_hook = "/opt/xensource/libexec/xapi-rolling-upgrade"

(* When set to true indicates that the host has still booted so we're initialising everything
   from scratch e.g. shared storage, sampling boot free mem etc *)
let on_system_boot = ref false

(* Default backlog supplied to Unix.listen *)
let listen_backlog = 128

(* Where the next artificial delay is stored in xenstore *)
let artificial_reboot_delay = "artificial-reboot-delay"

(* Xapi script hooks root *)
let xapi_hooks_root = "/etc/xapi.d/"

(* RRD storage location *)
let xapi_rrd_location = "/var/xapi/blobs/rrds"

let xapi_blob_location = "/var/xapi/blobs"

let last_blob_sync_time = "last_blob_sync_time"

(* Port on which to send network heartbeats *)
let xha_udp_port = 694 (* same as linux-ha *)

(* When a host is known to be shutting down or rebooting, we add it's reference in here.
   This can be used to force the Host_metrics.live flag to false. *)
let hosts_which_are_shutting_down : API.ref_host list ref = ref []
let hosts_which_are_shutting_down_m = Mutex.create ()

let xha_timeout = "timeout"

let http_limit_max_rpc_size = 300 * 1024 (* 300K *)
let http_limit_max_cli_size = 200 * 1024 (* 200K *)
let http_limit_max_rrd_size = 2 * 1024 * 1024 (* 2M -- FIXME : need to go below 1mb for security purpose. *)

let sync_timer = 3600.0 *. 24.0 (* sync once a day *)

let message_limit=10000

let xapi_message_script = "/opt/xensource/libexec/mail-alarm"

(* Emit a warning if more than this amount of clock skew detected *)
let max_clock_skew = 5. *. 60. (* 5 minutes *)

(* Optional directory containing XenAPI plugins *)
let xapi_plugins_root = "/etc/xapi.d/plugins"

let guest_liveness_timeout = 5.0 *. 60.0 

(** CA-18377: Providing lists of operations that were supported by the Miami release. *)
(** For now, we check against these lists when sending data across the wire that may  *)
(** be read by a Miami host, and remove any items that are not found on the lists.    *)

let host_operations_miami = [
	`evacuate;
	`provision;
]

let vm_operations_miami = [
	`assert_operation_valid;
	`changing_memory_live;
	`changing_shadow_memory_live;
	`changing_VCPUs_live;
	`clean_reboot;
	`clean_shutdown;
	`clone;
	`copy;
	`csvm;
	`destroy;
	`export;
	`get_boot_record;
	`hard_reboot;
	`hard_shutdown;
	`import;
	`make_into_template;
	`migrate;
	`pause;
	`pool_migrate;
	`power_state_reset;
	`provision;
	`resume;
	`resume_on;
	`send_sysrq;
	`send_trigger;
	`start;
	`start_on;
	`suspend;
	`unpause;
	`update_allowed_operations;
]

(* Viridian key name (goes in platform flags) *)
let viridian_key_name = "viridian"
(* Viridian key value (set in new templates, in built-in templates on upgrade and when Orlando PV drivers up-to-date first detected) *)
let default_viridian_key_value = "true"

(* machine-address-size key-name/value; goes in other-config of RHEL5.2 template *)
let machine_address_size_key_name = "machine-address-size"
let machine_address_size_key_value = "36"

(* Host.other_config key to indicate the absence of local storage *)
let host_no_local_storage = "no_local_storage"

(* Pool.other_config key to enable creation of min/max rras in new VM rrds *)
let create_min_max_in_new_VM_RRDs = "create_min_max_in_new_VM_RRDs"

(* Pool.other_config key to enable pass-through of PIF carrier *)
let pass_through_pif_carrier = "pass_through_pif_carrier"

let dev_zero = "/dev/zero"

let wlb_timeout = "wlb_timeout"
let wlb_reports_timeout = "wlb_reports_timeout"
let default_wlb_timeout = 30.0
let default_wlb_reports_timeout = 600.0

(** {2 Settings relating to dynamic memory control} *)

(** A pool-wide configuration key that specifies for HVM guests a lower bound
    for the ratio k, where (memory-dynamic-min >= k * memory-static-max) *)
let memory_ratio_hvm = ("memory-ratio-hvm", "0.25")

(** A pool-wide configuration key that specifies for PV guests a lower bound
    for the ratio k, where (memory-dynamic-min >= k * memory-static-max) *)
let memory_ratio_pv  = ("memory-ratio-pv", "0.25")

(** {2 Settings for the redo-log} *)

(** {3 Settings related to the connection to the block device I/O process} *)

(** The maximum allowed number of redo_log instances. *)
let redo_log_max_instances = 8

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while emptying *)
let redo_log_max_block_time_empty = 2.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while reading *)
let redo_log_max_block_time_read = 30.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while writing a delta *)
let redo_log_max_block_time_writedelta = 2.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while writing a database *)
let redo_log_max_block_time_writedb = 30.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while initially connecting to it *)
let redo_log_max_startup_time = 5.

(** The delay between each attempt to connect to the block device I/O process *)
let redo_log_connect_delay = 0.1

(** The prefix of the file used as a socket to communicate with the block device I/O process *)
let redo_log_comms_socket_stem = "sock-blkdev-io"

(** The maximum permitted number of block device I/O processes we are waiting to die after being killed *)
let redo_log_max_dying_processes = 2

(** {3 Settings related to the metadata VDI which hosts the redo log} *)

(** Reason associated with the static VDI attach, to help identify the metadata VDI later (HA) *)
let ha_metadata_vdi_reason = "HA metadata VDI"

(** Reason associated with the static VDI attach, to help identify the metadata VDI later (generic) *)
let gen_metadata_vdi_reason = "general metadata VDI"

(** Reason associated with the static VDI attach, to help identify the metadata VDI later (opening foreign databases) *)
let foreign_metadata_vdi_reason = "foreign metadata VDI"

(** The length, in bytes, of one redo log which constitutes half of the VDI *)
let redo_log_length_of_half = 60 * 1024 * 1024

(** {3 Settings related to the exponential back-off of repeated attempts to reconnect after failure} *)

(** The initial backoff delay, in seconds *)
let redo_log_initial_backoff_delay = 2

(** The factor by which the backoff delay is multiplied with each successive failure *)
let redo_log_exponentiation_base = 2

(** The maximum permitted backoff delay, in seconds *)
let redo_log_maximum_backoff_delay = 120

(** Pool.other_config key which, when set to the value "true", enables generation of METADATA_LUN_{HEALTHY_BROKEN} alerts *)
let redo_log_alert_key = "metadata_lun_alerts"

(** Mutex for the external authentication in pool *)
(* CP-825: Serialize execution of pool-enable-extauth and pool-disable-extauth *)
let serialize_pool_enable_disable_extauth = Mutex.create()
(* CP-695: controls our asynchronous persistent initialization of the external authentication service during Xapi.server_init *)

let event_hook_auth_on_xapi_initialize_succeeded = ref false

(** Directory used by the v6 license policy engine for caching *)
let upgrade_grace_file = "/var/xapi/ugp"


(** Time after which we conclude that a VM really is unco-operative *)
let cooperative_timeout = 30.

(** Where the ballooning daemon writes the initial overhead value *)
let squeezed_reserved_host_memory = "/squeezed/reserved-host-memory"

(** Xenclient enabled *)
let xenclient_enabled = false

(** {2 BIOS strings} *)

(** Type 11 strings that are always included *)
let standard_type11_strings =
	["oem-1", "Xen";
	 "oem-2", "MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d"]

(** Generic BIOS strings *)	 
let generic_bios_strings =
	["bios-vendor", "Xen";
	 "bios-version", "";
	 "system-manufacturer", "Xen";
	 "system-product-name", "HVM domU";
	 "system-version", "";
	 "system-serial-number", "";
	 "hp-rombios", ""] @ standard_type11_strings

(** BIOS strings of the old (XS 5.5) Dell Edition *)
let old_dell_bios_strings =
	["bios-vendor", "Dell Inc.";
	 "bios-version", "1.9.9";
	 "system-manufacturer", "Dell Inc.";
	 "system-product-name", "PowerEdge";
	 "system-version", "";
	 "system-serial-number", "3.3.1";
	 "oem-1", "Dell System";
	 "oem-2", "5[0000]";
	 "oem-3", "MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d";
	 "hp-rombios", ""]
	 
(** BIOS strings of the old (XS 5.5) HP Edition *)
let old_hp_bios_strings =
	["bios-vendor", "Xen";
	 "bios-version", "3.3.1";
	 "system-manufacturer", "HP";
	 "system-product-name", "ProLiant Virtual Platform";
	 "system-version", "3.3.1";
	 "system-serial-number", "e30aecc3-e587-5a95-9537-7c306759bced";
	 "oem-1", "Xen";
	 "oem-2", "MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d";
	 "hp-rombios", "COMPAQ"]


let permanent_master_failure_retry_timeout = 1. *. 60. (* 1 minute *)

(** {2 CPUID feature masking} *)

(** Pool.other_config key to hold the user-defined feature mask, used to
 *  override the feature equality checks at a Pool.join. *)
let cpuid_feature_mask_key = "cpuid_feature_mask"

(** Default feature mask: EST (base_ecx.7) is ignored. *)
let cpuid_default_feature_mask = "ffffff7f-ffffffff-ffffffff-ffffffff"

(** Path to trigger file for Network Reset. *)
let network_reset_trigger = "/tmp/network-reset"

let first_boot_dir = "/etc/firstboot.d/"

