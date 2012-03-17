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

module D = Debug.Debugger(struct let name="xapi_globs" end)

(* set this to true to use the experimental codepath *)
let use_xenopsd = ref false

(* xapi process returns this code on exit when it wants to be restarted *)
let restart_return_code = 123

let pool_secret_path = Filename.concat Fhs.etcdir "ptoken"
let pool_secret = ref ""

let localhost_ref : [`host] Ref.t ref = ref Ref.null

(* xapi version *)
let version_major = Version.xapi_version_major
let version_minor = Version.xapi_version_minor
let xapi_user_agent = "xapi/"^(string_of_int version_major)^"."^(string_of_int version_minor)

(* api version *)
let api_version_major = 1L
let api_version_minor = 10L
let api_version_string =
  Printf.sprintf "%Ld.%Ld" api_version_major api_version_minor
let api_version_vendor = "XenSource"
let api_version_vendor_implementation = []

(* version of latest tools ISO in filesystem *)
let tools_version_none = (-1, -1, -1, -1)

let tools_version = ref tools_version_none

(* client min/max version range *)
let xencenter_min_verstring = "1.10"
let xencenter_max_verstring = "1.10"

(* linux pack vsn key in host.software_version (used for a pool join restriction *)
let linux_pack_vsn_key = "xs:linux"
let packs_dir = Filename.concat Fhs.etcdir "installed-repos"

let ssl_pid = ref 0

let default_cleartext_port = 80
let default_ssl_port = 443

let ips_to_listen_on = "0.0.0.0"

let http_port = default_cleartext_port
let https_port = ref default_ssl_port

let xapi_gc_debug = ref true

let unix_domain_socket = Filename.concat Fhs.vardir "xapi"
let local_storage_unix_domain_socket = Filename.concat Fhs.vardir "storage-local"
let storage_unix_domain_socket = Filename.concat Fhs.vardir "storage"
let local_database = Filename.concat Fhs.vardir "local.db"


(* if a slave in emergency "cannot see master mode" then this flag is set *)
let slave_emergency_mode = ref false

(** Whenever in emergency mode we stash an error here so the user can determine what's wrong
    without trawling through logfiles *)
let emergency_mode_error = ref (Api_errors.Server_error(Api_errors.host_still_booting, []))

let http_realm = "xapi"

(* Special XS entry looked for by the XenSource PV drivers (see xenagentd.hg:src/xad.c) *)
let xe_key = "/mh/XenSource-TM_XenEnterprise-TM"
let xe_val = "XenSource(TM) and XenEnterprise(TM) are registered trademarks of XenSource Inc."

(* Base path and some of its immediate dependencies. *)
let xe_path = Filename.concat Fhs.bindir "xe"
let sm_dir = Filename.concat Fhs.optdir "sm"

let config_file = ref Fhs.xapiconf
let log_config_file = ref (Filename.concat Fhs.etcdir "log.conf")
let db_conf_path = Filename.concat Fhs.etcdir "db.conf"
let remote_db_conf_fragment_path = Filename.concat Fhs.etcdir "remote.db.conf"
let simulator_config_file = ref "/etc/XenServer-simulator.conf"
let pool_config_file = Filename.concat Fhs.etcdir "pool.conf"
let cpu_info_file = Filename.concat Fhs.etcdir "boot_time_cpus"
let initial_host_free_memory_file = "/var/run/xapi/boot_time_memory"
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
let _platform_name = "platform_name"
let _platform_version = "platform_version"
let _product_brand = "product_brand"
let _build_number = "build_number"
let _git_id = "git_id"
let _api_major = "API_major"
let _api_minor = "API_minor"
let _api_vendor = "API_vendor"
let _api_vendor_implementation = "API_vendor_implementation"
let _xapi_major = "xapi_major"
let _xapi_minor = "xapi_minor"
let _export_vsn = "export_vsn"
let _dbv = "dbv"

(* When comparing two host versions, always treat a host that has platform_version defined as newer
 * than any host that does not have platform_version defined.
 * Substituting this default when a host does not have platform_version defined will be acceptable,
 * as long as a host never has to distinguish between two hosts of different versions which are both
 * older than itself. *)
let default_platform_version = "0.0.0"

(* Used to differentiate between 
   Rio beta2 (0) [no inline checksums, end-of-tar checksum table],
   Rio GA (1) [inline checksums, end-of-tar checksum table]
   and Miami GA (2) [inline checksums, no end-of-tar checksum table] *)
let export_vsn = 2

let software_version =
	(* In the case of XCP, all product_* fields will be blank. *)
	List.filter (fun (_, value) -> value <> "")
		[_product_version, Version.product_version;
			_product_version_text,       Version.product_version_text;
			_product_version_text_short, Version.product_version_text_short;
			_platform_name, Version.platform_name;
			_platform_version, Version.platform_version;
			 _product_brand,   Version.product_brand;
			 _build_number,    Version.build_number;
			 _git_id,           Version.git_id;
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

let tools_sr_dir = Filename.concat Fhs.sharedir "packages/iso"

let default_template_key = "default_template"
let linux_template_key = "linux_template"

(* Keys to explain the presence of dom0 block-attached VBDs: *)
let vbd_task_key = "task_id"
let related_to_key = "related_to"

(* Set to true on the P2V server template and the tools SR *)
let xensource_internal = "xensource_internal"

(* Error codes for internal storage backends -- these have counterparts in sm.hg/drivers/XE_SR_ERRORCODES.xml *)
let sm_error_ISODconfMissingLocation = 1000
let sm_error_ISOMustHaveISOExtension = 1001
let sm_error_ISOMountFailure = 1002
let sm_error_ISOUnmountFailure = 1002
let sm_error_generic_VDI_create_failure = 78
let sm_error_generic_VDI_delete_failure = 80

(* temporary restore path for db *)
let db_temporary_restore_path = Filename.concat Fhs.vardir "restore_db.db"

(* temporary path for the HA metadata database *)
let ha_metadata_db = Filename.concat Fhs.vardir "ha_metadata.db"

(* temporary path for the general metadata database *)
let gen_metadata_db = Filename.concat Fhs.vardir "gen_metadata.db"

(* temporary path for opening a foreign metadata database *)
let foreign_metadata_db = Filename.concat Fhs.vardir "foreign.db"

let migration_failure_test_key = "migration_wings_fall_off" (* set in other-config to simulate migration failures *)

(* A comma-separated list of extra xenstore paths to watch in the migration code during
   the disk flushing *)
let migration_extra_paths_key = "migration_extra_paths"

(* After this we start to delete completed tasks (never pending ones) *)
let max_tasks = 200

(* After this we start to invalidate older sessions *)
(* We must allow for more sessions than running tasks *)
let max_sessions = max_tasks * 2

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

let backup_db = Filename.concat Fhs.vardir "state-backup.db"

(* Place where database XML backups are kept *)
let backup_db_xml = Filename.concat Fhs.vardir "state-backup.xml"

(* Directory containing scripts which are executed when a node becomes master
   and when a node gives up the master role *)
let master_scripts_dir = Filename.concat Fhs.etcdir "master.d"

(* Indicates whether we should allow clones of suspended VMs via VM.clone *)
let pool_allow_clone_suspended_vm = "allow_clone_suspended_vm"

(* Size of a VDI to store the shared database on *)
let shared_db_vdi_size = 134217728L (* 128 * 1024 * 1024 = 128 megs *)

(* Mount point for the shared DB *)
let shared_db_mount_point = Filename.concat Fhs.vardir "shared_db"
let snapshot_db = Filename.concat Fhs.vardir "snapshot.db"

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
let sync_pbds = "sync_pbds"
let sync_update_vms = "sync_update_vms"
let sync_remove_leaked_vbds = "sync_remove_leaked_vbds"
let sync_pif_params = "sync_pif_params"
let sync_patch_update_db = "sync_patch_update_db"
let sync_pbd_reset = "sync_pbd_reset"
let sync_bios_strings = "sync_bios_strings"
let sync_chipset_info = "sync_chipset_info"
let sync_pci_devices = "sync_pci_devices"
let sync_gpus = "sync_gpus"

(* create_storage *)
let sync_create_pbds = "sync_create_pbds"

(* sync VLANs on slave with master *)
let sync_vlans = "sync_vlans"

(* Set on the Pool.other_config to signal that the pool is currently in a mixed-mode
   rolling upgrade state. *)
let rolling_upgrade_in_progress = "rolling_upgrade_in_progress"

(* Set on Pool.other_config to override the base HA timeout in a persistent fashion *)
let default_ha_timeout = "default_ha_timeout"

(* Executed during startup when the API/database is online but before storage or networks
   are fully initialised. *)
let startup_script_hook = Filename.concat Fhs.libexecdir "xapi-startup-script"

(* Executed when a rolling upgrade is detected starting or stopping *)
let rolling_upgrade_script_hook = Filename.concat Fhs.libexecdir "xapi-rolling-upgrade"

(* When set to true indicates that the host has still booted so we're initialising everything
   from scratch e.g. shared storage, sampling boot free mem etc *)
let on_system_boot = ref false

(* Default backlog supplied to Unix.listen *)
let listen_backlog = 128

(* Where the next artificial delay is stored in xenstore *)
let artificial_reboot_delay = "artificial-reboot-delay"

(* Xapi script hooks root *)
let xapi_hooks_root = Fhs.hooksdir 

(* RRD storage location *)
let xapi_rrd_location = Filename.concat Fhs.vardir "blobs/rrds"

let xapi_blob_location = Filename.concat Fhs.vardir "blobs"

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

let message_limit=10000

let xapi_message_script = Filename.concat Fhs.libexecdir "mail-alarm"

(* Emit a warning if more than this amount of clock skew detected *)
let max_clock_skew = 5. *. 60. (* 5 minutes *)

(* Optional directory containing XenAPI plugins *)
let xapi_plugins_root = Fhs.plugindir 



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
let upgrade_grace_file = Filename.concat Fhs.vardir "ugp"

(** Where the ballooning daemon writes the initial overhead value *)
let squeezed_reserved_host_memory = "/squeezed/reserved-host-memory"

(** Where the ballooning daemon writes the initial overhead value *)
let squeezed_reserved_host_memory_filename = "/var/run/squeezed/reserved-host-memory"

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

(** {2 CPUID feature masking} *)

(** Pool.other_config key to hold the user-defined feature mask, used to
 *  override the feature equality checks at a Pool.join. *)
let cpuid_feature_mask_key = "cpuid_feature_mask"

(** Default feature mask: EST (base_ecx.7) is ignored. *)
let cpuid_default_feature_mask = "ffffff7f-ffffffff-ffffffff-ffffffff"

(** Path to trigger file for Network Reset. *)
let network_reset_trigger = "/tmp/network-reset"

let first_boot_dir = "/etc/firstboot.d/"


(** Dynamic configurations to be read whenever xapi (re)start *)

let master_connection_reset_timeout = ref 120.

(* amount of time to retry master_connection before (if
   restart_on_connection_timeout is set) restarting xapi; -ve means don't
   timeout: *)
let master_connection_retry_timeout = ref (-1.)

let master_connection_default_timeout = ref 10.

let qemu_dm_ready_timeout = ref 300.

(* seconds per balancing check *)
let squeezed_balance_check_interval = ref 10.

(* Time we allow for the hotplug scripts to run before we assume something bad
   has happened and abort *)
let hotplug_timeout = ref 300.

let pif_reconfigure_ip_timeout = ref 300.

(* CA-16878: 5 minutes, same as the local database flush *)
let pool_db_sync_interval = ref 300.
(* blob/message/rrd file syncing - sync once a day *)
let pool_data_sync_interval = ref 86400.

let domain_shutdown_ack_timeout = ref 10.
let domain_shutdown_total_timeout = ref 720.

(* The actual reboot delay will be a random value between base and base + extra *)
let emergency_reboot_delay_base = ref 60.
let emergency_reboot_delay_extra = ref 120.

let ha_xapi_healthcheck_interval = ref 60
let ha_xapi_healthcheck_timeout = ref 120 (* > the number of attempts in xapi-health-check script *)
let ha_xapi_restart_attempts = ref 1
let ha_xapi_restart_timeout = ref 300 (* 180s is max start delay and 60s max shutdown delay in the initscript *)

(* Logrotate - poll the amount of data written out by the logger, and call
   logrotate when it exceeds the threshold *)
let logrotate_check_interval = ref 300.

let rrd_backup_interval = ref 86400.

(* CP-703: Periodic revalidation of externally-authenticated sessions *)
let session_revalidation_interval = ref 300. (* every 5 minutes *)

(* CP-820: other-config field in subjects should be periodically refreshed *)
let update_all_subjects_interval = ref 900. (* every 15 minutes *)

(* The default upper bound on the length of time to wait for a running VM to
   reach its current memory target. *)
let wait_memory_target_timeout = ref 256.

let snapshot_with_quiesce_timeout = ref 600.

(* Interval between host heartbeats *)
let host_heartbeat_interval = ref 30.

(* If we haven't heard a heartbeat from a host for this interval then the host is assumed dead *)
let host_assumed_dead_interval = ref 600.0

(* the time taken to wait before restarting in a different mode for pool eject/join operations *)
let fuse_time = ref 10.

(* the time taken to wait before restarting after restoring db backup *)
let db_restore_fuse_time = ref 30.

(* If a session has a last_active older than this we delete it *)
let inactive_session_timeout = ref 86400. (* 24 hrs in seconds *) 

let pending_task_timeout = ref 86400. (* 24 hrs in seconds *)

let completed_task_timeout = ref 3900. (* 65 mins *)

(* Don't reboot a domain which crashes too quickly: *)
let minimum_time_between_bounces = ref 120. (* 2 minutes *)

(* If a domain is rebooted (from inside) in less than this time since it last
   started, then insert an artificial delay: *)
let minimum_time_between_reboot_with_no_added_delay = ref 60. (* 1 minute *)

let ha_monitor_interval = ref 20.
(* Unconditionally replan every once in a while just in case the overcommit
   protection is buggy and we don't notice *)
let ha_monitor_plan_interval = ref 1800.

let ha_monitor_startup_timeout = ref 1800.

let ha_default_timeout_base = ref 60.

let guest_liveness_timeout = ref 300.

let permanent_master_failure_retry_interval = ref 60.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while emptying *)
let redo_log_max_block_time_empty = ref 2.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while reading *)
let redo_log_max_block_time_read = ref 30.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while writing a delta *)
let redo_log_max_block_time_writedelta = ref 2.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while writing a database *)
let redo_log_max_block_time_writedb = ref 30.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while initially connecting to it *)
let redo_log_max_startup_time = ref 5.

(** The delay between each attempt to connect to the block device I/O process *)
let redo_log_connect_delay = ref 0.1

let xapi_globs_spec =
	[ "master_connection_reset_timeout",
	  Config.Set_float master_connection_reset_timeout;
	  "master_connection_retry_timeout",
	  Config.Set_float master_connection_retry_timeout;
	  "master_connection_default_timeout",
	  Config.Set_float master_connection_default_timeout;
	  "qemu_dm_ready_timeout",
	  Config.Set_float qemu_dm_ready_timeout;
	  "squeezed_balance_check_interval",
	  Config.Set_float squeezed_balance_check_interval;
	  "hotplug_timeout",
	  Config.Set_float hotplug_timeout;
	  "pif_reconfigure_ip_timeout",
	  Config.Set_float pif_reconfigure_ip_timeout;
	  "pool_db_sync_interval",
	  Config.Set_float pool_db_sync_interval;
	  "pool_data_sync_interval",
	  Config.Set_float pool_data_sync_interval;
	  "domain_shutdown_ack_timeout",
	  Config.Set_float domain_shutdown_ack_timeout;
	  "domain_shutdown_total_timeout",
	  Config.Set_float domain_shutdown_total_timeout;
	  "emergency_reboot_delay_base",
	  Config.Set_float emergency_reboot_delay_base;
	  "emergency_reboot_delay_extra",
	  Config.Set_float emergency_reboot_delay_extra;
	  "ha_xapi_healthcheck_interval",
	  Config.Set_int ha_xapi_healthcheck_interval;
	  "ha_xapi_healthcheck_timeout",
	  Config.Set_int ha_xapi_healthcheck_timeout;
	  "ha_xapi_restart_attempts",
	  Config.Set_int ha_xapi_restart_attempts;
	  "ha_xapi_restart_timeout",
	  Config.Set_int ha_xapi_restart_timeout;
	  "logrotate_check_interval",
	  Config.Set_float logrotate_check_interval;
	  "rrd_backup_interval",
	  Config.Set_float rrd_backup_interval;
	  "session_revalidation_interval",
	  Config.Set_float session_revalidation_interval;
	  "update_all_subjects_interval",
	  Config.Set_float update_all_subjects_interval;
	  "wait_memory_target_timeout",
	  Config.Set_float wait_memory_target_timeout;
	  "snapshot_with_quiesce_timeout",
	  Config.Set_float snapshot_with_quiesce_timeout;
	  "host_heartbeat_interval",
	  Config.Set_float host_heartbeat_interval;
	  "host_assumed_dead_interval",
	  Config.Set_float host_assumed_dead_interval;
	  "fuse_time",
	  Config.Set_float fuse_time;
	  "db_restore_fuse_time",
	  Config.Set_float db_restore_fuse_time;
	  "inactive_session_timeout",
	  Config.Set_float inactive_session_timeout;
	  "pending_task_timeout",
	  Config.Set_float pending_task_timeout;
	  "completed_task_timeout",
	  Config.Set_float completed_task_timeout;
	  "minimum_time_between_bounces",
	  Config.Set_float minimum_time_between_bounces;
	  "minimum_time_between_reboot_with_no_added_delay",
	  Config.Set_float minimum_time_between_reboot_with_no_added_delay;
	  "ha_monitor_interval",
	  Config.Set_float ha_monitor_interval;
	  "ha_monitor_plan_interval",
	  Config.Set_float ha_monitor_plan_interval;
	  "ha_monitor_startup_timeout",
	  Config.Set_float ha_monitor_startup_timeout;
	  "ha_default_timeout_base",
	  Config.Set_float ha_default_timeout_base;
	  "guest_liveness_timeout",
	  Config.Set_float guest_liveness_timeout;
	  "permanent_master_failure_retry_interval",
	  Config.Set_float permanent_master_failure_retry_interval;
	  "redo_log_max_block_time_empty",
	  Config.Set_float redo_log_max_block_time_empty;
	  "redo_log_max_block_time_read",
	  Config.Set_float redo_log_max_block_time_read;
	  "redo_log_max_block_time_writedelta",
	  Config.Set_float redo_log_max_block_time_writedelta;
	  "redo_log_max_block_time_writedb",
	  Config.Set_float redo_log_max_block_time_writedb;
	  "redo_log_max_startup_time",
	  Config.Set_float redo_log_max_startup_time;
	  "redo_log_connect_delay",
	  Config.Set_float redo_log_connect_delay;
	]

let xapi_globs_conf = "/etc/xensource/xapi_globs.conf"

let read_external_config () =
	let unknown_key k v = D.warn "Unknown key/value pairs: (%s, %s)" k v in
	if Sys.file_exists xapi_globs_conf then begin
		(* Will raise exception if xapi_globs.conf is mis-formatted. It's up to the
		   caller to inspect and handle the failure.
		*)
		Config.read xapi_globs_conf xapi_globs_spec unknown_key;
		D.info "Read global variables successfully from %s" xapi_globs_conf
	end

