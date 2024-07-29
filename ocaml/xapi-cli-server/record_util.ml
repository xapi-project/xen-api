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
(* conversion utils *)

exception Record_failure of string

let record_failure fmt =
  Printf.ksprintf (fun msg -> raise (Record_failure msg)) fmt

let to_str = function Rpc.String x -> x | _ -> failwith "Invalid"

let certificate_type_to_string = function
  | `host ->
      "host"
  | `host_internal ->
      "host_internal"
  | `ca ->
      "ca"

let class_to_string cls =
  match cls with
  | `VM ->
      "VM"
  | `Host ->
      "Host"
  | `SR ->
      "SR"
  | `Pool ->
      "Pool"
  | `VMPP ->
      "VMPP"
  | `VMSS ->
      "VMSS"
  | `PVS_proxy ->
      "PVS_proxy"
  | `VDI ->
      "VDI"
  | `Certificate ->
      "Certificate"
  | _ ->
      "unknown"

let string_to_class str =
  match str with
  | "VM" ->
      `VM
  | "Host" ->
      `Host
  | "SR" ->
      `SR
  | "Pool" ->
      `Pool
  | "VMPP" ->
      `VMPP
  | "VMSS" ->
      `VMSS
  | "PVS_proxy" ->
      `PVS_proxy
  | "VDI" ->
      `VDI
  | "Certificate" ->
      `Certificate
  | _ ->
      failwith "Bad type"

let power_state_to_string state =
  match state with
  | `Halted ->
      "Halted"
  | `Paused ->
      "Paused"
  | `Running ->
      "Running"
  | `Suspended ->
      "Suspended"
  | `ShuttingDown ->
      "Shutting down"
  | `Migrating ->
      "Migrating"

let vm_operation_table =
  [
    (`assert_operation_valid, "assertoperationvalid")
  ; (`changing_dynamic_range, "changing_dynamic_range")
  ; (`changing_static_range, "changing_static_range")
  ; (`changing_shadow_memory, "changing_shadow_memory")
  ; (`clean_reboot, "clean_reboot")
  ; (`clean_shutdown, "clean_shutdown")
  ; (`clone, "clone")
  ; (`snapshot, "snapshot")
  ; (`checkpoint, "checkpoint")
  ; (`snapshot_with_quiesce, "snapshot_with_quiesce")
  ; (`copy, "copy")
  ; (`revert, "revert")
  ; (`reverting, "reverting")
  ; (`provision, "provision")
  ; (`destroy, "destroy")
  ; (`export, "export")
  ; (`metadata_export, "metadata_export")
  ; (`import, "import")
  ; (`get_boot_record, "get_boot_record")
  ; (`data_source_op, "data_sources_op")
  ; (`hard_reboot, "hard_reboot")
  ; (`hard_shutdown, "hard_shutdown")
  ; (`migrate_send, "migrate_send")
  ; (`pause, "pause")
  ; (`resume, "resume")
  ; (`resume_on, "resume_on")
  ; (`changing_VCPUs_live, "changing_VCPUs_live")
  ; (`changing_NVRAM, "changing_NVRAM")
  ; (`start, "start")
  ; (`start_on, "start_on")
  ; (`shutdown, "shutdown")
  ; (`suspend, "suspend")
  ; (`unpause, "unpause")
  ; (`update_allowed_operations, "update_allowed_operations")
  ; (`make_into_template, "make_into_template")
  ; (`send_sysrq, "send_sysrq")
  ; (`send_trigger, "send_trigger")
  ; (`changing_memory_live, "changing_memory_live")
  ; (`awaiting_memory_live, "awaiting_memory_live")
  ; (`changing_shadow_memory_live, "changing_shadow_memory_live")
  ; (`pool_migrate, "pool_migrate")
  ; (`power_state_reset, "power_state_reset")
  ; (`csvm, "csvm")
  ; (`call_plugin, "call_plugin")
  ; (`create_vtpm, "create_vtpm")
  ]

let vm_operation_to_string x =
  if not (List.mem_assoc x vm_operation_table) then
    "(unknown operation)"
  else
    List.assoc x vm_operation_table

let string_to_vm_operation x =
  let table = List.map (fun (a, b) -> (b, a)) vm_operation_table in
  if not (List.mem_assoc x table) then
    raise
      (Api_errors.Server_error
         (Api_errors.invalid_value, ["blocked_operation"; x])
      )
  else
    List.assoc x table

let vm_uefi_mode_of_string = function
  | "setup" ->
      `setup
  | "user" ->
      `user
  | s ->
      record_failure "Expected 'user','setup', got %s" s

let vm_secureboot_readiness_to_string = function
  | `not_supported ->
      "not_supported"
  | `disabled ->
      "disabled"
  | `first_boot ->
      "first_boot"
  | `ready ->
      "ready"
  | `ready_no_dbx ->
      "ready_no_dbx"
  | `setup_mode ->
      "setup_mode"
  | `certs_incomplete ->
      "certs_incomplete"

let pool_guest_secureboot_readiness_to_string = function
  | `ready ->
      "ready"
  | `ready_no_dbx ->
      "ready_no_dbx"
  | `not_ready ->
      "not_ready"

let pool_operation_to_string = function
  | `ha_enable ->
      "ha_enable"
  | `ha_disable ->
      "ha_disable"
  | `cluster_create ->
      "cluster_create"
  | `designate_new_master ->
      "designate_new_master"
  | `tls_verification_enable ->
      "tls_verification_enable"
  | `configure_repositories ->
      "configure_repositories"
  | `sync_updates ->
      "sync_updates"
  | `get_updates ->
      "get_updates"
  | `apply_updates ->
      "apply_updates"
  | `cert_refresh ->
      "cert_refresh"
  | `exchange_certificates_on_join ->
      "exchange_certificates_on_join"
  | `exchange_ca_certificates_on_join ->
      "exchange_ca_certificates_on_join"
  | `copy_primary_host_certs ->
      "copy_primary_host_certs"
  | `eject ->
      "eject"

let host_operation_to_string = function
  | `provision ->
      "provision"
  | `evacuate ->
      "evacuate"
  | `shutdown ->
      "shutdown"
  | `reboot ->
      "reboot"
  | `power_on ->
      "power_on"
  | `vm_start ->
      "VM.start"
  | `vm_resume ->
      "VM.resume"
  | `vm_migrate ->
      "VM.migrate"
  | `apply_updates ->
      "apply_updates"
  | `enable ->
      "enable"

let update_guidance_to_string = function
  | `reboot_host ->
      "reboot_host"
  | `reboot_host_on_livepatch_failure ->
      "reboot_host_on_livepatch_failure"
  | `reboot_host_on_kernel_livepatch_failure ->
      "reboot_host_on_kernel_livepatch_failure"
  | `reboot_host_on_xen_livepatch_failure ->
      "reboot_host_on_xen_livepatch_failure"
  | `restart_toolstack ->
      "restart_toolstack"
  | `restart_device_model ->
      "restart_device_model"
  | `restart_vm ->
      "restart_vm"

let latest_synced_updates_applied_state_to_string = function
  | `yes ->
      "yes"
  | `no ->
      "no"
  | `unknown ->
      "unknown"

let vdi_operation_to_string : API.vdi_operations -> string = function
  | `clone ->
      "clone"
  | `copy ->
      "copy"
  | `resize ->
      "resize"
  | `resize_online ->
      "resize_online"
  | `destroy ->
      "destroy"
  | `force_unlock ->
      "force_unlock"
  | `snapshot ->
      "snapshot"
  | `mirror ->
      "mirror"
  | `forget ->
      "forget"
  | `update ->
      "update"
  | `generate_config ->
      "generate_config"
  | `enable_cbt ->
      "enable_cbt"
  | `disable_cbt ->
      "disable_cbt"
  | `data_destroy ->
      "data_destroy"
  | `list_changed_blocks ->
      "list_changed_blocks"
  | `set_on_boot ->
      "set_on_boot"
  | `blocked ->
      "blocked"

let sr_operation_to_string : API.storage_operations -> string = function
  | `scan ->
      "scan"
  | `destroy ->
      "destroy"
  | `forget ->
      "forget"
  | `plug ->
      "plug"
  | `unplug ->
      "unplug"
  | `update ->
      "update"
  | `vdi_create ->
      "VDI.create"
  | `vdi_introduce ->
      "VDI.introduce"
  | `vdi_destroy ->
      "VDI.destroy"
  | `vdi_resize ->
      "VDI.resize"
  | `vdi_clone ->
      "VDI.clone"
  | `vdi_snapshot ->
      "VDI.snapshot"
  | `vdi_mirror ->
      "VDI.mirror"
  | `vdi_enable_cbt ->
      "VDI.enable_cbt"
  | `vdi_disable_cbt ->
      "VDI.disable_cbt"
  | `vdi_set_on_boot ->
      "VDI.set_on_boot"
  | `vdi_data_destroy ->
      "VDI.data_destroy"
  | `vdi_list_changed_blocks ->
      "VDI.list_changed_blocks"
  | `pbd_create ->
      "PBD.create"
  | `pbd_destroy ->
      "PBD.destroy"

let vbd_operation_to_string = function
  | `attach ->
      "attach"
  | `eject ->
      "eject"
  | `insert ->
      "insert"
  | `plug ->
      "plug"
  | `unplug ->
      "unplug"
  | `unplug_force ->
      "unplug_force"
  | `pause ->
      "pause"
  | `unpause ->
      "unpause"

let vif_operation_to_string = function
  | `attach ->
      "attach"
  | `plug ->
      "plug"
  | `unplug ->
      "unplug"
  | `unplug_force ->
      "unplug_force"

let vif_locking_mode_to_string = function
  | `network_default ->
      "network_default"
  | `locked ->
      "locked"
  | `unlocked ->
      "unlocked"
  | `disabled ->
      "disabled"

let string_to_vif_locking_mode = function
  | "network_default" ->
      `network_default
  | "locked" ->
      `locked
  | "unlocked" ->
      `unlocked
  | "disabled" ->
      `disabled
  | s ->
      record_failure
        "Expected 'network_default', 'locked', 'unlocked', 'disabled', got %s" s

let vmss_type_to_string = function
  | `snapshot ->
      "snapshot"
  | `checkpoint ->
      "checkpoint"
  | `snapshot_with_quiesce ->
      "snapshot_with_quiesce"

let string_to_vmss_type = function
  | "snapshot" ->
      `snapshot
  | "checkpoint" ->
      `checkpoint
  | "snapshot_with_quiesce" ->
      `snapshot_with_quiesce
  | s ->
      record_failure
        "Expected 'snapshot', 'checkpoint', 'snapshot_with_quiesce', got %s" s

let vmss_frequency_to_string = function
  | `hourly ->
      "hourly"
  | `daily ->
      "daily"
  | `weekly ->
      "weekly"

let string_to_vmss_frequency = function
  | "hourly" ->
      `hourly
  | "daily" ->
      `daily
  | "weekly" ->
      `weekly
  | s ->
      record_failure "Expected 'hourly', 'daily', 'weekly', got %s" s

let network_default_locking_mode_to_string = function
  | `unlocked ->
      "unlocked"
  | `disabled ->
      "disabled"

let string_to_network_default_locking_mode = function
  | "unlocked" ->
      `unlocked
  | "disabled" ->
      `disabled
  | s ->
      record_failure "Expected 'unlocked' or 'disabled', got %s" s

let network_purpose_to_string : API.network_purpose -> string = function
  | `nbd ->
      "nbd"
  | `insecure_nbd ->
      "insecure_nbd"

let string_to_network_purpose : string -> API.network_purpose = function
  | "nbd" ->
      `nbd
  | "insecure_nbd" ->
      `insecure_nbd
  | s ->
      record_failure "Expected a network purpose string; got %s" s

let vm_appliance_operation_to_string = function
  | `start ->
      "start"
  | `clean_shutdown ->
      "clean_shutdown"
  | `hard_shutdown ->
      "hard_shutdown"
  | `shutdown ->
      "shutdown"

let cpu_feature_to_string f =
  match f with
  | `FPU ->
      "FPU"
  | `VME ->
      "VME"
  | `DE ->
      "DE"
  | `PSE ->
      "PSE"
  | `TSC ->
      "TSC"
  | `MSR ->
      "MSR"
  | `PAE ->
      "PAE"
  | `MCE ->
      "MCE"
  | `CX8 ->
      "CX8"
  | `APIC ->
      "APIC"
  | `SEP ->
      "SEP"
  | `MTRR ->
      "MTRR"
  | `PGE ->
      "PGE"
  | `MCA ->
      "MCA"
  | `CMOV ->
      "CMOV"
  | `PAT ->
      "PAT"
  | `PSE36 ->
      "PSE36"
  | `PN ->
      "PN"
  | `CLFLSH ->
      "CLFLSH"
  | `DTES ->
      "DTES"
  | `ACPI ->
      "ACPI"
  | `MMX ->
      "MMX"
  | `FXSR ->
      "FXSR"
  | `XMM ->
      "XMM"
  | `XMM2 ->
      "XMM2"
  | `SELFSNOOP ->
      "SELFSNOOP"
  | `HT ->
      "HT"
  | `ACC ->
      "ACC"
  | `IA64 ->
      "IA64"
  | `SYSCALL ->
      "SYSCALL"
  | `MP ->
      "MP"
  | `NX ->
      "NX"
  | `MMXEXT ->
      "MMXEXT"
  | `LM ->
      "LM"
  | `THREEDNOWEXT ->
      "3DNOWEXT"
  | `THREEDNOW ->
      "3DNOW"
  | `RECOVERY ->
      "RECOVERY"
  | `LONGRUN ->
      "LONGRUN"
  | `LRTI ->
      "LRTI"
  | `CXMMX ->
      "CXMMX"
  | `K6MTRR ->
      "K6MTRR"
  | `CYRIXARR ->
      "CYRIXARR"
  | `CENTAURMCR ->
      "CENTAURMCR"
  | `K8 ->
      "K8"
  | `K7 ->
      "K7"
  | `P3 ->
      "P3"
  | `P4 ->
      "P4"
  | `CONSTANTTSC ->
      "CONSTANTTSC"
  | `FXSAVELEAK ->
      "FXSAVELEAK"
  | `XMM3 ->
      "XMM3"
  | `MWAIT ->
      "MWAIT"
  | `DSCPL ->
      "DSCPL"
  | `EST ->
      "EST"
  | `TM2 ->
      "TM2"
  | `CID ->
      "CID"
  | `CX16 ->
      "CX16"
  | `XTPR ->
      "XTPR"
  | `XSTORE ->
      "XSTORE"
  | `XSTOREEN ->
      "XSTOREEN"
  | `XCRYPT ->
      "XCRYPT"
  | `XCRYPTEN ->
      "XCRYPTEN"
  | `LAHFLM ->
      "LAHFLM"
  | `CMPLEGACY ->
      "CMPLEGACY"
  | `VMX ->
      "VMX"

let task_status_type_to_string s =
  match s with
  | `pending ->
      "pending"
  | `success ->
      "success"
  | `failure ->
      "failure"
  | `cancelling ->
      "cancelling"
  | `cancelled ->
      "cancelled"

let protocol_to_string = function
  | `vt100 ->
      "VT100"
  | `rfb ->
      "RFB"
  | `rdp ->
      "RDP"

let telemetry_frequency_to_string = function
  | `daily ->
      "daily"
  | `weekly ->
      "weekly"
  | `monthly ->
      "monthly"

let task_allowed_operations_to_string s =
  match s with `cancel -> "Cancel" | `destroy -> "Destroy"

let alert_level_to_string s =
  match s with `Info -> "info" | `Warn -> "warning" | `Error -> "error"

let on_normal_exit_to_string x =
  match x with `destroy -> "Destroy" | `restart -> "Restart"

let string_to_on_normal_exit s =
  match String.lowercase_ascii s with
  | "destroy" ->
      `destroy
  | "restart" ->
      `restart
  | _ ->
      record_failure "Expected 'destroy' or 'restart', got %s" s

let on_crash_behaviour_to_string x =
  match x with
  | `destroy ->
      "Destroy"
  | `coredump_and_destroy ->
      "Core dump and destroy"
  | `restart ->
      "Restart"
  | `coredump_and_restart ->
      "Core dump and restart"
  | `preserve ->
      "Preserve"
  | `rename_restart ->
      "Rename restart"

let string_to_on_crash_behaviour s =
  match String.lowercase_ascii s with
  | "destroy" ->
      `destroy
  | "coredump_and_destroy" ->
      `coredump_and_destroy
  | "restart" ->
      `restart
  | "coredump_and_restart" ->
      `coredump_and_restart
  | "preserve" ->
      `preserve
  | "rename_restart" ->
      `rename_restart
  | _ ->
      record_failure
        "Expected 'destroy', 'coredump_and_destroy', \
         'restart','coredump_and_restart', 'preserve' or 'rename_restart', got \
         %s"
        s

let on_softreboot_behaviour_to_string x =
  match x with
  | `destroy ->
      "Destroy"
  | `restart ->
      "Restart"
  | `preserve ->
      "Preserve"
  | `soft_reboot ->
      "Soft reboot"

let string_to_on_softreboot_behaviour s =
  match String.lowercase_ascii s with
  | "destroy" ->
      `destroy
  | "restart" ->
      `restart
  | "preserve" ->
      `preserve
  | "soft_reboot" ->
      `soft_reboot
  | _ ->
      record_failure
        "Expected 'destroy', 'coredump_and_destroy', 'restart', \
         'coredump_and_restart', 'preserve', 'soft_reboot' or \
         'rename_restart', got %s"
        s

let host_display_to_string h =
  match h with
  | `enabled ->
      "enabled"
  | `enable_on_reboot ->
      "enable_on_reboot"
  | `disabled ->
      "disabled"
  | `disable_on_reboot ->
      "disable_on_reboot"

let host_sched_gran_of_string s =
  match String.lowercase_ascii s with
  | "core" ->
      `core
  | "cpu" ->
      `cpu
  | "socket" ->
      `socket
  | _ ->
      record_failure "Expected 'core','cpu', 'socket', got %s" s

let host_sched_gran_to_string = function
  | `core ->
      "core"
  | `cpu ->
      "cpu"
  | `socket ->
      "socket"

let host_numa_affinity_policy_to_string = function
  | `any ->
      "any"
  | `best_effort ->
      "best_effort"
  | `default_policy ->
      "default_policy"

let host_numa_affinity_policy_of_string a =
  match String.lowercase_ascii a with
  | "any" ->
      `any
  | "best_effort" ->
      `best_effort
  | "default_policy" ->
      `default_policy
  | s ->
      record_failure "Expected 'any', 'best_effort' or 'default_policy', got %s"
        s

let pci_dom0_access_to_string x = host_display_to_string x

let string_to_vdi_onboot s =
  match String.lowercase_ascii s with
  | "persist" ->
      `persist
  | "reset" ->
      `reset
  | _ ->
      record_failure "Expected 'persist' or 'reset', got %s" s

let string_to_vbd_mode s =
  match String.lowercase_ascii s with
  | "ro" ->
      `RO
  | "rw" ->
      `RW
  | _ ->
      record_failure "Expected 'RO' or 'RW', got %s" s

let vbd_mode_to_string = function `RO -> "ro" | `RW -> "rw"

let string_to_vbd_type s =
  match String.lowercase_ascii s with
  | "cd" ->
      `CD
  | "disk" ->
      `Disk
  | "floppy" ->
      `Floppy
  | _ ->
      record_failure "Expected 'CD' or 'Disk', got %s" s

let power_to_string h =
  match h with
  | `Halted ->
      "halted"
  | `Paused ->
      "paused"
  | `Running ->
      "running"
  | `Suspended ->
      "suspended"
  | `ShuttingDown ->
      "shutting down"
  | `Migrating ->
      "migrating"

let vdi_type_to_string t =
  match t with
  | `system ->
      "System"
  | `user ->
      "User"
  | `ephemeral ->
      "Ephemeral"
  | `suspend ->
      "Suspend"
  | `crashdump ->
      "Crashdump"
  | `ha_statefile ->
      "HA statefile"
  | `metadata ->
      "Metadata"
  | `redo_log ->
      "Redo log"
  | `rrd ->
      "rrd"
  | `pvs_cache ->
      "PVS cache"
  | `cbt_metadata ->
      "CBT metadata"

let ip_configuration_mode_to_string = function
  | `None ->
      "None"
  | `DHCP ->
      "DHCP"
  | `Static ->
      "Static"

let ip_configuration_mode_of_string m =
  match String.lowercase_ascii m with
  | "dhcp" ->
      `DHCP
  | "none" ->
      `None
  | "static" ->
      `Static
  | s ->
      record_failure "Expected 'dhcp','none' or 'static', got %s" s

let vif_ipv4_configuration_mode_to_string = function
  | `None ->
      "None"
  | `Static ->
      "Static"

let vif_ipv4_configuration_mode_of_string m =
  match String.lowercase_ascii m with
  | "none" ->
      `None
  | "static" ->
      `Static
  | s ->
      record_failure "Expected 'none' or 'static', got %s" s

let ipv6_configuration_mode_to_string = function
  | `None ->
      "None"
  | `DHCP ->
      "DHCP"
  | `Static ->
      "Static"
  | `Autoconf ->
      "Autoconf"

let ipv6_configuration_mode_of_string m =
  match String.lowercase_ascii m with
  | "dhcp" ->
      `DHCP
  | "none" ->
      `None
  | "static" ->
      `Static
  | "autoconf" ->
      `Autoconf
  | s ->
      record_failure "Expected 'dhcp','none' 'autoconf' or 'static', got %s" s

let vif_ipv6_configuration_mode_to_string = function
  | `None ->
      "None"
  | `Static ->
      "Static"

let vif_ipv6_configuration_mode_of_string m =
  match String.lowercase_ascii m with
  | "none" ->
      `None
  | "static" ->
      `Static
  | s ->
      record_failure "Expected 'none' or 'static', got %s" s

let primary_address_type_to_string = function
  | `IPv4 ->
      "IPv4"
  | `IPv6 ->
      "IPv6"

let primary_address_type_of_string m =
  match String.lowercase_ascii m with
  | "ipv4" ->
      `IPv4
  | "ipv6" ->
      `IPv6
  | s ->
      record_failure "Expected 'ipv4' or 'ipv6', got %s" s

let bond_mode_to_string = function
  | `balanceslb ->
      "balance-slb"
  | `activebackup ->
      "active-backup"
  | `lacp ->
      "lacp"

let bond_mode_of_string m =
  match String.lowercase_ascii m with
  | "balance-slb" | "" ->
      `balanceslb
  | "active-backup" ->
      `activebackup
  | "lacp" ->
      `lacp
  | s ->
      record_failure "Invalid bond mode. Got %s" s

let allocation_algorithm_to_string = function
  | `depth_first ->
      "depth-first"
  | `breadth_first ->
      "breadth-first"

let allocation_algorithm_of_string a =
  match String.lowercase_ascii a with
  | "depth-first" ->
      `depth_first
  | "breadth-first" ->
      `breadth_first
  | s ->
      record_failure "Invalid allocation algorithm. Got %s" s

let pvs_proxy_status_to_string = function
  | `stopped ->
      "stopped"
  | `initialised ->
      "initialised"
  | `caching ->
      "caching"
  | `incompatible_write_cache_mode ->
      "incompatible-write-cache-mode"
  | `incompatible_protocol_version ->
      "incompatible-protocol-version"

let cluster_operation_to_string op = API.rpc_of_cluster_operation op |> to_str

let cluster_host_operation_to_string op =
  API.rpc_of_cluster_host_operation op |> to_str

let bool_of_string s =
  match String.lowercase_ascii s with
  | "true" | "t" | "yes" | "y" | "1" ->
      true
  | "false" | "f" | "no" | "n" | "0" ->
      false
  | _ ->
      record_failure
        "Expected 'true','t','yes','y','1','false','f','no','n','0' got %s" s

let sdn_protocol_of_string s =
  match String.lowercase_ascii s with
  | "ssl" ->
      `ssl
  | "pssl" ->
      `pssl
  | _ ->
      record_failure "Expected 'ssl','pssl', got %s" s

let sdn_protocol_to_string = function `ssl -> "ssl" | `pssl -> "pssl"

let tunnel_protocol_of_string s =
  match String.lowercase_ascii s with
  | "gre" ->
      `gre
  | "vxlan" ->
      `vxlan
  | _ ->
      record_failure "Expected 'gre','vxlan', got %s" s

let tunnel_protocol_to_string = function `gre -> "gre" | `vxlan -> "vxlan"

let pif_igmp_status_to_string = function
  | `enabled ->
      "enabled"
  | `disabled ->
      "disabled"
  | `unknown ->
      "unknown"

let vusb_operation_to_string = function
  | `attach ->
      "attach"
  | `plug ->
      "plug"
  | `unplug ->
      "unplug"

let network_sriov_configuration_mode_to_string = function
  | `sysfs ->
      "sysfs"
  | `modprobe ->
      "modprobe"
  | `manual ->
      "manual"
  | `unknown ->
      "unknown"

let on_boot_to_string onboot =
  match onboot with `reset -> "reset" | `persist -> "persist"

let tristate_to_string tristate =
  match tristate with
  | `yes ->
      "true"
  | `no ->
      "false"
  | `unspecified ->
      "unspecified"

let domain_type_to_string = function
  | `hvm ->
      "hvm"
  | `pv ->
      "pv"
  | `pv_in_pvh ->
      "pv-in-pvh"
  | `pvh ->
      "pvh"
  | `unspecified ->
      "unspecified"

let domain_type_of_string x =
  match String.lowercase_ascii x with
  | "hvm" ->
      `hvm
  | "pv" ->
      `pv
  | "pv-in-pvh" ->
      `pv_in_pvh
  | "pvh" ->
      `pvh
  | s ->
      record_failure "Invalid domain type. Got %s" s

let vtpm_operation_to_string (op : API.vtpm_operations) =
  match op with `destroy -> "destroy"

(** parse [0-9]*(b|bytes|kib|mib|gib|tib)* to bytes *)
let bytes_of_string str =
  let ( ** ) a b = Int64.mul a b in
  let invalid msg = raise (Invalid_argument msg) in
  try
    Scanf.sscanf str "%Ld %s" @@ fun size suffix ->
    match String.lowercase_ascii suffix with
    | _ when size < 0L ->
        invalid str
    | "bytes" | "b" | "" ->
        size
    | "kib" | "kb" | "k" ->
        size ** 1024L
    | "mib" | "mb" | "m" ->
        size ** 1024L ** 1024L
    | "gib" | "gb" | "g" ->
        size ** 1024L ** 1024L ** 1024L
    | "tib" | "tb" | "t" ->
        size ** 1024L ** 1024L ** 1024L ** 1024L
    | _ ->
        invalid suffix
  with _ -> invalid str

(** Parse a string which might have a units suffix on the end *)
let bytes_of_string field x =
  try bytes_of_string x
  with Invalid_argument _ ->
    record_failure
      "Failed to parse field '%s': expecting an integer (possibly with suffix \
       KiB, MiB, GiB, TiB), got '%s'"
      field x

let mac_from_int_array macs =
  (* make sure bit 1 (local) is set and bit 0 (unicast) is clear *)
  macs.(0) <- macs.(0) lor 0x2 land lnot 0x1 ;
  Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x" macs.(0) macs.(1) macs.(2)
    macs.(3) macs.(4) macs.(5)

(* generate a random mac that is locally administered *)
let random_mac_local () = mac_from_int_array (Array.make 6 (Random.int 0x100))

let update_sync_frequency_to_string = function
  | `daily ->
      "daily"
  | `weekly ->
      "weekly"

let update_sync_frequency_of_string s =
  match String.lowercase_ascii s with
  | "daily" ->
      `daily
  | "weekly" ->
      `weekly
  | _ ->
      record_failure "Expected 'daily', 'weekly', got %s" s

let vm_placement_policy_to_string = function
  | `normal ->
      "normal"
  | `anti_affinity ->
      "anti-affinity"

let vm_placement_policy_of_string a =
  match String.lowercase_ascii a with
  | "normal" ->
      `normal
  | "anti-affinity" ->
      `anti_affinity
  | s ->
      record_failure "Invalid VM placement policy, got %s" s

let repo_origin_to_string = function `remote -> "remote" | `bundle -> "bundle"
