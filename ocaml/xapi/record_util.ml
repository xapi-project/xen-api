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

open Stdext.Xstringext

let power_state_to_string state =
  match state with
    `Halted -> "Halted"
  | `Paused -> "Paused"
  | `Running -> "Running"
  | `Suspended -> "Suspended"
  | `ShuttingDown -> "Shutting down"
  | `Migrating -> "Migrating"

let vm_operation_table =
  [
    `assert_operation_valid, "assertoperationvalid";
    `changing_dynamic_range, "changing_dynamic_range";
    `changing_static_range, "changing_static_range";
    `changing_shadow_memory, "changing_shadow_memory";
    `clean_reboot, "clean_reboot";
    `clean_shutdown, "clean_shutdown";
    `clone, "clone";
    `snapshot, "snapshot";
    `checkpoint, "checkpoint";
    `snapshot_with_quiesce, "snapshot_with_quiesce";
    `copy, "copy";
    `revert, "revert";
    `reverting, "reverting";
    `provision, "provision";
    `destroy, "destroy";
    `export, "export";
    `metadata_export, "metadata_export";
    `import, "import";
    `get_boot_record, "get_boot_record";
    `data_source_op, "data_sources_op";
    `hard_reboot, "hard_reboot";
    `hard_shutdown, "hard_shutdown";
    `migrate_send, "migrate_send";
    `pause, "pause";
    `resume, "resume";
    `resume_on, "resume_on";
    `changing_VCPUs_live, "changing_VCPUs_live";
    `start, "start";
    `start_on, "start_on";
    `suspend, "suspend";
    `unpause, "unpause";
    `update_allowed_operations, "update_allowed_operations";
    `make_into_template, "make_into_template";
    `send_sysrq, "send_sysrq";
    `send_trigger, "send_trigger";
    `changing_memory_live, "changing_memory_live";
    `awaiting_memory_live, "awaiting_memory_live";
    `changing_shadow_memory_live, "changing_shadow_memory_live";
    `pool_migrate, "pool_migrate";
    `power_state_reset, "power_state_reset";
    `csvm, "csvm";
    `call_plugin, "call_plugin";
  ]

let vm_operation_to_string x =
  if not(List.mem_assoc x vm_operation_table)
  then "(unknown operation)"
  else List.assoc x vm_operation_table

let string_to_vm_operation x =
  let table = List.map (fun (a, b) -> b, a) vm_operation_table in
  if not(List.mem_assoc x table)
  then (raise (Api_errors.Server_error(Api_errors.invalid_value, [ "blocked_operation"; x ])))
  else List.assoc x table

let pool_operation_to_string = function
  | `ha_enable -> "ha_enable"
  | `ha_disable -> "ha_disable"

let host_operation_to_string = function
  | `provision -> "provision"
  | `evacuate -> "evacuate"
  | `shutdown -> "shutdown"
  | `reboot -> "reboot"
  | `power_on -> "power_on"
  | `vm_start -> "VM.start"
  | `vm_resume -> "VM.resume"
  | `vm_migrate -> "VM.migrate"

let vdi_operation_to_string: API.vdi_operations -> string = function
  | `clone -> "clone"
  | `copy -> "copy"
  | `resize -> "resize"
  | `resize_online -> "resize_online"
  | `destroy -> "destroy"
  | `force_unlock -> "force_unlock"
  | `snapshot -> "snapshot"
  | `mirror -> "mirror"
  | `forget -> "forget"
  | `update -> "update"
  | `generate_config -> "generate_config"
  | `enable_cbt -> "enable_cbt"
  | `disable_cbt -> "disable_cbt"
  | `data_destroy -> "data_destroy"
  | `list_changed_blocks -> "list_changed_blocks"
  | `set_on_boot -> "set_on_boot"
  | `blocked -> "blocked"

let sr_operation_to_string: API.storage_operations -> string = function
  | `scan -> "scan"
  | `destroy -> "destroy"
  | `forget -> "forget"
  | `plug -> "plug"
  | `unplug -> "unplug"
  | `update -> "update"
  | `vdi_create -> "VDI.create"
  | `vdi_introduce -> "VDI.introduce"
  | `vdi_destroy -> "VDI.destroy"
  | `vdi_resize -> "VDI.resize"
  | `vdi_clone -> "VDI.clone"
  | `vdi_snapshot -> "VDI.snapshot"
  | `vdi_mirror -> "VDI.mirror"
  | `vdi_enable_cbt -> "VDI.enable_cbt"
  | `vdi_disable_cbt -> "VDI.disable_cbt"
  | `vdi_set_on_boot -> "VDI.set_on_boot"
  | `vdi_data_destroy -> "VDI.data_destroy"
  | `vdi_list_changed_blocks -> "VDI.list_changed_blocks"
  | `pbd_create -> "PBD.create"
  | `pbd_destroy -> "PBD.destroy"

let vbd_operation_to_string = function
  | `attach -> "attach"
  | `eject -> "eject"
  | `insert -> "insert"
  | `plug -> "plug"
  | `unplug -> "unplug"
  | `unplug_force -> "unplug_force"
  | `pause -> "pause"
  | `unpause -> "unpause"

let vif_operation_to_string = function
  | `attach -> "attach"
  | `plug -> "plug"
  | `unplug -> "unplug"
  | `unplug_force -> "unplug_force"

let vif_locking_mode_to_string = function
  | `network_default -> "network_default"
  | `locked -> "locked"
  | `unlocked -> "unlocked"
  | `disabled -> "disabled"

let string_to_vif_locking_mode = function
  | "network_default" -> `network_default
  | "locked" -> `locked
  | "unlocked" -> `unlocked
  | "disabled" -> `disabled
  | s -> raise (Record_failure ("Expected 'network_default', 'locked', 'unlocked', 'disabled', got "^s))

let vmss_type_to_string = function
  | `snapshot -> "snapshot"
  | `checkpoint -> "checkpoint"
  | `snapshot_with_quiesce -> "snapshot_with_quiesce"

let string_to_vmss_type = function
  | "snapshot" -> `snapshot
  | "checkpoint" -> `checkpoint
  | "snapshot_with_quiesce" -> `snapshot_with_quiesce
  | s -> raise (Record_failure ("Expected 'snapshot', 'checkpoint', 'snapshot_with_quiesce', got "^s))

let vmss_frequency_to_string = function
  | `hourly -> "hourly"
  | `daily -> "daily"
  | `weekly -> "weekly"

let string_to_vmss_frequency = function
  | "hourly" -> `hourly
  | "daily" -> `daily
  | "weekly" -> `weekly
  | s -> raise (Record_failure ("Expected 'hourly', 'daily', 'weekly', got "^s))

let network_default_locking_mode_to_string = function
  | `unlocked -> "unlocked"
  | `disabled -> "disabled"

let string_to_network_default_locking_mode = function
  | "unlocked" -> `unlocked
  | "disabled" -> `disabled
  | s -> raise (Record_failure ("Expected 'unlocked' or 'disabled', got "^s))

let network_purpose_to_string: API.network_purpose -> string = function
  | `nbd -> "nbd"
  | `insecure_nbd -> "insecure_nbd"

let string_to_network_purpose: string -> API.network_purpose = function
  | "nbd" -> `nbd
  | "insecure_nbd" -> `insecure_nbd
  | s -> raise (Record_failure ("Expected a network purpose string; got "^s))

let vm_appliance_operation_to_string = function
  | `start -> "start"
  | `clean_shutdown -> "clean_shutdown"
  | `hard_shutdown -> "hard_shutdown"
  | `shutdown -> "shutdown"

let cpu_feature_to_string f =
  match f with
    `FPU -> "FPU"
  | `VME -> "VME"
  | `DE -> "DE"
  | `PSE -> "PSE"
  | `TSC -> "TSC"
  | `MSR -> "MSR"
  | `PAE -> "PAE"
  | `MCE -> "MCE"
  | `CX8 -> "CX8"
  | `APIC -> "APIC"
  | `SEP -> "SEP"
  | `MTRR -> "MTRR"
  | `PGE -> "PGE"
  | `MCA -> "MCA"
  | `CMOV -> "CMOV"
  | `PAT -> "PAT"
  | `PSE36 -> "PSE36"
  | `PN -> "PN"
  | `CLFLSH -> "CLFLSH"
  | `DTES -> "DTES"
  | `ACPI -> "ACPI"
  | `MMX -> "MMX"
  | `FXSR -> "FXSR"
  | `XMM -> "XMM"
  | `XMM2 -> "XMM2"
  | `SELFSNOOP -> "SELFSNOOP"
  | `HT -> "HT"
  | `ACC -> "ACC"
  | `IA64 -> "IA64"
  | `SYSCALL -> "SYSCALL"
  | `MP -> "MP"
  | `NX -> "NX"
  | `MMXEXT -> "MMXEXT"
  | `LM -> "LM"
  | `THREEDNOWEXT -> "3DNOWEXT"
  | `THREEDNOW -> "3DNOW"
  | `RECOVERY -> "RECOVERY"
  | `LONGRUN -> "LONGRUN"
  | `LRTI -> "LRTI"
  | `CXMMX -> "CXMMX"
  | `K6MTRR -> "K6MTRR"
  | `CYRIXARR -> "CYRIXARR"
  | `CENTAURMCR -> "CENTAURMCR"
  | `K8 -> "K8"
  | `K7 -> "K7"
  | `P3 -> "P3"
  | `P4 -> "P4"
  | `CONSTANTTSC -> "CONSTANTTSC"
  | `FXSAVELEAK -> "FXSAVELEAK"
  | `XMM3 -> "XMM3"
  | `MWAIT -> "MWAIT"
  | `DSCPL -> "DSCPL"
  | `EST -> "EST"
  | `TM2 -> "TM2"
  | `CID -> "CID"
  | `CX16 -> "CX16"
  | `XTPR -> "XTPR"
  | `XSTORE -> "XSTORE"
  | `XSTOREEN -> "XSTOREEN"
  | `XCRYPT -> "XCRYPT"
  | `XCRYPTEN -> "XCRYPTEN"
  | `LAHFLM -> "LAHFLM"
  | `CMPLEGACY -> "CMPLEGACY"
  | `VMX -> "VMX"

let task_status_type_to_string s =
  match s with
  | `pending -> "pending"
  | `success -> "success"
  | `failure -> "failure"
  | `cancelling -> "cancelling"
  | `cancelled -> "cancelled"

let protocol_to_string = function
  | `vt100 -> "VT100"
  | `rfb -> "RFB"
  | `rdp -> "RDP"

let cpu_feature_list_to_string list =
  String.concat "," (List.map (fun x -> cpu_feature_to_string x) list)

let task_allowed_operations_to_string s =
  match s with
  | `cancel -> "Cancel"
  | `destroy -> "Destroy"

let alert_level_to_string s =
  match s with
  | `Info -> "info"
  | `Warn -> "warning"
  | `Error -> "error"

let on_normal_exit_to_string x =
  match x with
    `destroy -> "Destroy"
  | `restart -> "Restart"

let string_to_on_normal_exit s =
  match String.lowercase_ascii s with
    "destroy" -> `destroy
  | "restart" -> `restart
  | _ -> raise (Record_failure ("Expected 'destroy' or 'restart', got "^s))

let on_crash_behaviour_to_string x=
  match x with
    `destroy -> "Destroy"
  | `coredump_and_destroy -> "Core dump and destroy"
  | `restart -> "Restart"
  | `coredump_and_restart -> "Core dump and restart"
  | `preserve -> "Preserve"
  | `rename_restart -> "Rename restart"

let string_to_on_crash_behaviour s=
  match String.lowercase_ascii s with
  | "destroy" -> `destroy
  | "coredump_and_destroy" -> `coredump_and_destroy
  | "restart" -> `restart
  | "coredump_and_restart" -> `coredump_and_restart
  | "preserve" -> `preserve
  | "rename_restart" -> `rename_restart
  | _ -> raise (Record_failure ("Expected 'destroy', 'coredump_and_destroy'," ^
                                "'restart', 'coredump_and_restart', 'preserve' or 'rename_restart', got "^s))

let host_display_to_string h =
  match h with
  | `enabled -> "enabled"
  | `enable_on_reboot -> "enable_on_reboot"
  | `disabled -> "disabled"
  | `disable_on_reboot -> "disable_on_reboot"

let pgpu_dom0_access_to_string x =
  host_display_to_string x

let boot_type_to_string x =
  match x with
    `bios -> "BIOS"
  | `grub -> "GRUB"
  | `kernelexternal -> "Kernel external"

let string_to_boot_type s =
  match String.lowercase_ascii s with
    "bios" -> `bios
  | "grub" -> `grub
  | "kernelexternal" -> `kernelexternal
  | _ -> raise (Record_failure ("Expected 'bios', 'grub' or 'kernelexternal', got "^s))

let string_to_vdi_onboot s =
  match String.lowercase_ascii s with
  | "persist" -> `persist
  | "reset" -> `reset
  | _ -> raise (Record_failure ("Expected 'persist' or 'reset', got "^s))

let string_to_vbd_mode s =
  match String.lowercase_ascii s with
  | "ro" -> `RO
  | "rw" -> `RW
  | _ -> raise (Record_failure ("Expected 'RO' or 'RW', got "^s))

let vbd_mode_to_string = function
  | `RO -> "ro"
  | `RW -> "rw"

let string_to_vbd_type s =
  match String.lowercase_ascii s with
  | "cd" -> `CD
  | "disk" -> `Disk
  | "floppy" -> `Floppy
  | _ -> raise (Record_failure ("Expected 'CD' or 'Disk', got "^s))

let power_to_string h =
  match h with
    `Halted -> "halted"
  | `Paused -> "paused"
  | `Running -> "running"
  | `Suspended -> "suspended"
  | `ShuttingDown -> "shutting down"
  | `Migrating -> "migrating"

let vdi_type_to_string t =
  match t with
  | `system -> "System"
  | `user -> "User"
  | `ephemeral -> "Ephemeral"
  | `suspend -> "Suspend"
  | `crashdump -> "Crashdump"
  | `ha_statefile -> "HA statefile"
  | `metadata -> "Metadata"
  | `redo_log -> "Redo log"
  | `rrd -> "rrd"
  | `pvs_cache -> "PVS cache"
  | `cbt_metadata -> "CBT metadata"

let ip_configuration_mode_to_string = function
  | `None -> "None"
  | `DHCP -> "DHCP"
  | `Static -> "Static"

let ip_configuration_mode_of_string m =
  match String.lowercase_ascii m with
  | "dhcp"   -> `DHCP
  | "none"   -> `None
  | "static" -> `Static
  | s        -> raise (Record_failure ("Expected 'dhcp','none' or 'static', got "^s))

let vif_ipv4_configuration_mode_to_string = function
  | `None -> "None"
  | `Static -> "Static"

let vif_ipv4_configuration_mode_of_string m =
  match String.lowercase_ascii m with
  | "none"   -> `None
  | "static" -> `Static
  | s        -> raise (Record_failure ("Expected 'none' or 'static', got "^s))

let ipv6_configuration_mode_to_string = function
  | `None -> "None"
  | `DHCP -> "DHCP"
  | `Static -> "Static"
  | `Autoconf -> "Autoconf"

let ipv6_configuration_mode_of_string m =
  match String.lowercase_ascii m with
  | "dhcp"   -> `DHCP
  | "none"   -> `None
  | "static" -> `Static
  | "autoconf" -> `Autoconf
  | s        -> raise (Record_failure ("Expected 'dhcp','none' 'autoconf' or 'static', got "^s))

let vif_ipv6_configuration_mode_to_string = function
  | `None -> "None"
  | `Static -> "Static"

let vif_ipv6_configuration_mode_of_string m =
  match String.lowercase_ascii m with
  | "none"   -> `None
  | "static" -> `Static
  | s        -> raise (Record_failure ("Expected 'none' or 'static', got "^s))

let primary_address_type_to_string = function
  | `IPv4 -> "IPv4"
  | `IPv6 -> "IPv6"

let primary_address_type_of_string m =
  match String.lowercase_ascii m with
  | "ipv4"   -> `IPv4
  | "ipv6"   -> `IPv6
  | s        -> raise (Record_failure ("Expected 'ipv4' or 'ipv6', got "^s))

let bond_mode_to_string = function
  | `balanceslb -> "balance-slb"
  | `activebackup -> "active-backup"
  | `lacp -> "lacp"

let bond_mode_of_string m =
  match String.lowercase_ascii m with
  | "balance-slb" | "" -> `balanceslb
  | "active-backup" -> `activebackup
  | "lacp" -> `lacp
  | s -> raise (Record_failure ("Invalid bond mode. Got " ^ s))

let allocation_algorithm_to_string = function
  | `depth_first -> "depth-first"
  | `breadth_first -> "breadth-first"

let allocation_algorithm_of_string a =
  match String.lowercase_ascii a with
  | "depth-first" -> `depth_first
  | "breadth-first" -> `breadth_first
  | s -> raise (Record_failure ("Invalid allocation algorithm. Got " ^ s))

let pvs_proxy_status_to_string = function
  | `stopped -> "stopped"
  | `initialised -> "initialised"
  | `caching -> "caching"
  | `incompatible_write_cache_mode -> "incompatible-write-cache-mode"
  | `incompatible_protocol_version -> "incompatible-protocol-version"

let bool_of_string s =
  match String.lowercase_ascii s with
  |"true"|"yes"->true
  |"false"|"no"->false
  |_-> raise (Record_failure ("Expected 'true','yes','false','no', got "^s))

let sdn_protocol_of_string s =
  match String.lowercase_ascii s with
  | "ssl" -> `ssl
  | "pssl" -> `pssl
  |_-> raise (Record_failure ("Expected 'ssl','pssl', got "^s))

let sdn_protocol_to_string = function
  | `ssl -> "ssl"
  | `pssl -> "pssl"

let pif_igmp_status_to_string = function
  | `enabled -> "enabled"
  | `disabled -> "disabled"
  | `unknown -> "unknown"

let vusb_operation_to_string = function
  | `attach -> "attach"
  | `plug -> "plug"
  | `unplug -> "unplug"

let network_sriov_configuration_mode_to_string = function
  | `sysfs -> "sysfs"
  | `modprobe -> "modprobe"
  | `unknown -> "unknown"

(* string_to_string_map_to_string *)
let s2sm_to_string sep x =
  String.concat sep (List.map (fun (a,b) -> a^": "^b) x)

(* string to blob ref map to string *)
let s2brm_to_string get_uuid_from_ref sep x =
  String.concat sep (List.map (fun (n,r) -> n ^ ": " ^ (get_uuid_from_ref r)) x)

(* int64_to_float_map_to_string *)
let i642fm_to_string sep x =
  String.concat sep (List.map (fun (a,b) -> Printf.sprintf "%Ld %f" a b) x)

(* int64_to_string_map_to_string *)
let i642sm_to_string sep x =
  String.concat sep (List.map (fun (a,b) -> Printf.sprintf "%Ld %s" a b) x)

let on_boot_to_string onboot =
  match onboot with
  | `reset -> "reset"
  | `persist -> "persist"

let tristate_to_string tristate =
  match tristate with
  | `yes -> "true"
  | `no -> "false"
  | `unspecified -> "unspecified"

let wrap f err x = try f x with _ -> err x
let generic_error x = raise (Record_failure ("Unknown value: "^x))
let rpc_to_string = function | Rpc.String s -> s | _ -> failwith "Bad RPC type in record_util"

(** Parse a string which might have a units suffix on the end *)
let bytes_of_string field x =
  let isdigit c = c >= '0' && c <= '9' in
  let ( ** ) a b = Int64.mul a b in
  let max_size_TiB = Int64.div Int64.max_int (1024L ** 1024L ** 1024L ** 1024L) in
  (* detect big number that cannot be represented by Int64. *)
  let int64_of_string s =
    try
      Int64.of_string s
    with _ ->
      if s = "" then
        raise (Record_failure (Printf.sprintf "Failed to parse field '%s': expecting an integer (possibly with suffix)" field));
      let alldigit = ref true and i = ref (String.length s - 1) in
      while !alldigit && !i > 0 do alldigit := isdigit s.[!i]; decr i done;
      if !alldigit then
        raise (Record_failure (Printf.sprintf "Failed to parse field '%s': number too big (maximum = %Ld TiB)" field max_size_TiB))
      else
        raise (Record_failure (Printf.sprintf "Failed to parse field '%s': expecting an integer (possibly with suffix)" field));
  in
  match (String.split_f (fun c -> String.isspace c || (isdigit c)) x) with
  | [] ->
    (* no suffix on the end *)
    int64_of_string x
  | [ suffix ] -> begin
      let number = match (String.split_f (fun x -> not (isdigit x)) x) with
        | [ number ] -> int64_of_string number
        | _ -> raise (Record_failure (Printf.sprintf "Failed to parse field '%s': expecting an integer (possibly with suffix)" field)) in
      let multiplier = match suffix with
        | "bytes" -> 1L
        | "KiB" -> 1024L
        | "MiB" -> 1024L ** 1024L
        | "GiB" -> 1024L ** 1024L ** 1024L
        | "TiB" -> 1024L ** 1024L ** 1024L ** 1024L
        | x -> raise (Record_failure (Printf.sprintf "Failed to parse field '%s': Unknown suffix: '%s' (try KiB, MiB, GiB or TiB)" field x)) in
      (* FIXME: detect overflow *)
      number ** multiplier
    end
  | _ -> raise (Record_failure (Printf.sprintf "Failed to parse field '%s': expecting an integer (possibly with suffix)" field))

(* Vincent's random mac utils *)

(* generate a random mac with XenSource OUI "00:16:3e" *)
let random_mac () =
  let macs = [0x00; 0x16; 0x3e] @ (List.map Random.int [0x80; 0x100; 0x100]) in
  String.concat ":" (List.map (Printf.sprintf "%02x") macs)

let mac_from_int_array macs =
  (* make sure bit 1 (local) is set and bit 0 (unicast) is clear *)
  macs.(0) <- ((macs.(0) lor 0x2) land (lnot 0x1));
  Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x" macs.(0) macs.(1) macs.(2)
    macs.(3) macs.(4) macs.(5)

(* generate a random mac that is locally administered *)
let random_mac_local () =
  mac_from_int_array (Array.init 6 (fun i -> Random.int 0x100))
