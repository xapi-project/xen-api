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
(* NOTE: Unless conversion requires some custom logic, no new functions should
   be added here. Automatically-generated functions with consistent behaviour
   and naming are generated from the datamodel and included here.
   If the custom logic is required, these functions should be shadowed and
   justified here.
   See:
    _build/default/ocaml/xapi-cli-server/generated_record_utils.ml
   for the generated code. And:
    ~/xen-api/ocaml/idl/ocaml_backend/gen_api.ml
   for the code generating it.
*)

include Generated_record_utils

let to_str = function Rpc.String x -> x | _ -> failwith "Invalid"

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

(* Intentional shadowing - data_souces_op, assertoperationinvalid,
   changing_vcpus, changing_memory_limits, query_services, create_template
   are inconsistent *)
let vm_operation_to_string x =
  if not (List.mem_assoc x vm_operation_table) then
    "(unknown operation)"
  else
    List.assoc x vm_operation_table

(* Intentional shadowing -
   In addition to the above, also inconsistent exceptions *)
let string_to_vm_operation x =
  let table = List.map (fun (a, b) -> (b, a)) vm_operation_table in
  if not (List.mem_assoc x table) then
    raise
      (Api_errors.Server_error
         (Api_errors.invalid_value, ["blocked_operation"; x])
      )
  else
    List.assoc x table

(* Intentional shadowing - inconsistent behaviour:
   vm_start, vm_resume, vm_migrate *)
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

(* Intentional shadowing - inconsistent behaviour around _/. *)
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
  | `vdi_blocked ->
      "VDI.blocked"
  | `vdi_copy ->
      "VDI.copy"
  | `vdi_force_unlock ->
      "VDI.force_unlock"
  | `vdi_forget ->
      "VDI.forget"
  | `vdi_generate_config ->
      "VDI.generate_config"
  | `vdi_resize_online ->
      "VDI.resize_online"
  | `vdi_update ->
      "VDI.update"
  | `pbd_create ->
      "PBD.create"
  | `pbd_destroy ->
      "PBD.destroy"

(* Is not defined in the datamodel - only defined here *)
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

(* Intentional shadowing - inconsistent capitalization *)
let protocol_to_string = function
  | `vt100 ->
      "VT100"
  | `rfb ->
      "RFB"
  | `rdp ->
      "RDP"

(* Intentional shadowing - inconsistent capitalization *)
let task_allowed_operations_to_string s =
  match s with `cancel -> "Cancel" | `destroy -> "Destroy"

(* Is not defined in the datamodel - only defined here *)
let alert_level_to_string s =
  match s with `Info -> "info" | `Warn -> "warning" | `Error -> "error"

(* Intentional shadowing - inconsistent capitalization *)
let on_normal_exit_to_string x =
  match x with `destroy -> "Destroy" | `restart -> "Restart"

(* Intentional shadowing - inconsistent capitalization *)
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

(* Intentional shadowing - inconsistent capitalization *)
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

let pci_dom0_access_to_string x = host_display_to_string x

let string_to_vdi_onboot s =
  match String.lowercase_ascii s with
  | "persist" ->
      `persist
  | "reset" ->
      `reset
  | _ ->
      record_failure "Expected 'persist' or 'reset', got %s" s

(* Intentional shadowing - inconsistent capitalization *)
let vbd_mode_to_string = function `RO -> "ro" | `RW -> "rw"

(* Some usage sites rely on the output of the
   conversion function to be lowercase*)
let vm_power_state_to_lowercase_string h =
  vm_power_state_to_string h |> String.uncapitalize_ascii

(* Intentional shadowing - inconsistent capitalization *)
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

(* Intentional shadowing - inconsistent underscore/dash *)
let bond_mode_to_string = function
  | `balanceslb ->
      "balance-slb"
  | `activebackup ->
      "active-backup"
  | `lacp ->
      "lacp"

(* Intentional shadowing - inconsistent underscore/dash, custom case *)
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

(* Intentional shadowing - inconsistent underscore/dash *)
let allocation_algorithm_to_string = function
  | `depth_first ->
      "depth-first"
  | `breadth_first ->
      "breadth-first"

(* Intentional shadowing - inconsistent underscore/dash *)
let allocation_algorithm_of_string a =
  match String.lowercase_ascii a with
  | "depth-first" ->
      `depth_first
  | "breadth-first" ->
      `breadth_first
  | s ->
      record_failure "Invalid allocation algorithm. Got %s" s

(* Intentional shadowing - inconsistent underscore/dash *)
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

(* Intentional shadowing - inconsistent naming *)
let tristate_to_string tristate =
  match tristate with
  | `yes ->
      "true"
  | `no ->
      "false"
  | `unspecified ->
      "unspecified"

(* Intentional shadowing - inconsistent underscore/dash *)
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

(* Intentional shadowing - inconsistent underscore/dash *)
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

(* Intentional shadowing - inconsistent underscore/dash *)
let vm_placement_policy_to_string = function
  | `normal ->
      "normal"
  | `anti_affinity ->
      "anti-affinity"

(* Intentional shadowing - inconsistent underscore/dash *)
let vm_placement_policy_of_string a =
  match String.lowercase_ascii a with
  | "normal" ->
      `normal
  | "anti-affinity" ->
      `anti_affinity
  | s ->
      record_failure "Invalid VM placement policy, got %s" s
