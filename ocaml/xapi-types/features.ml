(* (C) 2006-2010 Citrix Systems Inc.
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

(* Features and restrictions *)

type feature =
  | VLAN
  | QoS
  | Shared_storage
  | Netapp
  | Equalogic
  | Pooling
  | HA
  | Marathon
  | Email
  | Performance
  | WLB
  | RBAC
  | DMC
  | Checkpoint
  | CPU_masking
  | Connection
  | No_platform_filter
  | No_nag_dialog
  | VMPR
  | VMSS
  | IntelliCache
  | GPU
  | DR
  | VIF_locking
  | Storage_motion
  | VGPU
  | Integrated_GPU
  | VSS
  | Guest_agent_auto_update
  | PCI_device_for_auto_update
  | Xen_motion
  | Guest_ip_setting
  | AD
  | Nested_virt
  | Live_patching
  | Live_set_vcpus
  | PVS_proxy
  | IGMP_snooping
  | RPU
  | Pool_size
  | CBT
  | USB_passthrough
  | Network_sriov
  | Corosync
  | Cluster_address
  | Zstd_export
  | Pool_secret_rotation
  | Certificate_verification
  | Updates
  | Internal_repo_access
  | VTPM
  | VM_groups
  | VM_start
  | VM_appliance_start
[@@deriving rpc, enum]

type orientation = Positive | Negative

let props_of_feature = function
  | VLAN ->
      ("restrict_vlan", Negative, "VLAN")
  | QoS ->
      ("restrict_qos", Negative, "QoS")
  | Shared_storage ->
      ("restrict_pool_attached_storage", Negative, "SStorage")
  | Netapp ->
      ("restrict_netapp", Negative, "NTAP")
  | Equalogic ->
      ("restrict_equalogic", Negative, "EQL")
  | Pooling ->
      ("restrict_pooling", Negative, "Pool")
  | HA ->
      ("enable_xha", Positive, "XHA")
  | Marathon ->
      ("restrict_marathon", Negative, "MTC")
  | Email ->
      ("restrict_email_alerting", Negative, "email")
  | Performance ->
      ("restrict_historical_performance", Negative, "perf")
  | WLB ->
      ("restrict_wlb", Negative, "WLB")
  | RBAC ->
      ("restrict_rbac", Negative, "RBAC")
  | DMC ->
      ("restrict_dmc", Negative, "DMC")
  | Checkpoint ->
      ("restrict_checkpoint", Negative, "chpt")
  | CPU_masking ->
      ("restrict_cpu_masking", Negative, "Mask")
  | Connection ->
      ("restrict_connection", Negative, "Cnx")
  | No_platform_filter ->
      ("platform_filter", Negative, "Plat")
  | No_nag_dialog ->
      ("regular_nag_dialog", Negative, "nonag")
  | VMPR ->
      ("restrict_vmpr", Negative, "VMPR")
  | VMSS ->
      ("restrict_vmss", Negative, "VMSS")
  | IntelliCache ->
      ("restrict_intellicache", Negative, "IntelliCache")
  | GPU ->
      ("restrict_gpu", Negative, "GPU")
  | DR ->
      ("restrict_dr", Negative, "DR")
  | VIF_locking ->
      ("restrict_vif_locking", Negative, "VIFLock")
  | Storage_motion ->
      ("restrict_storage_xen_motion", Negative, "SXM")
  | VGPU ->
      ("restrict_vgpu", Negative, "vGPU")
  | Integrated_GPU ->
      ("restrict_integrated_gpu_passthrough", Negative, "iGPU")
  | VSS ->
      ("restrict_vss", Negative, "VSS")
  | Guest_agent_auto_update ->
      ("restrict_guest_agent_auto_update", Negative, "GAAU")
  | PCI_device_for_auto_update ->
      ("restrict_pci_device_for_auto_update", Negative, "PciAU")
  | Xen_motion ->
      ("restrict_xen_motion", Negative, "Live_migration")
  | Guest_ip_setting ->
      ("restrict_guest_ip_setting", Negative, "GuestIP")
  | AD ->
      ("restrict_ad", Negative, "AD")
  | Nested_virt ->
      ("restrict_nested_virt", Negative, "Nested_virt")
  | Live_patching ->
      ("restrict_live_patching", Negative, "Live_patching")
  | Live_set_vcpus ->
      ("restrict_set_vcpus_number_live", Negative, "Live_set_vcpus")
  | PVS_proxy ->
      ("restrict_pvs_proxy", Negative, "PVS_proxy")
  | IGMP_snooping ->
      ("restrict_igmp_snooping", Negative, "IGMP_snooping")
  | RPU ->
      ("restrict_rpu", Negative, "RPU")
  | Pool_size ->
      ("restrict_pool_size", Negative, "Pool_size")
  | CBT ->
      ("restrict_cbt", Negative, "CBT")
  | USB_passthrough ->
      ("restrict_usb_passthrough", Negative, "USB_passthrough")
  | Network_sriov ->
      ("restrict_network_sriov", Negative, "Network_sriov")
  | Corosync ->
      ("restrict_corosync", Negative, "Corosync")
  | Cluster_address ->
      ("restrict_cluster_address", Negative, "Cluster_address")
  | Zstd_export ->
      ("restrict_zstd_export", Negative, "Zstd_export")
  | Pool_secret_rotation ->
      ("restrict_pool_secret_rotation", Negative, "Pool_secret_rotation")
  | Certificate_verification ->
      ("restrict_certificate_verification", Negative, "Certificate_verification")
  | Updates ->
      ("restrict_updates", Negative, "Upd")
  | Internal_repo_access ->
      ("restrict_internal_repo_access", Negative, "Internal_repo_access")
  | VTPM ->
      ("restrict_vtpm", Negative, "VTPM")
  | VM_groups ->
      ("restrict_vm_groups", Negative, "VM_groups")
  | VM_start ->
      ("restrict_vm_start", Negative, "Start")
  | VM_appliance_start ->
      ("restrict_vm_appliance_start", Negative, "Start")

(* A list of features that must be considered "enabled" by `of_assoc_list`
   if the feature string is missing from the list. These are existing features
   that have been recently restricted, and which we want to remain enabled during
   a rolling pool upgrade. *)
let enabled_when_unknown =
  [Xen_motion; AD; Updates; VM_start; VM_appliance_start]

let all_features =
  let length = max_feature - min_feature + 1 in
  let start = min_feature in
  List.init length (fun i -> feature_of_enum (i + start) |> Option.get)

let name_of_feature f = rpc_of_feature f |> Rpc.string_of_rpc

let is_enabled v = function Positive -> v | Negative -> not v

let to_compact_string (s : feature list) =
  let get_tag f =
    let _, _, tag = props_of_feature f in
    if List.mem f s then
      tag
    else
      String.make (String.length tag) ' '
  in
  List.map get_tag all_features |> String.concat " "

let to_assoc_list (s : feature list) =
  let get_map f =
    let str, o, _ = props_of_feature f in
    let switch = List.mem f s in
    let switch = string_of_bool (is_enabled switch o) in
    (str, switch)
  in
  List.map get_map all_features

let of_assoc_list l =
  let enabled f =
    try
      let str, o, _ = props_of_feature f in
      let v = List.assoc str l in
      is_enabled (bool_of_string v) o
    with _ -> List.mem f enabled_when_unknown
  in
  List.filter enabled all_features
