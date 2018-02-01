(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

open Network
open Db_filter_types
module D = Debug.Make(struct let name="xapi" end)
open D

let mutex = Mutex.create ()

let pci_path x = Printf.sprintf "/sys/bus/pci/devices/%s/physfn" x

let assert_no_vf_inuse ~__context ~pif =
  let pci = Db.PIF.get_PCI ~__context ~self:pif in
  let vfs = Db.PCI.get_virtual_functions ~__context ~self:pci in
  let vfs_inuse = List.filter (fun vf ->
      (Db.PCI.get_attached_VMs ~__context ~self:vf <> []) || (Db.PCI.get_scheduled_to_be_attached_to ~__context ~self:vf <> Ref.null)) vfs in
  if vfs_inuse <> [] then raise (Api_errors.Server_error(Api_errors.network_sriov_pif_has_vf_inuse, [Ref.string_of pif; String.concat "," (List.map Ref.string_of vfs_inuse)]))

let update_sriovs ~__context =
  let is_phy_fn ~pci =
    let pci_id = Db.PCI.get_pci_id ~__context ~self:pci in
    let path = pci_path pci_id in
    try
      (*if can't read link from the path,then it's a physical function*)
      let _ = Unix.readlink path in
      false
    with _ -> true
  in
  let set_phy_fn ~vf ~pfs =
    let pci_id = Db.PCI.get_pci_id ~__context ~self:vf in
    let path = pci_path pci_id in
    let physfn_addr = Filename.basename(Unix.readlink path) in
    try
      let pf = List.find (fun pf -> physfn_addr = Db.PCI.get_pci_id ~__context ~self:pf) pfs in
      Db.PCI.set_physical_function ~__context ~self:vf ~value:pf
    with Not_found ->
      error "Failed to find physical function of vf %s" (Db.PCI.get_uuid ~__context ~self:vf);
      raise (Api_errors.Server_error(Api_errors.network_sriov_find_pf_from_vf_failed, [Ref.string_of vf]))
  in
  Xapi_pci.update_pcis ~__context;
  let open Xapi_stdext_threads in
  Threadext.Mutex.execute mutex (fun () ->
      let local_pcis = Xapi_pci.get_local_pci_refs ~__context in
      let pfs, vfs = List.partition (fun pci -> is_phy_fn ~pci) local_pcis in
      (* set physical function for vfs *)
      List.iter (fun vf -> if Db.PCI.get_physical_function ~__context ~self:vf = Ref.null then set_phy_fn ~vf ~pfs) vfs;
      (* update pf's functions *)
      List.iter (fun pf ->
          let vf_cnt = Db.PCI.get_virtual_functions ~__context ~self:pf |> List.length in
          Db.PCI.set_functions ~__context ~self:pf ~value:(Int64.of_int (vf_cnt + 1))
        ) pfs
    )

let need_operate_pci_device ~__context ~self =
  let is_sriov_enabled ~pif =
    match Db.PIF.get_sriov_logical_PIF_of ~__context ~self:pif with
    | [] -> false
    | sriov :: _ ->
      Db.PIF.get_currently_attached ~__context ~self:pif = true || Db.Network_sriov.get_requires_reboot ~__context ~self:sriov = true
  in
  if is_sriov_enabled ~pif:self then begin
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self) in
    match Db.Network_sriov.get_configuration_mode ~__context ~self:sriov with
    | `sysfs -> true
    | `unknown -> false
    | `modprobe ->
      let host = Db.PIF.get_host ~__context ~self in
      let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
      let pci = Db.PIF.get_PCI ~__context ~self:physical_pif in
      let driver_name = Db.PCI.get_driver_name ~__context ~self:pci in
      (* Filter the network SR-IOV logical PIF on local host which *)
      (* 1. has same driver name with me *)
      (* 2. PIF.currently_attached = `true` or Network_sriov.requires_reboot = `true`. Aka the PIF that enabled SR-IOV or will enable SR-IOV after reboot *)
      (* If the final list just contains me, should call networkd to disable SR-IOV for the device. *)
      Db.PIF.get_records_where ~__context ~expr:(And (
          Eq (Field "host", Literal (Ref.string_of host)),
          Not (Eq (Field "sriov_logical_PIF_of", Literal "()"))
        ))
      |> List.filter (fun (_, pif_rec) ->
          let sriov = List.hd pif_rec.API.pIF_sriov_logical_PIF_of in
          let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
          let pci = Db.PIF.get_PCI ~__context ~self:physical_pif in
          Db.PCI.get_driver_name ~__context ~self:pci = driver_name
        )
      |> List.filter (fun (pif_ref, pif_rec) ->
          is_sriov_enabled ~pif:pif_ref
        )
      |> List.length
      |> (=) 1
  end
  else false

let is_device_underneath_same_type ~__context pif1 pif2 =
  let get_device_info pif =
    let pci = Db.PIF.get_PCI ~__context ~self:pif in
    Db.PCI.get_vendor_id ~__context ~self:pci, Db.PCI.get_device_id ~__context ~self:pci
  in
  (get_device_info pif1) = (get_device_info pif2)

let assert_sriov_pif_compatible_with_network ~__context ~pif ~network =
  match Db.Network.get_PIFs ~__context ~self:network with
  | [] -> ()
  | logical_pif :: _ ->
    begin
      match Db.PIF.get_sriov_logical_PIF_of ~__context ~self:logical_pif with
      | [] -> raise (Api_errors.Server_error(Api_errors.network_incompatible_purposes, [Ref.string_of network]));
      | sriov :: _ ->
        let exist_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
        if not (is_device_underneath_same_type ~__context pif exist_pif) then
          raise (Api_errors.Server_error(Api_errors.network_sriov_pci_vendor_not_compatible, [Ref.string_of pif; Ref.string_of network]))
    end
