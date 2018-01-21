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
open Xapi_stdext_std
module D = Debug.Make(struct let name="xapi_network_sriov" end)
open D

let get_sriov_of ~__context ~sriov_logical_pif =
  match Db.PIF.get_sriov_logical_PIF_of ~__context ~self:sriov_logical_pif with
  | v :: _ -> v
  | [] -> raise Api_errors.(Server_error (internal_error, [Printf.sprintf "Cannot find sriov object in sriov logical PIF %s" (Ref.string_of sriov_logical_pif)]))

let sriov_bring_up ~__context ~self =
  let open Network_interface in
  let update_sriov_with_result result =
    let mode, require_reboot = match result with
      | Sysfs_successful -> `sysfs, false
      | Modprobe_successful -> `modprobe, false
      | Modprobe_successful_requires_reboot -> `modprobe, true
    in
    let sriov = get_sriov_of ~__context ~sriov_logical_pif:self in
    let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
    info "Enable network sriov on PIF %s successful, mode: %s need_reboot: %b" (Ref.string_of physical_pif) (Record_util.network_sriov_configuration_mode_to_string mode) require_reboot;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:mode;
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:require_reboot;
    Db.PIF.set_currently_attached ~__context ~self ~value:(not require_reboot)
  in
  let device = Db.PIF.get_device ~__context ~self in
  let dbg = Context.string_of_task __context in
  match Net.Sriov.enable dbg ~name:device with
  | Ok result -> update_sriov_with_result result
  | Error error ->
    Db.PIF.set_currently_attached ~__context ~self ~value:false;
    raise Api_errors.(Server_error (network_sriov_enable_failed, [Ref.string_of self; error]))

let require_operation_on_pci_device ~__context ~sriov ~self =
  let is_sriov_enabled ~pif_rec =
    match pif_rec.API.pIF_sriov_logical_PIF_of with
    | [] -> false
    | sriov :: _ ->
      pif_rec.API.pIF_currently_attached || Db.Network_sriov.get_requires_reboot ~__context ~self:sriov
  in
  let pif_rec = Db.PIF.get_record ~__context ~self in
  if is_sriov_enabled ~pif_rec then begin
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
          let sriov = match pif_rec.API.pIF_sriov_logical_PIF_of with
            | v :: _ -> v
            | [] -> raise Api_errors.(Server_error (internal_error, [Printf.sprintf "Cannot find sriov object in sriov logical PIF %s" pif_rec.API.pIF_uuid]))
          in
          let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
          let pci = Db.PIF.get_PCI ~__context ~self:physical_pif in
          Db.PCI.get_driver_name ~__context ~self:pci = driver_name
        )
      |> List.filter (fun (_, pif_rec) ->
          is_sriov_enabled ~pif_rec
        )
      |> List.map (fun (pif_ref, _) -> pif_ref)
      |> (=) [self]
  end
  else false

let sriov_bring_down ~__context ~self =
  let sriov = get_sriov_of ~__context ~sriov_logical_pif:self in
  let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
  if require_operation_on_pci_device ~__context ~sriov ~self then begin
    debug "Disable network sriov on pci device. PIF: %s" (Ref.string_of self);
    let open Network_interface in
    let dbg = Context.string_of_task __context in
    let device = Db.PIF.get_device ~__context ~self in
    match Net.Sriov.disable dbg ~name:device with
    | Ok -> ()
    | Error error ->
      raise Api_errors.(Server_error (network_sriov_disable_failed, [Ref.string_of self; error]))
  end;
  info "Disable network sriov on PIF %s successful" (Ref.string_of physical_pif);
  Db.PIF.set_currently_attached ~__context ~self ~value:false;
  Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false

let is_device_underneath_same_type ~__context pif1 pif2 =
  let get_device_info pif =
    let pci = Db.PIF.get_PCI ~__context ~self:pif in
    Db.PCI.get_vendor_id ~__context ~self:pci, Db.PCI.get_device_id ~__context ~self:pci
  in
  (get_device_info pif1) = (get_device_info pif2)

(* SRIOV PIF can only join the network which is empty or all of the existing PIFs of it are SRIOV PIFS and all of them has identical PCI devices. *)
let assert_sriov_pif_compatible_with_network ~__context ~pif ~network =
  match Db.Network.get_PIFs ~__context ~self:network with
  | [] -> ()
  | logical_pif :: _ ->
    begin
      match Db.PIF.get_sriov_logical_PIF_of ~__context ~self:logical_pif with
      | [] -> raise Api_errors.(Server_error (network_is_not_sriov_compatible, [Ref.string_of network]))
      | sriov :: _ ->
        let existing_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
        if not (is_device_underneath_same_type ~__context pif existing_pif) then
          raise Api_errors.(Server_error (network_has_incompatible_sriov_pifs, [Ref.string_of pif; Ref.string_of network]))
    end

let get_remaining_capacity_on_sriov ~__context ~self =
  let physical_PIF = Db.Network_sriov.get_physical_PIF ~__context ~self in
  let pci = Db.PIF.get_PCI ~__context ~self:physical_PIF in
  Xapi_pci.get_idle_vf_nums ~__context ~self:pci

(*Returns physical PIF underlying the given PIF, return None if the given PIF is not (a VLAN on) a SR-IOV logical PIF *)
let get_underlying_pif ~__context ~pif =
  let pif_rec = Db.PIF.get_record ~__context ~self:pif in
  match Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec with
  | Network_sriov_logical sriov :: _
  | VLAN_untagged _ :: Network_sriov_logical sriov :: _ ->
    Some (Db.Network_sriov.get_physical_PIF ~__context ~self:sriov)
  | _ -> None

(* Only 2 type pif of sr-iov can be quickly up without a reboot:
   1. sysfs mode
   2. modprobe with currently_attached or unattached but has remaining-capacity(it means unplug sr-iov pif but before reboot host) 
   Used in
   * Group host by best sriov
   * Check the network is properly shared *)
let can_be_up_without_reboot ~__context sriov =
  match Db.Network_sriov.get_configuration_mode ~__context ~self:sriov with
  | `sysfs -> true
  | `unknown -> false
  | `modprobe ->
    let pif = Db.Network_sriov.get_logical_PIF ~__context ~self:sriov in
    Db.PIF.get_currently_attached ~__context ~self:pif || get_remaining_capacity_on_sriov ~__context ~self:sriov > 0L

(* Just take one pif from the network and check if it has an underlying_pif, if so it's a SR-IOV network. 
Note, get_underlying_pif only matches a (VLAN on) SR-IOV type of PIF. *)
let is_sriov_network ~__context ~self =
  match Db.Network.get_PIFs ~__context ~self with
  | [] -> raise Api_errors.(Server_error (internal_error, ["Cannot find PIF in Network"]))
  | pif :: _ ->
    begin match get_underlying_pif ~__context ~pif with
      | Some _ -> true
      | None -> false
    end

let get_sriov_networks_from_vm ~__context ~vm =
  let networks = Db.VM.get_VIFs ~__context ~self:vm |> List.map (fun vif -> Db.VIF.get_network ~__context ~self:vif ) in
  List.filter (fun network -> is_sriov_network ~__context ~self:network) networks

(* Get localhost underlying pif with for the sr-iov network *)
let get_local_underlying_pif ~__context ~network ~host =
  match Xapi_network_attach_helpers.get_local_pifs ~__context ~network ~host with
    | [] -> raise Api_errors.(Server_error (internal_error, ["Cannot get local pif on network"]))
    | pif :: _ ->
      begin match get_underlying_pif ~__context ~pif with
      | Some pif -> pif
      | None -> raise Api_errors.(Server_error (internal_error, ["Cannot get underlying pif on sriov network"]))
      end

(* Get remaining capacity for localhost on the given network, return None if no underlying_pif found or capacity = 0L *)
let get_remaining_capacity_on_localhost ~__context ~host ~network =
  let local_pifs = Xapi_network_attach_helpers.get_local_pifs ~__context ~network ~host in
  match local_pifs with
  | [] -> raise Api_errors.(Server_error (internal_error, ["Cannot get local pif on network"]))
  | local_pif :: _ ->
    match get_underlying_pif ~__context ~pif:local_pif with
    | Some underlying_pif ->
      let pci = Db.PIF.get_PCI ~__context ~self:underlying_pif in
      Xapi_pci.get_idle_vf_nums ~__context ~self:pci
    | None -> raise Api_errors.(Server_error (internal_error, ["Cannot get underlying pif on sriov network"]))

(* Partition hosts by attached and unattached pifs, the network input is a SR-IOV type.
  1.For attached pifs, check the free capacity > 0   
  2.For unattached pifs,check the sriov on pif can be attached without reboot when vm start
  3.Group host by free capacity,finally returns the host list list like [ [(host0,num0);(host1,num0)];[(host2;num1);(host3,num1)]... ] 
  4.Since before plug the unattached_pif, vf capacity on pif is unknown,so used vf_num = 0L
  5.If unattached_hosts not empty then add at the end of host lists.
 *)
let group_hosts_by_best_sriov ~__context ~network =
  let pifs = Db.Network.get_PIFs ~__context ~self:network in
  let attached_hosts, unattached_hosts = List.fold_left (fun (l1, l2) pif ->
    let host = Db.PIF.get_host ~__context ~self:pif in
    if Db.PIF.get_currently_attached ~__context ~self:pif then begin
      let num = get_remaining_capacity_on_localhost ~__context ~host ~network in
      if num = 0L then (l1,l2) else ((host, num) :: l1, l2)
    end else begin
      let sriov = get_sriov_of ~__context ~sriov_logical_pif:pif in
      if can_be_up_without_reboot ~__context sriov then (l1, (host, 0L) :: l2)
      else (l1, l2)
    end
    ) ([], []) pifs
  in
  let host_lists =
    Helpers.group_by `descending (fun ( _ , num ) -> num) (Listext.List.setify attached_hosts)
    |> List.map (fun hl -> List.map (fun ( (h, num), _ ) -> h, num) hl)
  in
  if unattached_hosts <> [] then 
    host_lists @ [unattached_hosts]
  else host_lists

(* If exn happens during vifs reservation ,reserved vfs will be cleared *)
let reserve_sriov_vfs ~__context ~host ~vm =
  let vifs = Db.VM.get_VIFs ~__context ~self:vm in
  List.iter (fun vif ->
      let network = Db.VIF.get_network ~__context ~self:vif in
      let pif =  get_local_underlying_pif ~__context ~network ~host in
      let pci = Db.PIF.get_PCI ~__context ~self:pif in
      match Pciops.reserve_free_virtual_function ~__context vm pci with
      | Some vf -> Db.VIF.set_reserved_pci ~__context ~self:vif ~value:vf
      | None -> raise Api_errors.(Server_error (internal_error, ["No free virtual function found"]))
  ) vifs


