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

let sriov_bring_up ~__context ~self =
  let open Network_interface in
  let update_sriov_with_result result =
    let mode, require_reboot = match result with
      | Sysfs_successful -> `sysfs, false
      | Modprobe_successful -> `modprobe, false
      | Modprobe_successful_requires_reboot -> `modprobe, true
    in
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self) in
    let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
    info "Enable network sriov on PIF %s successful, mode: %s need_reboot: %b" (Ref.string_of physical_pif) (Record_util.network_sriov_configuration_mode_to_string mode) require_reboot;
    Db.Network_sriov.set_configuration_mode ~__context ~self:sriov ~value:mode;
    Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:require_reboot;
    Db.PIF.set_currently_attached ~__context ~self ~value:(not require_reboot)
  in
  let device = Db.PIF.get_device ~__context ~self in
  begin
    match Net.Sriov.enable "enable_sriov" ~name:device with
    | Ok result -> update_sriov_with_result result
    | Error error ->
      Db.PIF.set_currently_attached ~__context ~self ~value:false;
      raise (Api_errors.(Server_error (network_sriov_enable_failed, [Ref.string_of self; error])))
  end;
  update_sriovs ~__context

let require_operation_on_pci_device ~__context ~self =
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

let sriov_bring_down ~__context ~self =
  let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self) in
  let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
  if require_operation_on_pci_device ~__context ~self then begin
    debug "Disable network sriov on pci device. PIF: %s" (Ref.string_of self);
    let open Network_interface in
    let device = Db.PIF.get_device ~__context ~self in
    match Net.Sriov.disable "disable_sriov" ~name:device with
    | Ok -> ()
    | Error error ->
      raise (Api_errors.(Server_error (network_sriov_disable_failed, [Ref.string_of self; error])))
  end;
  info "Disable network sriov on PIF %s successful" (Ref.string_of physical_pif);
  Db.PIF.set_currently_attached ~__context ~self ~value:false;
  Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
  update_sriovs ~__context

let is_device_underneath_same_type ~__context pif1 pif2 =
  let get_device_info pif =
    let pci = Db.PIF.get_PCI ~__context ~self:pif in
    Db.PCI.get_vendor_id ~__context ~self:pci, Db.PCI.get_device_id ~__context ~self:pci
  in
  (get_device_info pif1) = (get_device_info pif2)

(* SRIOV PIF can only join the network which is empty or all of the existing PIFs of it are SRIOV PIFS *)
let assert_sriov_pif_compatible_with_network ~__context ~pif ~network =
  match Db.Network.get_PIFs ~__context ~self:network with
  | [] -> ()
  | logical_pif :: _ ->
    begin
      match Db.PIF.get_sriov_logical_PIF_of ~__context ~self:logical_pif with
      | [] -> raise (Api_errors.(Server_error (network_is_not_sriov_compatible, [Ref.string_of network])))
      | sriov :: _ ->
        let existing_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
        if not (is_device_underneath_same_type ~__context pif existing_pif) then
          raise (Api_errors.(Server_error (network_has_incompatible_sriov_pifs, [Ref.string_of pif; Ref.string_of network])))
    end
