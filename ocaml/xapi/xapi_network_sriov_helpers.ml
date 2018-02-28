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
  begin
    let dbg = Context.string_of_task __context in
    match Net.Sriov.enable dbg ~name:device with
    | Ok result -> update_sriov_with_result result
    | Error error ->
      Db.PIF.set_currently_attached ~__context ~self ~value:false;
      raise Api_errors.(Server_error (network_sriov_enable_failed, [Ref.string_of self; error]))
  end;
  Xapi_pci.update_pcis ~__context

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
  Db.Network_sriov.set_requires_reboot ~__context ~self:sriov ~value:false;
  Xapi_pci.update_pcis ~__context
