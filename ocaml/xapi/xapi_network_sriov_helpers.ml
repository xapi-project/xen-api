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

open Stdext
open Listext
open Threadext
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
  Mutex.execute mutex (fun () ->
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

let error_handler ~__context ~pif ~error =
  let open Network_interface in
  match error with
  | Device_not_found ->
    let device = Db.PIF.get_device ~__context ~self:pif in
    raise (Api_errors.Server_error (Api_errors.network_sriov_device_not_found, [Ref.string_of pif; device]))
  | Bus_out_of_range -> raise (Api_errors.Server_error (Api_errors.network_sriov_bus_out_of_range, [Ref.string_of pif]))
  | Not_enough_mmio_resources -> raise (Api_errors.Server_error (Api_errors.network_sriov_not_enough_mmio_resources, [Ref.string_of pif]))
  | Unknown msg -> raise (Api_errors.Server_error (Api_errors.network_sriov_unknown_error, [Ref.string_of pif; msg]))

let sriov_bring_up ~__context ~self =
  let open Network_interface in
  let update_sriov_with_result result =
    let mode, require_reboot = match result with
      | Sysfs_successful -> `sysfs, false
      | Modprobe_successful -> `modprobe, false
      | Modprobe_successful_requires_reboot -> `modprobe, true
      | _ -> raise (Api_errors.Server_error (Api_errors.network_sriov_enable_failed, [Ref.string_of self; "unable to enable sriov, but does not get error from networkd"]))
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
      error_handler ~__context ~pif:self ~error
  end;
  update_sriovs ~__context

let need_operate_pci_device ~__context ~self =
  let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self) in
  match Db.Network_sriov.get_configuration_mode ~__context ~self:sriov with
  | `sysfs | `unknown -> true
  | `modprobe ->
    let host = Db.PIF.get_host ~__context ~self in
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self) in
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
        let sriov = List.hd pif_rec.API.pIF_sriov_logical_PIF_of in
        let requires_reboot = Db.Network_sriov.get_requires_reboot ~__context ~self:sriov in
        pif_rec.API.pIF_currently_attached = true || requires_reboot
      )
    |> List.length
    |> (=) 1

let sriov_bring_down ~__context ~self =
  let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self) in
  let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
  assert_no_vf_inuse ~__context ~pif:physical_pif;
  if need_operate_pci_device ~__context ~self then begin
    debug "Disable network sriov on pci device. PIF: %s" (Ref.string_of self);
    let open Network_interface in
    let handle_result = function
      | Disable_successful -> ()
      | _ ->
        raise (Api_errors.Server_error (Api_errors.network_sriov_disable_failed, [Ref.string_of self; "unable to disable sriov, but does not get error from networkd"]))
    in
    let device = Db.PIF.get_device ~__context ~self in
    match Net.Sriov.disable "disable_sriov" ~name:device with
    | Ok result -> handle_result result
    | Error error -> error_handler ~__context ~pif:self ~error
  end;

  let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self) in
  let physical_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
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

let assert_sriov_pif_compatible_with_network ~__context ~pif ~network =
  match Db.Network.get_PIFs ~__context ~self:network with
  | [] -> ()
  | logical_pif :: _ ->
    let sriov = List.hd (Db.PIF.get_sriov_logical_PIF_of ~__context ~self:logical_pif) in
    if sriov = Ref.null then raise (Api_errors.Server_error(Api_errors.network_incompatible_purposes, [Ref.string_of network]));
    let exist_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
    if not (is_device_underneath_same_type ~__context pif exist_pif) then
      raise (Api_errors.Server_error(Api_errors.network_sriov_pci_vendor_not_compatible, [Ref.string_of pif; Ref.string_of network]))

let get_remaining_capacity_on_sriov ~__context ~self =
  let physical_PIF = Db.Network_sriov.get_physical_PIF ~__context ~self in
  let pci = Db.PIF.get_PCI ~__context ~self:physical_PIF in
  Xapi_pci.get_idle_vf_nums ~__context ~self:pci
  |> Int64.of_int

let rec get_underlying_pif ~__context ~pif =
  match Db.PIF.get_sriov_logical_PIF_of ~__context ~self:pif with
  | [] ->
    let vlan = Db.PIF.get_VLAN_master_of ~__context ~self:pif in
    if vlan = Ref.null then None
    else begin
      let tagged_pif = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
      get_underlying_pif ~__context ~pif:tagged_pif
    end
  | sriov :: _ ->
    Some (Db.Network_sriov.get_physical_PIF ~__context ~self:sriov)

let is_sriov_network ~__context ~self =
  let pifs = Db.Network.get_PIFs ~__context ~self in
  match List.filter_map (fun pif -> get_underlying_pif ~__context ~pif) pifs with
  | [] -> false
  | _ -> true

let get_sriov_networks_from_vm ~__context ~vm =
  let networks = Db.VM.get_VIFs ~__context ~self:vm |> List.map (fun vif -> Db.VIF.get_network ~__context ~self:vif ) in
  List.filter (fun network -> is_sriov_network ~__context ~self:network) networks

let get_pcis_from_network ~__context ~network =
  Db.Network.get_PIFs ~__context ~self:network
  |> List.filter_map (fun pif -> get_underlying_pif ~__context ~pif)
  |> List.map (fun pif -> Db.PIF.get_PCI ~__context ~self:pif)

let get_remaining_on_network ~__context ~network =
  try
    let pcis = get_pcis_from_network ~__context ~network in
    let capacity = List.fold_left
        (fun acc pci ->
            let remaining_capacity = Xapi_pci.get_idle_vf_nums ~__context ~self:pci |> Int64.of_int in
            Int64.add acc remaining_capacity
        ) 0L pcis
    in
    if capacity > 0L
    then Either.Right capacity
    else Either.Left (Api_errors.Server_error(Api_errors.network_sriov_insufficient_vfs, []))
  with e -> Either.Left e

let host_sriov_capable ~__context ~host ~network =
  (*at most one pif in network on localhost*)
  let local_pifs =
    Db.PIF.get_refs_where ~__context ~expr:(And (
      Eq (Field "network", Literal (Ref.string_of network)),
      Eq (Field "host", Literal (Ref.string_of host))
    )) in
  match local_pifs with
  | local_pif :: _ ->
    begin match get_underlying_pif ~__context ~pif:local_pif with
      | Some underlying_pif ->
        let pci = Db.PIF.get_PCI ~__context ~self:underlying_pif in
        if Xapi_pci.get_idle_vf_nums ~__context ~self:pci > 0 then true else false
      | None -> false
    end
  | [] -> false

let group_hosts_by_best_sriov ~__context ~network =
  let pcis = get_pcis_from_network __context network in
  let can_allocate_vf pci = Xapi_pci.get_idle_vf_nums ~__context ~self:pci |> Int64.of_int > 0L in
  let valid_pcis = List.filter can_allocate_vf pcis in
  (* group hosts by breadth-first algorithm *)
  Helpers.group_by `descending
    (fun host ->
        let pci = List.find (fun pci -> host = Db.PCI.get_host ~__context ~self:pci) valid_pcis in
        Xapi_pci.get_idle_vf_nums ~__context ~self:pci |> Int64.of_int
    ) (List.map (fun pci -> Db.PCI.get_host ~__context ~self:pci) valid_pcis |> List.setify)

let assert_capacity_for_vf ~__context ~pci =
  match Xapi_pci.get_idle_vf_nums ~__context ~self:pci with
  | 0 -> raise (Api_errors.Server_error(Api_errors.network_sriov_insufficient_vfs, []))
  | _ -> ()

let choose_pf_for_vf ~__context ~host ~vif =
  let network = Db.VIF.get_network ~__context ~self:vif in
  (* at most one pif within one network on localhost*)
  let host_pcis = get_pcis_from_network __context network |>
                  List.filter (fun pci -> Db.PCI.get_host ~__context ~self:pci = host) in
  match host_pcis with
  | pci :: t -> assert_capacity_for_vf ~__context ~pci; pci
  | [] -> raise (Api_errors.Server_error(Api_errors.vm_requires_vf_pci_for_sriov_vif, [Ref.string_of vif]))

(* If exn happens during vifs reservation ,reserved vfs will be cleared*)
let reserve_sriov_vfs ~__context ~host ~vm =
  let sriov_networks = get_sriov_networks_from_vm __context vm in
  let sriov_vifs = Db.VM.get_VIFs ~__context ~self:vm
    |> List.filter (fun vif ->
        List.mem (Db.VIF.get_network ~__context ~self:vif) sriov_networks) in
  List.iter (fun vif ->
    let vf = choose_pf_for_vf ~__context ~host ~vif |>
              Pciops.reserve_free_virtual_function ~__context vm in
    match vf with
    | Some vf -> Db.VIF.set_reserved_pci ~__context ~self:vif ~value:vf
    | None -> raise Api_errors.(Server_error (internal_error, ["No free virtual function found"]))
  ) sriov_vifs

