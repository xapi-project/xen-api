(*
 * Copyright (C) Citrix Systems Inc.
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
open API
module D=Debug.Make(struct let name="xapi_pif_helpers" end)
open D

(* Any given PIF should belongs only one of the following types *)
type pif_type_t =
  | Tunnel_access of ref_tunnel
  | VLAN_untagged of ref_VLAN
  | Network_sriov_logical of ref_network_sriov
  | Bond_master of ref_Bond
  | Physical of pIF_t

let pif_type_to_string = function
  | Tunnel_access _ -> "Tunnel_access"
  | VLAN_untagged _ -> "VLAN_untagged"
  | Network_sriov_logical _ -> "Network_sriov_logical"
  | Bond_master _ -> "Bond_master"
  | Physical _ -> "Physical"

let is_tunnel_access_pif pif_rec =
  match pif_rec.API.pIF_tunnel_access_PIF_of with
  | tunnel :: _ -> Some (Tunnel_access tunnel)
  | _ -> None

let is_vlan_master_pif pif_rec =
  let vlan = pif_rec.API.pIF_VLAN_master_of in
  if vlan = Ref.null then None else Some (VLAN_untagged vlan)

let is_sriov_logical_pif pif_rec =
  match pif_rec.API.pIF_sriov_logical_PIF_of with
  | sriov :: _ -> Some (Network_sriov_logical sriov)
  | _ -> None

let is_bond_master_pif pif_rec =
  match pif_rec.API.pIF_bond_master_of with
  | bond :: _ -> Some (Bond_master bond)
  | _ -> None

let is_physical_pif pif_rec =
  if pif_rec.API.pIF_physical then Some (Physical pif_rec) else None

let (>>=) (ret, pif_rec) f =
  match ret, pif_rec with
  | Some _ as v, _ -> v, pif_rec
  | None, _ -> f pif_rec, pif_rec

let get_pif_type pif_rec =
  match (None, pif_rec)
    >>= is_tunnel_access_pif
    >>= is_vlan_master_pif
    >>= is_sriov_logical_pif
    >>= is_bond_master_pif
    >>= is_physical_pif
  with
  | Some v, _ -> v
  | None, _ -> raise Api_errors.(Server_error (internal_error, [Printf.sprintf "Cannot calculate PIF type of %s" pif_rec.API.pIF_uuid]))

(** This function aims to get a list of types of the PIFs underneath the given PIF *)
(* The root PIF underneath should be Physical or Bond_master *)
let get_pif_topo ~__context ~pif_rec =
  let rec get_pif_type_till_root ret pif_rec =
    let pif_t = get_pif_type pif_rec in
    match pif_t with
    | Tunnel_access tunnel ->
      let tunnel_rec = Db.Tunnel.get_record ~__context ~self:tunnel in
      let pif_ref = tunnel_rec.API.tunnel_transport_PIF in
      let pif_rec = Db.PIF.get_record ~__context ~self:pif_ref in
      get_pif_type_till_root (pif_t :: ret) pif_rec
    | VLAN_untagged vlan ->
      let vlan_rec = Db.VLAN.get_record ~__context ~self:vlan in
      let pif_ref = vlan_rec.API.vLAN_tagged_PIF in
      let pif_rec = Db.PIF.get_record ~__context ~self:pif_ref in
      get_pif_type_till_root (pif_t :: ret) pif_rec
    | Network_sriov_logical sriov ->
      let sriov_rec = Db.Network_sriov.get_record ~__context ~self:sriov in
      let pif_ref = sriov_rec.API.network_sriov_physical_PIF in
      let pif_rec = Db.PIF.get_record ~__context ~self:pif_ref in
      get_pif_type_till_root (pif_t :: ret) pif_rec
    | Bond_master _
    | Physical _ ->
      pif_t :: ret
  in
  let pif_t_list = get_pif_type_till_root [] pif_rec in
  let pif_t_list = List.rev pif_t_list in
  pif_t_list

let vlan_is_allowed_on_pif ~__context ~tagged_PIF ~pif_rec ~pif_topo ~tag =
  match pif_topo with
  | Physical pif_rec :: _ when pif_rec.API.pIF_bond_slave_of <> Ref.null ->
    (* Disallow creating on bond slave *)
    (* Here we rely on the implementation to guarantee that `Physical` is a terminating case *)
    raise Api_errors.(Server_error (cannot_add_vlan_to_bond_slave, [Ref.string_of tagged_PIF]))
  | VLAN_untagged _ :: _ ->
    raise Api_errors.(Server_error (pif_is_vlan, [Ref.string_of tagged_PIF]))
  | Tunnel_access _ :: _ ->
    raise Api_errors.(Server_error (is_tunnel_access_pif, [Ref.string_of tagged_PIF]))
  | _ -> ()

let tunnel_is_allowed_on_pif ~__context ~transport_PIF =
  let pif_rec = Db.PIF.get_record ~__context ~self:transport_PIF in
  match get_pif_topo ~__context ~pif_rec with
  | Physical pif_rec :: _ when pif_rec.API.pIF_bond_slave_of <> Ref.null ->
    (* Disallow creating on bond slave *)
    (* Here we rely on the implementation to guarantee that `Physical` is a terminating case *)
    raise Api_errors.(Server_error (cannot_add_tunnel_to_bond_slave, [Ref.string_of transport_PIF]))
  | Tunnel_access _ :: _ ->
    raise Api_errors.(Server_error (is_tunnel_access_pif, [Ref.string_of transport_PIF]));
  | Network_sriov_logical _ :: _ ->
    raise Api_errors.(Server_error (cannot_add_tunnel_to_sriov_logical, [Ref.string_of transport_PIF]))
  | VLAN_untagged _ :: Network_sriov_logical _ :: _ ->
      raise Api_errors.(Server_error (cannot_add_tunnel_to_vlan_on_sriov_logical, [Ref.string_of transport_PIF]))
  | _ -> ()

let bond_is_allowed_on_pif ~__context ~self =
  let pif_rec = Db.PIF.get_record ~__context ~self in
  match get_pif_topo ~__context ~pif_rec with
  | Physical pif_rec :: _ when pif_rec.API.pIF_bond_slave_of <> Ref.null ->
    (* Disallow creating on bond slave *)
    (* Here we rely on the implementation to guarantee that `Physical` is a terminating case *)
    let bond = pif_rec.API.pIF_bond_slave_of in
    let bonded = try ignore(Db.Bond.get_uuid ~__context ~self:bond); true with _ -> false in
    if bonded
    then raise Api_errors.(Server_error (pif_already_bonded, [ Ref.string_of self ]))
  | VLAN_untagged _ :: _ ->
    raise Api_errors.(Server_error (pif_vlan_exists, [Db.PIF.get_device_name ~__context ~self]))
  | Tunnel_access _ :: _ ->
    raise Api_errors.(Server_error (is_tunnel_access_pif, [Ref.string_of self]))
  | Network_sriov_logical _ :: _ ->
    raise Api_errors.(Server_error (pif_is_sriov_logical, [Ref.string_of self]))
  | _ -> ()

let sriov_is_allowed_on_pif ~__context ~physical_PIF ~pif_rec =
  let _ = match get_pif_type pif_rec with
    | Physical _ -> ()
    | _ ->
      raise Api_errors.(Server_error (pif_is_not_physical, [Ref.string_of physical_PIF]))
  in
  if pif_rec.API.pIF_sriov_physical_PIF_of <> [] then
    raise Api_errors.(Server_error (network_sriov_already_enabled, [Ref.string_of physical_PIF]));
  if not (List.mem "sriov" pif_rec.API.pIF_capabilities) then
    raise Api_errors.(Server_error (pif_is_not_sriov_capable, [Ref.string_of physical_PIF]))

let assert_pif_is_managed ~__context ~self =
  if Db.PIF.get_managed ~__context ~self <> true then
    raise Api_errors.(Server_error (pif_unmanaged, [Ref.string_of self]))

let assert_not_vlan_slave ~__context ~self =
  let vlans = Db.PIF.get_VLAN_slave_of ~__context ~self in
  debug "PIF %s assert_no_vlans = [ %s ]"
    (Db.PIF.get_uuid ~__context ~self)
    (String.concat "; " (List.map Ref.string_of vlans));
  if vlans <> []
  then begin
    List.map (fun self -> Db.VLAN.get_uuid ~__context ~self) vlans
    |> String.concat "; "
    |> debug "PIF has associated VLANs: [ %s ]";
    raise Api_errors.(Server_error
             (pif_vlan_still_exists,
              [ Ref.string_of self ]))
  end

let is_device_underneath_same_type ~__context pif1 pif2 =
  let get_device_info pif =
    let pci = Db.PIF.get_PCI ~__context ~self:pif in
    let pci_rec = Db.PCI.get_record_internal ~__context ~self:pci in
    pci_rec.Db_actions.pCI_vendor_id, pci_rec.Db_actions.pCI_device_id
  in
  (get_device_info pif1) = (get_device_info pif2)
