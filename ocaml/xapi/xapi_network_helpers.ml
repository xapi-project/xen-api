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
module D=Debug.Make(struct let name="xapi" end)
open D
open Xapi_pif_helpers

let is_sriov_based_network ~__context ~network =
  match Db.Network.get_PIFs ~__context ~self:network with
  | [] -> false
  | existing_pif :: _ ->
    let existing_pif_rec = Db.PIF.get_record ~__context ~self:existing_pif in
    match get_pif_topo ~__context ~pif_rec:existing_pif_rec with
    | VLAN_untagged _ :: Network_sriov_logical _ :: _
    | Network_sriov_logical _ :: _ -> true
    | _ -> false

let assert_network_compatible_with_tunnel ~__context ~network =
  if is_sriov_based_network ~__context ~network then
    raise Api_errors.(Server_error (network_incompatible_with_tunnel, [Ref.string_of network]))

let assert_network_compatible_with_bond ~__context ~network =
  if is_sriov_based_network ~__context ~network then
    raise Api_errors.(Server_error (network_incompatible_with_bond, [Ref.string_of network]))

let assert_network_compatible_with_vlan_on_bridge ~__context ~network =
  if is_sriov_based_network ~__context ~network then
    raise Api_errors.(Server_error (network_incompatible_with_vlan_on_bridge, [Ref.string_of network]))

let assert_network_compatible_with_vlan_on_sriov ~__context ~network ~sriov ~tagged_PIF =
  match Db.Network.get_PIFs ~__context ~self:network with
  | [] -> ()
  | existing_pif :: _ ->
    let existing_pif_rec = Db.PIF.get_record ~__context ~self:existing_pif in
    match get_pif_topo ~__context ~pif_rec:existing_pif_rec with
    | VLAN_untagged _ :: Network_sriov_logical existing_sriov :: _ ->
      let existing_phy_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:existing_sriov in
      let candidate_phy_pif = Db.Network_sriov.get_physical_PIF ~__context ~self:sriov in
      if not (is_device_underneath_same_type ~__context existing_phy_pif candidate_phy_pif) then
        raise Api_errors.(Server_error (network_has_incompatible_vlan_on_sriov_pifs, [Ref.string_of tagged_PIF; Ref.string_of network]))
    | _ ->
      raise Api_errors.(Server_error (network_incompatible_with_vlan_on_sriov, [Ref.string_of network]))
