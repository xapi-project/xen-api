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

(* Network object relations
   tunnel : vlan
          | PHYSICAL
          | BOND_MASTER

   vlan : vlan
        | sriov
        | BOND_MASTER
        | PHYSICAL

   sriov : BOND_SLAVE
         | PHYSICAL
 *)

type pif_type_t =
  | Tunnel of ref_tunnel
  | VLAN of ref_VLAN
  | Network_sriov of ref_network_sriov
  | Bond_master of ref_Bond
  | Bond_slave of ref_Bond
  | Physical of pIF_t

let pif_type_to_string = function
  | Tunnel _ -> "Tunnel"
  | VLAN _ -> "VLAN"
  | Network_sriov _ -> "Network_sriov"
  | Bond_master _ -> "Bond_master"
  | Bond_slave _ -> "Bond_slave"
  | Physical _ -> "Physical"

let is_tunnel_access_pif pif_rec =
  match pif_rec.API.pIF_tunnel_access_PIF_of with
  | tunnel :: _ -> Some (Tunnel tunnel)
  | _ -> None

let is_vlan_master_pif pif_rec =
  let vlan = pif_rec.API.pIF_VLAN_master_of in
  if vlan = Ref.null then None else Some (VLAN vlan)

let is_sriov_logical_pif pif_rec =
  match pif_rec.API.pIF_sriov_logical_PIF_of with
  | sriov :: _ -> Some (Network_sriov sriov)
  | _ -> None

let is_bond_master_pif pif_rec =
  match pif_rec.API.pIF_bond_master_of with
  | bond :: _ -> Some (Bond_master bond)
  | _ -> None

let is_bond_slave_pif pif_rec =
  let bond = pif_rec.API.pIF_bond_slave_of in
  if bond = Ref.null then None else Some (Bond_slave bond)

let is_physical_pif pif_rec =
  if pif_rec.API.pIF_physical then Some (Physical pif_rec) else None

let (>>=) (opt, pif_rec) f =
  match opt, pif_rec with
  | Some _ as v, _ -> v, pif_rec
  | None, _ -> f pif_rec, pif_rec

let get_pif_type_under_tunnel ~__context ~pif_rec =
  match (None, pif_rec)
    >>= is_vlan_master_pif
    >>= is_bond_master_pif
    >>= is_physical_pif
  with
  | Some v, _ -> v
  | None, _ -> raise (Api_errors.Server_error (Api_errors.unknown_pif_type, [pif_rec.API.pIF_uuid]))

let get_pif_type_under_vlan ~__context ~pif_rec =
  match (None, pif_rec)
    >>= is_vlan_master_pif
    >>= is_sriov_logical_pif
    >>= is_bond_master_pif
    >>= is_physical_pif
  with
  | Some v, _ -> v
  | None, _ -> raise (Api_errors.Server_error (Api_errors.unknown_pif_type, [pif_rec.API.pIF_uuid]))

let get_pif_type_under_sriov ~__context ~pif_rec =
  match (None, pif_rec)
    >>= is_bond_slave_pif
    >>= is_physical_pif
  with
  | Some v, _ -> v
  | None, _ -> raise (Api_errors.Server_error (Api_errors.unknown_pif_type, [pif_rec.API.pIF_uuid]))

let get_pif_type ~__context ~pif_rec =
  let rec get_pif_type_till_bottom ret pif_t =
    match pif_t with
    | Tunnel tunnel ->
      let tunnel_rec = Db.Tunnel.get_record ~__context ~self:tunnel in
      let pif_ref = tunnel_rec.API.tunnel_transport_PIF in
      let pif_rec = Db.PIF.get_record ~__context ~self:pif_ref in
      let x = get_pif_type_under_tunnel ~__context ~pif_rec in
      get_pif_type_till_bottom (pif_t::ret) x
    | VLAN vlan ->
      let vlan_rec = Db.VLAN.get_record ~__context ~self:vlan in
      let pif_ref = vlan_rec.API.vLAN_tagged_PIF in
      let pif_rec = Db.PIF.get_record ~__context ~self:pif_ref in
      let x = get_pif_type_under_vlan ~__context ~pif_rec in
      get_pif_type_till_bottom (pif_t::ret) x
    | Network_sriov sriov ->
      let sriov_rec = Db.Network_sriov.get_record ~__context ~self:sriov in
      let pif_ref = sriov_rec.API.network_sriov_physical_PIF in
      let pif_rec = Db.PIF.get_record ~__context ~self:pif_ref in
      let x = get_pif_type_under_sriov ~__context ~pif_rec in
      get_pif_type_till_bottom (pif_t::ret) x
    | Bond_slave _
    | Bond_master _
    | Physical _ ->
      pif_t :: ret
  in
  match (None, pif_rec)
    >>= is_tunnel_access_pif
    >>= is_vlan_master_pif
    >>= is_sriov_logical_pif
    >>= is_bond_master_pif
    >>= is_bond_slave_pif
    >>= is_physical_pif
  with
  | Some v, _ ->
    let ret = List.rev (get_pif_type_till_bottom [] v) in
    debug "PIF type of %s is: %s" pif_rec.API.pIF_uuid (String.concat " " (List.map pif_type_to_string ret));
    ret
  | None, _ -> raise (Api_errors.Server_error (Api_errors.unknown_pif_type, [pif_rec.API.pIF_uuid]))

let vlan_is_allowed_on_pif ~__context ~tagged_PIF ~tag =
  let pif_rec = Db.PIF.get_record ~__context ~self:tagged_PIF in
  match get_pif_type ~__context ~pif_rec with
  | Bond_slave _ :: _ ->
    raise (Api_errors.Server_error (Api_errors.cannot_add_vlan_to_bond_slave, [Ref.string_of tagged_PIF]))
  | VLAN _ :: _ ->
    (* Check that the tagged PIF is not a VLAN itself - CA-25160. This check can be skipped using the allow_vlan_on_vlan FIST point. *)
    if not (Xapi_fist.allow_vlan_on_vlan()) then
      raise (Api_errors.Server_error (Api_errors.pif_is_vlan, [Ref.string_of tagged_PIF]))
  | Tunnel _ :: _ ->
    raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of tagged_PIF]))
  | _ -> ()

let tunnel_is_allowed_on_pif ~__context ~transport_PIF =
  let pif_rec = Db.PIF.get_record ~__context ~self:transport_PIF in
  match get_pif_type ~__context ~pif_rec with
  | Bond_slave _ :: _ ->
    raise (Api_errors.Server_error (Api_errors.cannot_add_tunnel_to_bond_slave, [Ref.string_of transport_PIF]))
  | Tunnel _ :: _ ->
    raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of transport_PIF]));
  | Network_sriov _ :: _ ->
    raise (Api_errors.Server_error (Api_errors.cannot_add_tunnel_to_sriov_logical, [Ref.string_of transport_PIF]))
  | VLAN _ :: tl when List.exists (fun x -> match x with Network_sriov _ -> true | _ -> false) tl ->
      raise (Api_errors.Server_error (Api_errors.cannot_add_tunnel_to_vlan_on_sriov_logical, [Ref.string_of transport_PIF]))
  | _ -> ()

let bond_is_allowed_on_pif ~__context ~self =
  let pif_rec = Db.PIF.get_record ~__context ~self in
  match get_pif_type ~__context ~pif_rec with
  | Bond_slave bond :: _ ->
    let bonded = try ignore(Db.Bond.get_uuid ~__context ~self:bond); true with _ -> false in
    if bonded
    then raise (Api_errors.Server_error (Api_errors.pif_already_bonded, [ Ref.string_of self ]))
  | VLAN _ :: _ ->
    raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [ Db.PIF.get_device_name ~__context ~self] ))
  | Network_sriov _ :: _ ->
    raise (Api_errors.Server_error (Api_errors.pif_is_sriov_logical, [Ref.string_of self]))
  | Tunnel _ :: _ ->
    raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of self]))
  | _ -> ()

let sriov_is_allowed_on_pif ~__context ~self =
  let pif_rec = Db.PIF.get_record ~__context ~self in
  let _ = match get_pif_type ~__context ~pif_rec with
    | Bond_slave _ :: _
    | Physical _ :: _ -> ()
    | _ ->
      raise (Api_errors.Server_error (Api_errors.pif_is_not_physical, [Ref.string_of self]))
  in
  if Db.PIF.get_sriov_physical_PIF_of ~__context ~self <> [] then
    raise (Api_errors.Server_error (Api_errors.network_sriov_already_enabled, [Ref.string_of self]));
  if not (List.mem "sriov" (Db.PIF.get_capabilities ~__context ~self)) then
    raise (Api_errors.Server_error (Api_errors.pif_is_not_sriov_capable, [Ref.string_of self]));
  ()

let assert_pif_is_managed ~__context ~self =
  if Db.PIF.get_managed ~__context ~self <> true then
    raise (Api_errors.Server_error (Api_errors.pif_unmanaged, [Ref.string_of self]))
