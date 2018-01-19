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

(* Any PIF should only belongs to only one of following types *)
type pif_type_t =
  | Tunnel_access of ref_tunnel
  | Bond_slave of ref_Bond
  | Bond_master of ref_Bond
  | Network_sriov_logical of ref_network_sriov
  | VLAN_master_on_physical of ref_PIF
  | VLAN_master_on_network_sriov of ref_PIF
  | Other_PIF

let get_pif_type ~__context ~self =
  (* Define a bind func to build the pipe for the monad.*)
  (* We should ensure any 2 func that used in the pipeline should not return `Some` at the same time.*)
  let (>>=) opt f =
    match opt with
    | Some _ as v -> v
    | None -> f ()
  in
  let is_tunnel_access_pif () =
    match Db.PIF.get_tunnel_access_PIF_of ~__context ~self with
    | tunnel :: _ -> Some (Tunnel_access tunnel)
    | _ -> None
  in
  let is_bond_slave_pif () =
    let bond = Db.PIF.get_bond_slave_of ~__context ~self in
    if bond <> Ref.null then Some (Bond_slave bond) else None
  in
  let is_bond_master_pif () =
    let bond = Db.PIF.get_bond_master_of ~__context ~self in
    if bond <> [] then Some (Bond_master (List.hd bond)) else None
  in
  let is_sriov_logical_pif () =
    let sriov = Db.PIF.get_sriov_logical_PIF_of ~__context ~self in
    if sriov <> [] then Some (Network_sriov_logical (List.hd sriov)) else None
  in
  let is_vlan_master_pif () =
    let vlan = Db.PIF.get_VLAN_master_of ~__context ~self in
    if vlan = Ref.null then None
    else
      let tagged_pif = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
      if Db.PIF.get_sriov_logical_PIF_of ~__context ~self:tagged_pif = [] then Some (VLAN_master_on_physical tagged_pif)
      else Some (VLAN_master_on_network_sriov tagged_pif)
  in
  match None
    >>= is_tunnel_access_pif
    >>= is_bond_slave_pif
    >>= is_bond_master_pif
    >>= is_sriov_logical_pif
    >>= is_vlan_master_pif with
  | Some x -> x
  | None -> Other_PIF

let vlan_is_allowed_on_pif ~__context ~tagged_PIF ~tag =
  if Db.PIF.get_managed ~__context ~self:tagged_PIF <> true then
    raise (Api_errors.Server_error (Api_errors.pif_unmanaged, [Ref.string_of tagged_PIF]));
  begin
    match get_pif_type ~__context ~self:tagged_PIF with
    | Bond_slave _ ->
      raise (Api_errors.Server_error (Api_errors.cannot_add_vlan_to_bond_slave, [Ref.string_of tagged_PIF]))
    | VLAN_master_on_physical _ | VLAN_master_on_network_sriov _ ->
      (* Check that the tagged PIF is not a VLAN itself - CA-25160. This check can be skipped using the allow_vlan_on_vlan FIST point. *)
      if not (Xapi_fist.allow_vlan_on_vlan()) then
        raise (Api_errors.Server_error (Api_errors.pif_is_vlan, [Ref.string_of tagged_PIF]))
    | Tunnel_access _ ->
      raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of tagged_PIF]))
    | _ -> ()
  end;

  (* Check the VLAN tag is sensible;  4095 is reserved for implementation use (802.1Q) *)
  if tag<0L || tag>4094L then
    raise (Api_errors.Server_error (Api_errors.vlan_tag_invalid, [Int64.to_string tag]));

  let vlans = Db.VLAN.get_records_where ~__context
      ~expr:(Db_filter_types.And (Db_filter_types.Eq (Db_filter_types.Field "tagged_PIF", Db_filter_types.Literal (Ref.string_of tagged_PIF)),
                                  Db_filter_types.Eq (Db_filter_types.Field "tag", Db_filter_types.Literal (Int64.to_string tag)))) in
  if vlans <> [] then begin
    let device = Db.PIF.get_device ~__context ~self:tagged_PIF in
    raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [device]));
  end

let tunnel_is_allowed_on_pif ~__context ~transport_PIF =
  if Db.PIF.get_managed ~__context ~self:transport_PIF <> true then
    raise (Api_errors.Server_error (Api_errors.pif_unmanaged, [Ref.string_of transport_PIF]));
  match get_pif_type ~__context ~self:transport_PIF with
  | Bond_slave _ ->
    raise (Api_errors.Server_error (Api_errors.cannot_add_tunnel_to_bond_slave, [Ref.string_of transport_PIF]))
  | Tunnel_access _ ->
    raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of transport_PIF]));
  | Network_sriov_logical _ ->
    raise (Api_errors.Server_error (Api_errors.cannot_add_tunnel_to_sriov_logical, [Ref.string_of transport_PIF]))
  | VLAN_master_on_network_sriov _ ->
    raise (Api_errors.Server_error (Api_errors.cannot_add_tunnel_to_vlan_on_sriov_logical, [Ref.string_of transport_PIF]))
  | _ -> ()

let bond_is_allowed_on_pif ~__context ~self =
  if Db.PIF.get_managed ~__context ~self <> true
  then raise (Api_errors.Server_error (Api_errors.pif_unmanaged, [Ref.string_of self]));
  match get_pif_type ~__context ~self with
  | Bond_slave bond ->
    let bonded = try ignore(Db.Bond.get_uuid ~__context ~self:bond); true with _ -> false in
    if bonded
    then raise (Api_errors.Server_error (Api_errors.pif_already_bonded, [ Ref.string_of self ]))
  | VLAN_master_on_physical _ | VLAN_master_on_network_sriov _ ->
    raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [ Db.PIF.get_device_name ~__context ~self] ))
  | Network_sriov_logical _ ->
    raise (Api_errors.Server_error (Api_errors.pif_is_sriov_logical, [Ref.string_of self]))
  | Tunnel_access _ ->
    raise (Api_errors.Server_error (Api_errors.is_tunnel_access_pif, [Ref.string_of self]))
  | _ -> ()

let sriov_is_allowed_on_pif ~__context ~self =
  if Db.PIF.get_physical ~__context ~self <> true then
    raise (Api_errors.Server_error (Api_errors.pif_is_not_physical, [Ref.string_of self]));
  if Db.PIF.get_sriov_physical_PIF_of ~__context ~self <> [] then
    raise (Api_errors.Server_error (Api_errors.network_sriov_already_enabled, [Ref.string_of self]));
  if not (List.mem "sriov" (Db.PIF.get_capabilities ~__context ~self)) then
    raise (Api_errors.Server_error (Api_errors.pif_is_not_sriov_capable, [Ref.string_of self]));
  ()
