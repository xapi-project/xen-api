(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
module D = Debug.Make(struct let name="xapi" end)
open D

(* Dummy MAC used by the VLAN *)
let vlan_mac = "fe:ff:ff:ff:ff:ff"

let pool_introduce ~__context ~tagged_PIF ~untagged_PIF ~tag ~other_config =
  let vlan = Ref.make () in
  let vlan_uuid = Uuid.to_string (Uuid.make_uuid ()) in
  let () = Db.VLAN.create ~__context ~ref:vlan ~uuid:vlan_uuid ~tagged_PIF ~untagged_PIF ~tag ~other_config in
  (* Untagged PIF going to be VLAN_master_of above VLAN *)
  Db.PIF.set_VLAN_master_of ~__context ~self:untagged_PIF ~value:vlan;
  vlan

let create_internal ~__context ~host ~tagged_PIF ~tag ~network ~device =
  let vlan = Ref.make () and vlan_uuid = Uuid.to_string (Uuid.make_uuid ()) in
  let untagged_PIF = Ref.make () in
  (* Copy the MTU and metrics from the base PIF *)
  let mTU = Db.PIF.get_MTU ~__context ~self:tagged_PIF in
  let metrics = Db.PIF.get_metrics ~__context ~self:tagged_PIF in
  Db.PIF.create ~__context ~ref:untagged_PIF ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
    ~device ~device_name:device ~network ~host ~mAC:vlan_mac ~mTU ~vLAN:tag ~metrics
    ~physical:false ~currently_attached:false ~igmp_snooping_status:`unknown
    ~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:"" ~dNS:"" ~bond_slave_of:Ref.null
    ~vLAN_master_of:vlan ~management:false
    ~other_config:[] ~disallow_unplug:false
    ~ipv6_configuration_mode:`None ~iPv6:[""] ~ipv6_gateway:"" ~primary_address_type:`IPv4 ~managed:true
    ~properties:[] ~capabilities:[] ~pCI:Ref.null;

  let () = Db.VLAN.create ~__context ~ref:vlan ~uuid:vlan_uuid ~tagged_PIF ~untagged_PIF ~tag ~other_config:[] in
  vlan, untagged_PIF

let create ~__context ~tagged_PIF ~tag ~network =
  Xapi_network.assert_network_is_managed ~__context ~self:network;
  let host = Db.PIF.get_host ~__context ~self:tagged_PIF in
  Xapi_pif.assert_no_other_local_pifs ~__context ~host ~network;
  Xapi_pif_helpers.assert_pif_is_managed ~__context ~self:tagged_PIF;
  let pif_rec = Db.PIF.get_record ~__context ~self:tagged_PIF in
  let pif_topo = Xapi_pif_helpers.get_pif_topo ~__context ~pif_rec in
  Xapi_pif_helpers.vlan_is_allowed_on_pif ~__context ~tagged_PIF ~pif_rec ~pif_topo ~tag;
  Xapi_network_helpers.assert_vlan_network_compatible_with_pif ~__context ~network ~tagged_PIF ~pif_topo;
  (* Check the VLAN tag is sensible;  4095 is reserved for implementation use (802.1Q) *)
  if tag<0L || tag>4094L then
    raise (Api_errors.Server_error (Api_errors.vlan_tag_invalid, [Int64.to_string tag]));

  let device = pif_rec.API.pIF_device in
  let vlans = Db.VLAN.get_records_where ~__context
      ~expr:(Db_filter_types.And (Db_filter_types.Eq (Db_filter_types.Field "tagged_PIF", Db_filter_types.Literal (Ref.string_of tagged_PIF)),
                                  Db_filter_types.Eq (Db_filter_types.Field "tag", Db_filter_types.Literal (Int64.to_string tag)))) in
  if vlans <> [] then
    raise (Api_errors.Server_error (Api_errors.pif_vlan_exists, [device]));

  (* Check the VLAN is not in use by the kernel *)
  let open Network in
  if Net.Interface.has_vlan (Context.string_of_task __context) device (Int64.to_int tag) then
    raise (Api_errors.Server_error (Api_errors.vlan_in_use, [device; Int64.to_string tag]));

  let vlan, untagged_PIF = create_internal ~__context ~host ~tagged_PIF ~tag ~network ~device in
  Xapi_pif.plug ~__context ~self:untagged_PIF;
  vlan

let destroy ~__context ~self =
  debug "VLAN.destroy uuid = %s" (Db.VLAN.get_uuid ~__context ~self);
  let untagged_PIF = Db.VLAN.get_untagged_PIF ~__context ~self in
  (* Check if the untagged_PIF exists, if not we must be an orphaned record *)
  if try ignore(Db.PIF.get_uuid ~__context ~self:untagged_PIF); false with _ -> true then begin
    warn "VLAN's untagged PIF doesn't exist -- orphaned record?";
    Db.VLAN.destroy ~__context ~self
  end else begin
    debug "untagged PIF uuid = %s" (Db.PIF.get_uuid ~__context ~self:untagged_PIF);
    (* Side-effect of this is to destroy any VLAN object *)
    Xapi_pif.assert_not_in_bond ~__context ~self:untagged_PIF;
    Xapi_pif.assert_not_slave_management_pif ~__context ~self:untagged_PIF;
    Xapi_pif.assert_no_protection_enabled ~__context ~self:untagged_PIF;

    if Db.PIF.get_VLAN ~__context ~self:untagged_PIF < 0L then
      raise (Api_errors.Server_error (Api_errors.pif_is_physical, []));
    (* Because of the precondition in create_VLAN, this will always be the only PIF
       		   connecting this host to the network. Therefore it is safe to detach the network. *)
    let network = Db.PIF.get_network ~__context ~self:untagged_PIF in
    let bridge = Db.Network.get_bridge ~__context ~self:network in
    let managed = Db.Network.get_managed ~__context ~self:network in
    Xapi_pif.unplug ~__context ~self:untagged_PIF;

    Xapi_network.detach ~__context ~bridge_name:bridge ~managed;

    (try
       let vlan = Db.PIF.get_VLAN_master_of ~__context ~self:untagged_PIF in
       Db.VLAN.destroy ~__context ~self:vlan with _ -> ());
    Db.PIF.destroy ~__context ~self:untagged_PIF
  end

