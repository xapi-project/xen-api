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

module D = Debug.Make(struct let name="xapi_network_sriov" end)
open D

(** Dummy MAC used by the SRIOV **)
let network_sriov_mac = "fe:ff:ff:ff:ff:ff"

let create_internal ~__context ~physical_PIF ~physical_rec ~network =
  let sriov = Ref.make () in
  let sriov_uuid = Uuid.to_string (Uuid.make_uuid ()) in
  let logical_PIF = Ref.make () in
  let mTU = physical_rec.API.pIF_MTU in
  let metrics = physical_rec.API.pIF_metrics in
  let device = physical_rec.API.pIF_device in
  let host = physical_rec.API.pIF_host in
  Db.PIF.create ~__context ~ref:logical_PIF ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
    ~device ~device_name:device ~network ~host ~mAC:network_sriov_mac ~mTU ~vLAN:(-1L) ~metrics
    ~physical:false ~currently_attached:false ~igmp_snooping_status:`unknown
    ~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:"" ~dNS:"" ~bond_slave_of:Ref.null
    ~vLAN_master_of:Ref.null ~management:false ~other_config:[] ~disallow_unplug:false
    ~ipv6_configuration_mode:`None ~iPv6:[] ~ipv6_gateway:"" ~primary_address_type:`IPv4 ~managed:true
    ~properties:[] ~capabilities:[] ~pCI:Ref.null;
  info "network-sriov create uuid=%s" sriov_uuid;
  Db.Network_sriov.create ~__context ~ref:sriov ~uuid:sriov_uuid ~physical_PIF ~logical_PIF ~requires_reboot:false ~configuration_mode:`unknown;
  sriov, logical_PIF

let create ~__context ~pif ~network =
  Xapi_network.assert_network_is_managed ~__context ~self:network;
  Xapi_pif_helpers.assert_pif_is_managed ~__context ~self:pif;
  let pif_rec = Db.PIF.get_record ~__context ~self:pif in
  Xapi_pif_helpers.sriov_is_allowed_on_pif ~__context ~physical_PIF:pif ~pif_rec;
  let host = Db.PIF.get_host ~__context ~self:pif in
  Xapi_pif.assert_no_other_local_pifs ~__context ~host ~network;
  Xapi_network_helpers.assert_network_compatible_with_sriov ~__context ~pif ~network;

  info "Start creating logical PIF and network-sriov object";
  let sriov, logical_PIF = create_internal ~__context ~physical_PIF:pif ~physical_rec:pif_rec ~network in
  Xapi_pif.plug ~__context ~self:logical_PIF;
  sriov

let destroy ~__context ~self =
  let logical_PIF = Db.Network_sriov.get_logical_PIF ~__context ~self in
  Xapi_pif_helpers.assert_not_vlan_slave ~__context ~self:logical_PIF;
  Xapi_pif.unplug ~__context ~self:logical_PIF;
  Db.PIF.destroy ~__context ~self:logical_PIF;
  let sriov_uuid = Db.Network_sriov.get_uuid ~__context ~self in
  info "network-sriov destroy uuid=%s" sriov_uuid;
  Db.Network_sriov.destroy ~__context ~self

let get_remaining_capacity ~__context ~self =
  Xapi_network_sriov_helpers.get_remaining_capacity_on_sriov ~__context ~self
