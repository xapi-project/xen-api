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

open Db_filter_types

let choose_tunnel_device_name ~__context ~host =
  (* list all the tunnel access PIFs on this host *)
  let pifs = Db.PIF.get_refs_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of host)),
      Not (Eq (Field "tunnel_access_PIF_of", Literal "()"))
    )) in
  let devices = List.map (fun self -> Db.PIF.get_device ~__context ~self) pifs in
  let rec choose n =
    let name = Printf.sprintf "tunnel%d" n in
    if List.mem name devices
    then choose (n + 1)
    else name in
  choose 0

let create_internal ~__context ~transport_PIF ~network ~host =
  let tunnel = Ref.make () in
  let access_PIF = Ref.make () in
  let device = choose_tunnel_device_name ~__context ~host in
  let device_name = device in
  let mAC = Xapi_vif_helpers.gen_mac (0, Uuid.to_string (Uuid.make_uuid ())) in
  let metrics = Db.PIF.get_metrics ~__context ~self:transport_PIF in
  Db.PIF.create ~__context ~ref:access_PIF ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
    ~device ~device_name ~network ~host ~mAC ~mTU:(-1L) ~vLAN:(-1L) ~metrics
    ~physical:false ~currently_attached:false ~igmp_snooping_status:`unknown
    ~ip_configuration_mode:`None ~iP:"" ~netmask:"" ~gateway:"" ~dNS:"" ~bond_slave_of:Ref.null
    ~vLAN_master_of:Ref.null
    ~management:false ~other_config:[] ~disallow_unplug:false ~ipv6_configuration_mode:`None
    ~iPv6:[""] ~ipv6_gateway:"" ~primary_address_type:`IPv4 ~managed:true ~properties:[] ~capabilities:[] ~pCI:Ref.null;
  Db.Tunnel.create ~__context ~ref:tunnel ~uuid:(Uuid.to_string (Uuid.make_uuid ()))
    ~access_PIF ~transport_PIF ~status:["active", "false"] ~other_config:[];
  tunnel, access_PIF

let create ~__context ~transport_PIF ~network =
  Xapi_network.assert_network_is_managed ~__context ~self:network;
  let host = Db.PIF.get_host ~__context ~self:transport_PIF in
  Xapi_pif.assert_no_other_local_pifs ~__context ~host ~network;
  Xapi_pif_helpers.assert_pif_is_managed ~__context ~self:transport_PIF;
  Xapi_pif_helpers.tunnel_is_allowed_on_pif ~__context ~transport_PIF;
  let hosts = Db.Host.get_all ~__context in
  List.iter
    (fun h ->
       let v = Db.Host.get_software_version ~__context ~self:h in
       if not (List.mem_assoc "network_backend" v && List.assoc "network_backend" v = "openvswitch") then
         raise (Api_errors.Server_error (Api_errors.openvswitch_not_active, []));
    ) hosts;
  let tunnel, access_PIF = create_internal ~__context ~transport_PIF ~network ~host in
  Xapi_pif.plug ~__context ~self:access_PIF;
  tunnel

let destroy ~__context ~self =
  let pif = Db.Tunnel.get_access_PIF ~__context ~self in
  Xapi_pif.unplug ~__context ~self:pif;
  Db.PIF.destroy ~__context ~self:pif;
  Db.Tunnel.destroy ~__context ~self

