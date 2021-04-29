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
module D = Debug.Make (struct let name = "nm" end)

open D
open Xapi_stdext_std.Xstringext
module Listext = Xapi_stdext_std.Listext.List
open Xapi_stdext_threads.Threadext
open Db_filter_types
open Network
open Network_interface

(* Protect a bunch of local operations with a mutex *)
let local_m = Mutex.create ()

let with_local_lock f = Mutex.execute local_m f

let is_dom0_interface pif_r =
  pif_r.API.pIF_ip_configuration_mode <> `None
  || pif_r.API.pIF_ipv6_configuration_mode <> `None
  || pif_r.API.pIF_physical = true
  || pif_r.API.pIF_bond_master_of <> []

let determine_mtu pif_rc net_rc =
  let mtu = Int64.to_int net_rc.API.network_MTU in
  if List.mem_assoc "mtu" pif_rc.API.pIF_other_config then (
    let value = List.assoc "mtu" pif_rc.API.pIF_other_config in
    try int_of_string value
    with _ ->
      debug "Invalid value for mtu = %s" value ;
      mtu
  ) else
    mtu

let determine_ethtool_settings properties oc =
  let proc key =
    if List.mem_assoc ("ethtool-" ^ key) oc then
      let value = List.assoc ("ethtool-" ^ key) oc in
      if value = "true" || value = "on" then
        [(key, "on")]
      else if value = "false" || value = "off" then
        [(key, "off")]
      else (
        debug "Invalid value for ethtool-%s = %s. Must be on|true|off|false."
          key value ;
        []
      )
    else if List.mem_assoc key properties then
      [(key, List.assoc key properties)]
    else
      []
  in
  let speed =
    if List.mem_assoc "ethtool-speed" oc then
      let value = List.assoc "ethtool-speed" oc in
      if value = "10" || value = "100" || value = "1000" then
        [("speed", value)]
      else (
        debug "Invalid value for ethtool-speed = %s. Must be 10|100|1000." value ;
        []
      )
    else
      []
  in
  let duplex =
    if List.mem_assoc "ethtool-duplex" oc then
      let value = List.assoc "ethtool-duplex" oc in
      if value = "half" || value = "full" then
        [("duplex", value)]
      else (
        debug "Invalid value for ethtool-duplex = %s. Must be half|full." value ;
        []
      )
    else
      []
  in
  let autoneg = proc "autoneg" in
  let settings = speed @ duplex @ autoneg in
  let offload =
    List.flatten
      (List.map proc ["rx"; "tx"; "sg"; "tso"; "ufo"; "gso"; "gro"; "lro"])
  in
  (settings, offload)

let determine_other_config ~__context pif_rc net_rc =
  let pif_oc = pif_rc.API.pIF_other_config in
  let net_oc = net_rc.API.network_other_config in
  let pool_oc =
    Db.Pool.get_other_config ~__context ~self:(Helpers.get_pool ~__context)
  in
  let additional = [("network-uuids", net_rc.API.network_uuid)] in
  (pool_oc |> Listext.update_assoc net_oc |> Listext.update_assoc pif_oc)
  @ additional

let create_bond ~__context bond mtu persistent =
  (* Get all information we need from the DB before doing anything that may drop our
     	 * management connection *)
  let master = Db.Bond.get_master ~__context ~self:bond in
  let master_rc = Db.PIF.get_record ~__context ~self:master in
  let slaves = Db.Bond.get_slaves ~__context ~self:bond in
  let slave_devices_bridges_and_config =
    List.map
      (fun pif ->
        let device = Db.PIF.get_device ~__context ~self:pif in
        let bridge =
          let network = Db.PIF.get_network ~__context ~self:pif in
          Db.Network.get_bridge ~__context ~self:network
        in
        let other_config = Db.PIF.get_other_config ~__context ~self:pif in
        let ethtool_settings, ethtool_offload =
          determine_ethtool_settings master_rc.API.pIF_properties other_config
        in
        let config =
          {
            default_interface with
            mtu
          ; ethtool_settings
          ; ethtool_offload
          ; persistent_i= persistent
          }
        in
        (device, bridge, config))
      slaves
  in
  let master_net_rc =
    Db.Network.get_record ~__context ~self:master_rc.API.pIF_network
  in
  let props = Db.Bond.get_properties ~__context ~self:bond in
  let mode = Db.Bond.get_mode ~__context ~self:bond in
  let other_config =
    determine_other_config ~__context master_rc master_net_rc
  in
  (* clean up and configure bond slaves *)
  let cleanup =
    List.map
      (fun (_, bridge, _) -> (bridge, true))
      slave_devices_bridges_and_config
  in
  let interface_config =
    List.map
      (fun (device, _, config) -> (device, config))
      slave_devices_bridges_and_config
  in
  let port = master_rc.API.pIF_device in
  let mac = master_rc.API.pIF_MAC in
  (* set bond properties *)
  let props =
    let rec get_prop p =
      if List.mem_assoc p props then
        List.assoc p props
      else
        ""
    and get_prop_assoc_if_mode m p =
      if mode = m then
        if List.mem_assoc p props then
          [(p, List.assoc p props)]
        else
          []
      else
        []
    in
    if List.length slaves > 1 then
      let hashing_algorithm = get_prop "hashing_algorithm"
      and rebalance_interval =
        if mode = `lacp then
          []
        else
          [("rebalance-interval", "1800000")]
      and lacp_timeout = get_prop_assoc_if_mode `lacp "lacp-time"
      and lacp_aggregation_key =
        get_prop_assoc_if_mode `lacp "lacp-aggregation-key"
      and lacp_fallback_ab = get_prop_assoc_if_mode `lacp "lacp-fallback-ab" in
      let props =
        [
          ("mode", Record_util.bond_mode_to_string mode)
        ; ("miimon", "100")
        ; ("downdelay", "200")
        ; ("updelay", "31000")
        ; ("use_carrier", "1")
        ; ("hashing-algorithm", hashing_algorithm)
        ]
        @ rebalance_interval
        @ lacp_timeout
        @ lacp_aggregation_key
        @ lacp_fallback_ab
      in
      let overrides =
        List.filter_map
          (fun (k, v) ->
            if String.startswith "bond-" k then
              Some (String.sub_to_end k 5, v)
            else
              None)
          master_rc.API.pIF_other_config
      in
      (* add defaults for properties that are not overridden *)
      List.filter (fun (k, _) -> not (List.mem_assoc k overrides)) props
      @ overrides
    else (* Sometimes a "Bond" is not actually a bond... *)
      []
  in
  let ports =
    [
      ( port
      , {
          interfaces=
            List.map
              (fun (device, _, _) -> device)
              slave_devices_bridges_and_config
        ; bond_properties= props
        ; bond_mac= Some mac
        ; kind= Basic_port
        } )
    ]
  in
  let igmp_snooping =
    Some
      (Db.Pool.get_igmp_snooping_enabled ~__context
         ~self:(Helpers.get_pool ~__context))
  in
  ( cleanup
  , [
      ( master_net_rc.API.network_bridge
      , {
          default_bridge with
          ports
        ; bridge_mac= Some mac
        ; other_config
        ; igmp_snooping
        ; persistent_b= persistent
        } )
    ]
  , interface_config )

let destroy_bond ~__context ~force bond =
  let master = Db.Bond.get_master ~__context ~self:bond in
  let network = Db.PIF.get_network ~__context ~self:master in
  [(Db.Network.get_bridge ~__context ~self:network, force)]

let create_vlan ~__context vlan persistent =
  let master = Db.VLAN.get_untagged_PIF ~__context ~self:vlan in
  let master_rc = Db.PIF.get_record ~__context ~self:master in
  let master_network_rc =
    Db.Network.get_record ~__context ~self:master_rc.API.pIF_network
  in
  let slave = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
  let slave_rc = Db.PIF.get_record ~__context ~self:slave in
  let slave_network_rc =
    Db.Network.get_record ~__context ~self:slave_rc.API.pIF_network
  in
  let tag = Int64.to_int (Db.VLAN.get_tag ~__context ~self:vlan) in
  let mac = slave_rc.API.pIF_MAC in
  let other_config =
    determine_other_config ~__context master_rc master_network_rc
  in
  let other_config =
    Listext.replace_assoc "network-uuids"
      (master_network_rc.API.network_uuid
      ^ ";"
      ^ slave_network_rc.API.network_uuid
      )
      other_config
  in
  [
    ( master_network_rc.API.network_bridge
    , {
        default_bridge with
        vlan= Some (slave_network_rc.API.network_bridge, tag)
      ; other_config
      ; bridge_mac= Some mac
      ; persistent_b= persistent
      } )
  ]

let destroy_vlan ~__context vlan =
  let master = Db.VLAN.get_untagged_PIF ~__context ~self:vlan in
  let bridge =
    let network = Db.PIF.get_network ~__context ~self:master in
    Db.Network.get_bridge ~__context ~self:network
  in
  [(bridge, false)]

let linux_pif_config pif_type pif_rc properties mtu persistent =
  (* If we are using linux bridge rather than OVS, then we need to
     	 * configure the "pif" that represents the vlan or bond.
     	 * In OVS there is no such device, so the config entry will be ignored
     	 * by Interface.make_config in xcp-networkd/networkd/network_server.ml *)
  let ethtool_settings, ethtool_offload =
    determine_ethtool_settings properties pif_rc.API.pIF_other_config
  in
  ( (pif_rc.API.pIF_device
    ^
    match pif_type with
    | `bond_pif ->
        ""
    | `vlan_pif ->
        "." ^ Int64.to_string pif_rc.API.pIF_VLAN
    )
  , {
      default_interface with
      mtu
    ; ethtool_settings
    ; ethtool_offload
    ; persistent_i= persistent
    } )

let rec create_bridges ~__context pif_rc net_rc =
  let mtu = determine_mtu pif_rc net_rc in
  let other_config = determine_other_config ~__context pif_rc net_rc in
  let persistent = is_dom0_interface pif_rc in
  let igmp_snooping =
    Some
      (Db.Pool.get_igmp_snooping_enabled ~__context
         ~self:(Helpers.get_pool ~__context))
  in
  let open Xapi_pif_helpers in
  match get_pif_type pif_rc with
  | Tunnel_access _ ->
      ( []
      , [
          ( net_rc.API.network_bridge
          , {
              default_bridge with
              bridge_mac= Some pif_rc.API.pIF_MAC
            ; igmp_snooping
            ; other_config
            ; persistent_b= persistent
            } )
        ]
      , [] )
  | VLAN_untagged vlan ->
      let original_pif_rc = pif_rc in
      let slave = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
      let pif_rc = Db.PIF.get_record ~__context ~self:slave in
      let net_rc =
        Db.Network.get_record ~__context ~self:pif_rc.API.pIF_network
      in
      let cleanup, bridge_config, interface_config =
        create_bridges ~__context pif_rc net_rc
      in
      let interface_config =
        (* Add configuration for the vlan device itself *)
        linux_pif_config `vlan_pif original_pif_rc pif_rc.API.pIF_properties mtu
          persistent
        :: interface_config
      in
      ( cleanup
      , create_vlan ~__context vlan persistent @ bridge_config
      , interface_config )
  | Bond_master bond ->
      let cleanup, bridge_config, interface_config =
        create_bond ~__context bond mtu persistent
      in
      let interface_config =
        (*  Add configuration for the bond pif itself *)
        linux_pif_config `bond_pif pif_rc pif_rc.API.pIF_properties mtu
          persistent
        :: interface_config
      in
      (cleanup, bridge_config, interface_config)
  | Physical _ ->
      let cleanup =
        if pif_rc.API.pIF_bond_slave_of <> Ref.null then
          destroy_bond ~__context ~force:true pif_rc.API.pIF_bond_slave_of
        else
          []
      in
      let ethtool_settings, ethtool_offload =
        determine_ethtool_settings pif_rc.API.pIF_properties
          pif_rc.API.pIF_other_config
      in
      let ports =
        [
          ( pif_rc.API.pIF_device
          , {default_port with interfaces= [pif_rc.API.pIF_device]} )
        ]
      in
      ( cleanup
      , [
          ( net_rc.API.network_bridge
          , {
              default_bridge with
              ports
            ; bridge_mac= Some pif_rc.API.pIF_MAC
            ; igmp_snooping
            ; other_config
            ; persistent_b= persistent
            } )
        ]
      , [
          ( pif_rc.API.pIF_device
          , {
              default_interface with
              mtu
            ; ethtool_settings
            ; ethtool_offload
            ; persistent_i= persistent
            } )
        ] )
  | Network_sriov_logical _ ->
      raise
        Api_errors.(
          Server_error
            (internal_error, ["Should not create bridge for SRIOV logical PIF"]))

let rec destroy_bridges ~__context ~force pif_rc bridge =
  let open Xapi_pif_helpers in
  match get_pif_type pif_rc with
  | Tunnel_access _ ->
      [(bridge, false)]
  | VLAN_untagged vlan ->
      let cleanup = destroy_vlan ~__context vlan in
      let slave = Db.VLAN.get_tagged_PIF ~__context ~self:vlan in
      let rc = Db.PIF.get_record ~__context ~self:slave in
      if not rc.API.pIF_currently_attached then
        let bridge =
          Db.Network.get_bridge ~__context ~self:rc.API.pIF_network
        in
        destroy_bridges ~__context ~force rc bridge @ cleanup
      else
        cleanup
  | Bond_master bond ->
      destroy_bond ~__context ~force bond
  | Physical _ ->
      [(bridge, false)]
  | Network_sriov_logical _ ->
      raise
        Api_errors.(
          Server_error
            (internal_error, ["Should not destroy bridge for SRIOV logical PIF"]))

let determine_static_routes net_rc =
  if List.mem_assoc "static-routes" net_rc.API.network_other_config then
    try
      let routes =
        String.split ','
          (List.assoc "static-routes" net_rc.API.network_other_config)
      in
      List.map
        (fun route ->
          Scanf.sscanf route "%[^/]/%d/%[^/]" (fun a b c ->
              {
                subnet= Unix.inet_addr_of_string a
              ; netmask= b
              ; gateway= Unix.inet_addr_of_string c
              }))
        routes
    with _ -> []
  else
    []

let maybe_update_master_pif_mac ~__context bond_record
    (bond_master_record : API.pIF_t) bond_master =
  if bond_record.API.bond_auto_update_mac then
    (* Make sure the MAC address of the PIF Bond master matches the MAC address of the Bond's primary slave *)
    let primary_slave_ref = bond_record.API.bond_primary_slave in
    let primary_slave_MAC = Db.PIF.get_MAC ~__context ~self:primary_slave_ref in
    if bond_master_record.pIF_MAC <> primary_slave_MAC then (
      Db.PIF.set_MAC ~__context ~self:bond_master ~value:primary_slave_MAC ;
      {bond_master_record with API.pIF_MAC= primary_slave_MAC}
    ) else
      bond_master_record
  else
    bond_master_record

let bring_pif_up ~__context ?(management_interface = false) (pif : API.ref_PIF)
    =
  with_local_lock (fun () ->
      let rc = Db.PIF.get_record ~__context ~self:pif in
      let open Xapi_pif_helpers in
      match get_pif_topo ~__context ~pif_rec:rc with
      | Network_sriov_logical _ :: _ ->
          Xapi_network_sriov_helpers.sriov_bring_up ~__context ~self:pif
      | VLAN_untagged _ :: Network_sriov_logical sriov :: _ ->
          let sriov_logical_pif =
            Db.Network_sriov.get_logical_PIF ~__context ~self:sriov
          in
          let currently_attached =
            Db.PIF.get_currently_attached ~__context ~self:sriov_logical_pif
          in
          Db.PIF.set_currently_attached ~__context ~self:pif
            ~value:currently_attached
      | _ -> (
          let dbg = Context.string_of_task __context in
          let net_rc =
            Db.Network.get_record ~__context ~self:rc.API.pIF_network
          in
          let bridge = net_rc.API.network_bridge in
          (* Call networkd even if currently_attached is false, just to update its state *)
          debug "Making sure that PIF %s is up" rc.API.pIF_uuid ;
          (* If the PIF is a bond master, the bond slaves will now go down *)
          (* Interface-reconfigure in bridge mode requires us to set currently_attached to false here *)
          let rc =
            match rc.API.pIF_bond_master_of with
            | [] ->
                rc
            | bond :: _ ->
                let bond_record = Db.Bond.get_record ~__context ~self:bond in
                List.iter
                  (fun self ->
                    Db.PIF.set_currently_attached ~__context ~self ~value:false)
                  bond_record.API.bond_slaves ;
                maybe_update_master_pif_mac ~__context bond_record rc pif
          in
          let address_type = rc.API.pIF_primary_address_type in
          Network.transform_networkd_exn pif (fun () ->
              let persistent = is_dom0_interface rc in
              let gateway_if, dns_if =
                Helpers.determine_gateway_and_dns_ifs ~__context
                  ?management_interface:
                    (if management_interface then Some pif else None)
                  ()
              in
              Option.iter
                (fun (_, name) -> Net.set_gateway_interface dbg name)
                gateway_if ;
              Option.iter
                (fun (_, name) -> Net.set_dns_interface dbg name)
                dns_if ;
              (* Setup network infrastructure *)
              let cleanup, bridge_config, interface_config =
                create_bridges ~__context rc net_rc
              in
              List.iter
                (fun (name, force) -> Net.Bridge.destroy dbg force name)
                cleanup ;
              Net.Bridge.make_config dbg false bridge_config ;
              Net.Interface.make_config dbg false interface_config ;
              (* Configure IPv4 parameters and DNS *)
              let ipv4_conf, ipv4_gateway =
                match rc.API.pIF_ip_configuration_mode with
                | `None ->
                    (None4, None)
                | `DHCP ->
                    (DHCP4, None)
                | `Static ->
                    let conf =
                      Static4
                        [
                          ( Unix.inet_addr_of_string rc.API.pIF_IP
                          , netmask_to_prefixlen rc.API.pIF_netmask )
                        ]
                    in
                    let gateway =
                      if rc.API.pIF_gateway <> "" then
                        Some (Unix.inet_addr_of_string rc.API.pIF_gateway)
                      else
                        None
                    in
                    (conf, gateway)
              in
              let ipv4_routes = determine_static_routes net_rc in
              (* Configure IPv6 parameters *)
              let ipv6_conf, ipv6_gateway =
                match rc.API.pIF_ipv6_configuration_mode with
                | `None ->
                    (Linklocal6, None)
                | `DHCP ->
                    (DHCP6, None)
                | `Autoconf ->
                    (Autoconf6, None)
                | `Static ->
                    let addresses =
                      List.filter_map
                        (fun addr_and_prefixlen ->
                          try
                            let n = String.index addr_and_prefixlen '/' in
                            let addr =
                              Unix.inet_addr_of_string
                                (String.sub addr_and_prefixlen 0 n)
                            in
                            let prefixlen =
                              int_of_string
                                (String.sub_to_end addr_and_prefixlen (n + 1))
                            in
                            Some (addr, prefixlen)
                          with _ -> None)
                        rc.API.pIF_IPv6
                    in
                    let conf = Static6 addresses in
                    let gateway =
                      if rc.API.pIF_ipv6_gateway <> "" then
                        Some (Unix.inet_addr_of_string rc.API.pIF_ipv6_gateway)
                      else
                        None
                    in
                    (conf, gateway)
              in
              let static =
                match address_type with
                | `IPv4 ->
                    rc.API.pIF_ip_configuration_mode = `Static
                | `IPv6 ->
                    rc.API.pIF_ipv6_configuration_mode = `Static
              in
              let dns =
                match (static, rc.API.pIF_DNS) with
                | false, _ | true, "" ->
                    ([], [])
                | true, pif_dns ->
                    let nameservers =
                      List.map Unix.inet_addr_of_string
                        (String.split ',' pif_dns)
                    in
                    let domains =
                      match List.assoc_opt "domain" rc.API.pIF_other_config with
                      | None ->
                          []
                      | Some domains -> (
                        try String.split ',' domains
                        with _ ->
                          warn "Invalid DNS search domains: %s" domains ;
                          []
                      )
                    in
                    (nameservers, domains)
              in
              let mtu = determine_mtu rc net_rc in
              let ethtool_settings, ethtool_offload =
                determine_ethtool_settings rc.API.pIF_properties
                  net_rc.API.network_other_config
              in
              let interface_config =
                [
                  ( bridge
                  , {
                      ipv4_conf
                    ; ipv4_gateway
                    ; ipv6_conf
                    ; ipv6_gateway
                    ; ipv4_routes
                    ; dns
                    ; ethtool_settings
                    ; ethtool_offload
                    ; mtu
                    ; persistent_i= persistent
                    } )
                ]
              in
              Net.Interface.make_config dbg false interface_config) ;
          let new_ip =
            let addr =
              match address_type with
              | `IPv4 ->
                  Net.Interface.get_ipv4_addr dbg bridge
              | `IPv6 ->
                  Net.Interface.get_ipv6_addr dbg bridge
            in
            match addr with
            | (ip, _) :: _ ->
                Unix.string_of_inet_addr ip
            | [] ->
                ""
          in
          let pif_ip =
            match address_type with
            | `IPv4 ->
                rc.API.pIF_IP
            | `IPv6 -> (
              match rc.API.pIF_IPv6 with
              | [] ->
                  ""
              | hd :: _ -> (
                (* IPv6 addresses are stored with this format: <ipv6>/<cidr> *)
                match String.split_on_char '/' hd with
                | [ip; _] ->
                    ip
                | _ ->
                    ""
              )
            )
          in
          if new_ip <> pif_ip then (
            warn "An IP address of dom0 was changed" ;
            warn "About to kill idle client stunnels" ;
            (* The master_connection would otherwise try to take a broken stunnel from the cache *)
            Stunnel_cache.flush () ;
            warn "About to forcibly reset the master connection" ;
            Master_connection.force_connection_reset ()
          ) ;
          if rc.API.pIF_currently_attached = false || management_interface then (
            if management_interface then (
              warn "About to kill active client stunnels" ;
              let stunnels =
                let all =
                  Locking_helpers.Thread_state.get_all_acquired_resources ()
                in
                debug "There are %d allocated resources" (List.length all) ;
                List.filter
                  (function
                    | Locking_helpers.Process ("stunnel", _) ->
                        true
                    | _ ->
                        false)
                  all
              in
              debug "Of which %d are stunnels" (List.length stunnels) ;
              List.iter Locking_helpers.kill_resource stunnels
            ) ;
            Db.PIF.set_currently_attached ~__context ~self:pif ~value:true ;
            (* If the PIF is a bond slave, the bond master will now be down *)
            ( match rc.API.pIF_bond_slave_of with
            | bond when bond = Ref.null ->
                ()
            | bond ->
                let master = Db.Bond.get_master ~__context ~self:bond in
                Db.PIF.set_currently_attached ~__context ~self:master
                  ~value:false
            ) ;
            Xapi_mgmt_iface.on_dom0_networking_change ~__context
          ) ;
          (* sync MTU *)
          ( try
              let mtu = Int64.of_int (Net.Interface.get_mtu dbg bridge) in
              if mtu <> rc.API.pIF_MTU then
                Db.PIF.set_MTU ~__context ~self:pif ~value:mtu
            with _ ->
              warn "could not update MTU field on PIF %s" rc.API.pIF_uuid
          ) ;
          (* sync igmp_snooping_enabled *)
          ( if rc.API.pIF_VLAN = -1L then
              let igmp_snooping =
                Db.Pool.get_igmp_snooping_enabled ~__context
                  ~self:(Helpers.get_pool ~__context)
              in
              let igmp_snooping' =
                if igmp_snooping then `enabled else `disabled
              in
              if igmp_snooping' <> rc.API.pIF_igmp_snooping_status then
                Db.PIF.set_igmp_snooping_status ~__context ~self:pif
                  ~value:igmp_snooping'
          ) ;
          (* Open VxLAN UDP port (4789) if a VxLAN tunnel access PIF is plugged *)
          match Xapi_pif_helpers.get_pif_type rc with
          | Tunnel_access tunnel_ref -> (
              let protocol =
                Db.Tunnel.get_protocol ~__context ~self:tunnel_ref
              in
              match protocol with
              | `vxlan ->
                  debug
                    "Opening VxLAN UDP port for tunnel with protocol 'vxlan'" ;
                  ignore
                  @@ Helpers.call_script
                       !Xapi_globs.firewall_port_config_script
                       ["open"; "4789"; "udp"]
              | `gre ->
                  ()
            )
          | _ ->
              ()
        ))

let bring_pif_down ~__context ?(force = false) (pif : API.ref_PIF) =
  with_local_lock (fun () ->
      let rc = Db.PIF.get_record ~__context ~self:pif in
      let open Xapi_pif_helpers in
      let close_network_interface cleanup_f () =
        Network.transform_networkd_exn pif (fun () ->
            let dbg = Context.string_of_task __context in
            debug "Making sure that PIF %s down" rc.API.pIF_uuid ;
            let bridge =
              Db.Network.get_bridge ~__context ~self:rc.API.pIF_network
            in
            let cleanup = destroy_bridges ~__context ~force rc bridge in
            List.iter
              (fun (name, force) -> Net.Bridge.destroy dbg force name)
              cleanup ;
            Net.Interface.set_persistent dbg bridge false ;
            Db.PIF.set_currently_attached ~__context ~self:pif ~value:false ;
            cleanup_f ())
      in
      match get_pif_topo ~__context ~pif_rec:rc with
      | Network_sriov_logical _ :: _ ->
          Xapi_network_sriov_helpers.sriov_bring_down ~__context ~self:pif
      | VLAN_untagged _ :: Network_sriov_logical _ :: _ ->
          Db.PIF.set_currently_attached ~__context ~self:pif ~value:false
      | Tunnel_access tunnel_ref :: _ ->
          (* If last tunnel with VxLAN protocol is unplugged, close VxLAN UDP port *)
          let maybe_close_port () =
            let host = rc.API.pIF_host in
            match Db.Tunnel.get_protocol ~__context ~self:tunnel_ref with
            | `vxlan ->
                let expr =
                  Eq
                    ( Field "protocol"
                    , Literal (Record_util.tunnel_protocol_to_string `vxlan) )
                in
                let tunnels = Db.Tunnel.get_records_where ~__context ~expr in
                let still_plugged_on_host (_, tunnel_rec) =
                  let pif_ref = tunnel_rec.API.tunnel_access_PIF in
                  let pif_record = Db.PIF.get_record ~__context ~self:pif_ref in
                  host = pif_record.API.pIF_host
                  && pif_record.API.pIF_currently_attached
                in
                let no_more_vxlan =
                  not (List.exists still_plugged_on_host tunnels)
                in
                if no_more_vxlan then (
                  debug "Last VxLAN tunnel was closed, closing VxLAN UDP port" ;
                  ignore
                  @@ Helpers.call_script
                       !Xapi_globs.firewall_port_config_script
                       ["close"; "4789"; "udp"]
                )
            | `gre ->
                ()
          in
          close_network_interface maybe_close_port ()
      | _ ->
          close_network_interface (fun () -> ()) ())
