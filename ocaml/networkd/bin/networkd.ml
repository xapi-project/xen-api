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

module D = Debug.Make (struct let name = "networkd" end)

let resources =
  [
    {
      Xcp_service.name= "network-conf"
    ; description= "used to select the network backend"
    ; essential= true
    ; path= Network_server.network_conf
    ; perms= [Unix.R_OK]
    }
  ; {
      Xcp_service.name= "brctl"
    ; description= "used to set up bridges"
    ; essential= true
    ; path= Network_utils.brctl
    ; perms= [Unix.X_OK]
    }
  ; {
      Xcp_service.name= "ethtool"
    ; description= "used to configure network interfaces"
    ; essential= true
    ; path= Network_utils.ethtool
    ; perms= [Unix.X_OK]
    }
  ; {
      Xcp_service.name= "fcoedriver"
    ; description= "used to identify fcoe interfaces"
    ; essential= false
    ; path= Network_utils.fcoedriver
    ; perms= [Unix.X_OK]
    }
  ; {
      Xcp_service.name= "inject-igmp-query-script"
    ; description= "used to inject an IGMP query message for a bridge"
    ; essential= false
    ; path= Network_utils.inject_igmp_query_script
    ; perms= [Unix.X_OK]
    }
  ]

let options =
  [
    ( "monitor_whitelist"
    , Arg.String
        (fun x ->
          Network_monitor_thread.monitor_whitelist :=
            Astring.String.cuts ~empty:false ~sep:"," x
        )
    , (fun () -> String.concat "," !Network_monitor_thread.monitor_whitelist)
    , "List of prefixes of interface names that are to be monitored"
    )
  ; ( "mac-table-size"
    , Arg.Set_int Network_utils.mac_table_size
    , (fun () -> string_of_int !Network_utils.mac_table_size)
    , "Default value for the mac-table-size openvswitch parameter (see \
       ovs-vswitchd.conf.db.5)"
    )
  ; ( "pvs-proxy-socket"
    , Arg.Set_string Network_server.PVS_proxy.path
    , (fun () -> !Network_server.PVS_proxy.path)
    , "Path to the Unix domain socket for the PVS-proxy daemon"
    )
  ; ( "igmp-query-maxresp-time"
    , Arg.Set_string Network_utils.igmp_query_maxresp_time
    , (fun () -> !Network_utils.igmp_query_maxresp_time)
    , "Maximum Response Time in IGMP Query message to send"
    )
  ; ( "enable-ipv6-mcast-snooping"
    , Arg.Bool (fun x -> Network_utils.enable_ipv6_mcast_snooping := x)
    , (fun () -> string_of_bool !Network_utils.enable_ipv6_mcast_snooping)
    , "IPv6 multicast snooping toggle"
    )
  ; ( "mcast-snooping-disable-flood-unregistered"
    , Arg.Bool
        (fun x -> Network_utils.mcast_snooping_disable_flood_unregistered := x)
    , (fun () ->
        string_of_bool !Network_utils.mcast_snooping_disable_flood_unregistered
      )
    , "Set OVS bridge configuration mcast-snooping-disable-flood-unregistered \
       as 'true' or 'false'"
    )
  ; ( "uname-cmd-path"
    , Arg.Set_string Network_utils.uname
    , (fun () -> !Network_utils.uname)
    , "Path to the Unix command uname"
    )
  ; ( "dracut-cmd-path"
    , Arg.Set_string Network_utils.dracut
    , (fun () -> !Network_utils.dracut)
    , "Path to the Unix command dracut"
    )
  ; ( "dracut-timeout"
    , Arg.Float
        (fun x ->
          let x = Float.to_int (x *. 1000.) in
          Network_utils.dracut_timeout := Mtime.Span.(x * ms)
        )
    , (fun () ->
        Float.to_string (Clock.Timer.span_to_s !Network_utils.dracut_timeout)
      )
    , "Default value for the dracut command timeout"
    )
  ; ( "modinfo-cmd-path"
    , Arg.Set_string Network_utils.modinfo
    , (fun () -> !Network_utils.modinfo)
    , "Path to the Unix command modinfo"
    )
  ; ( "json-rpc-max-len"
    , Arg.Set_int Jsonrpc_client.json_rpc_max_len
    , (fun () -> string_of_int !Jsonrpc_client.json_rpc_max_len)
    , "Maximum buffer size for Json RPC response"
    )
  ; ( "json-rpc-read-timeout"
    , Arg.Int
        (fun x ->
          Jsonrpc_client.json_rpc_read_timeout := Int64.(mul 1000000L (of_int x))
        )
    , (fun () ->
        Int64.(to_string (div !Jsonrpc_client.json_rpc_read_timeout 1000000L))
      )
    , "JSON RPC response read timeout value in ms"
    )
  ; ( "json-rpc-write-timeout"
    , Arg.Int
        (fun x ->
          Jsonrpc_client.json_rpc_write_timeout :=
            Int64.(mul 1000000L (of_int x))
        )
    , (fun () ->
        Int64.(to_string (div !Jsonrpc_client.json_rpc_write_timeout 1000000L))
      )
    , "JSON RPC write timeout value in ms"
    )
  ]

let start server =
  Network_monitor_thread.start () ;
  Network_server.on_startup () ;
  let (_ : Thread.t) =
    Thread.create (fun () -> Xcp_service.serve_forever server) ()
  in
  ()

let stop signal =
  Network_server.on_shutdown signal ;
  Network_monitor_thread.stop () ;
  exit 0

let handle_shutdown () =
  Sys.set_signal Sys.sigterm (Sys.Signal_handle stop) ;
  Sys.set_signal Sys.sigint (Sys.Signal_handle stop) ;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let doc =
  String.concat "\n"
    [
      "This is the xapi toolstack network management daemon."
    ; ""
    ; "This service looks after host network configuration, including setting \
       up bridges and/or openvswitch instances, configuring IP addresses etc."
    ]

let bind () =
  let open Network_server in
  S.clear_state clear_state ;
  S.reset_state reset_state ;
  S.sync_state sync_state ;
  S.set_gateway_interface set_gateway_interface ;
  S.set_dns_interface set_dns_interface ;
  S.Interface.get_all Interface.get_all ;
  S.Interface.get_interface_positions Interface.get_interface_positions ;
  S.Interface.exists Interface.exists ;
  S.Interface.get_mac Interface.get_mac ;
  S.Interface.get_pci_bus_path Interface.get_pci_bus_path ;
  S.Interface.is_up Interface.is_up ;
  S.Interface.get_ipv4_addr Interface.get_ipv4_addr ;
  S.Interface.set_ipv4_conf Interface.set_ipv4_conf ;
  S.Interface.get_ipv4_gateway Interface.get_ipv4_gateway ;
  S.Interface.get_ipv6_addr Interface.get_ipv6_addr ;
  S.Interface.get_ipv6_gateway Interface.get_ipv6_gateway ;
  S.Interface.get_dns Interface.get_dns ;
  S.Interface.get_mtu Interface.get_mtu ;
  S.Interface.get_capabilities Interface.get_capabilities ;
  S.Interface.is_connected Interface.is_connected ;
  S.Interface.is_physical Interface.is_physical ;
  S.Interface.has_vlan Interface.has_vlan ;
  S.Interface.bring_down Interface.bring_down ;
  S.Interface.set_persistent Interface.set_persistent ;
  S.Interface.make_config Interface.make_config ;
  S.Bridge.get_all Bridge.get_all ;
  S.Bridge.create Bridge.create ;
  S.Bridge.destroy Bridge.destroy ;
  S.Bridge.get_kind Bridge.get_kind ;
  S.Bridge.get_all_ports Bridge.get_all_ports ;
  S.Bridge.get_all_bonds Bridge.get_all_bonds ;
  S.Bridge.set_persistent Bridge.set_persistent ;
  S.Bridge.add_port Bridge.add_port ;
  S.Bridge.remove_port Bridge.remove_port ;
  S.Bridge.get_interfaces Bridge.get_interfaces ;
  S.Bridge.get_physical_interfaces Bridge.get_physical_interfaces ;
  S.Bridge.make_config Bridge.make_config ;
  S.PVS_proxy.configure_site PVS_proxy.configure_site ;
  S.PVS_proxy.remove_site PVS_proxy.remove_site ;
  S.Sriov.enable Sriov.enable ;
  S.Sriov.disable Sriov.disable ;
  S.Sriov.make_vf_config Sriov.make_vf_config

let () =
  Xcp_service.configure2 ~name:Sys.argv.(0) ~version:Xapi_version.version ~doc
    ~options ~resources () ;
  bind () ;
  let server =
    Xcp_service.make
      ~path:!Network_interface.default_path
      ~queue_name:!Network_interface.queue_name
      ~rpc_fn:(Idl.Exn.server Network_server.S.implementation)
      ()
  in
  Debug.set_facility Syslog.Local5 ;
  (* We should make the following configurable *)
  Debug.disable "http" ;
  handle_shutdown () ;
  Debug.with_thread_associated "main" start server ;
  let module Daemon = Xapi_stdext_unix.Unixext.Daemon in
  if Daemon.systemd_notify Daemon.State.Ready then
    ()
  else
    D.warn "Sending systemd notification failed at %s" __LOC__ ;
  while true do
    Thread.delay 300. ; Network_server.on_timer ()
  done
