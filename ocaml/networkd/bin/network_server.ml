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

open Network_utils
open Network_interface
module S = Network_interface.Interface_API (Idl.Exn.GenServer ())

module D = Debug.Make (struct let name = "network_server" end)

open D

type context = unit

let network_conf = ref "/etc/xcp/network.conf"

let config : config_t ref = ref Network_config.empty_config

let write_lock = ref false

let backend_kind = ref Openvswitch

let write_config () =
  if not !write_lock then
    try Network_config.write_config !config
    with Network_config.Write_error -> ()

let read_config () =
  try
    config := Network_config.read_config () ;
    debug "Read configuration from networkd.db file."
  with Network_config.Read_error -> (
    try
      (* No configuration file found. Try to get the initial network setup from
       * the first-boot data written by the host installer. *)
      config := Network_config.read_management_conf () ;
      debug "Read configuration from management.conf file."
    with Network_config.Read_error ->
      debug "Could not interpret the configuration in management.conf"
  )

let on_shutdown signal =
  let dbg = "shutdown" in
  Debug.with_thread_associated dbg
    (fun () ->
      debug "xcp-networkd caught signal %d; performing cleanup actions." signal ;
      write_config ()
    )
    ()

let on_timer () = write_config ()

let clear_state () =
  write_lock := true ;
  config := Network_config.empty_config

let sync_state () =
  write_lock := false ;
  write_config ()

let reset_state () = config := Network_config.read_management_conf ()

let set_gateway_interface _dbg name =
  (* Remove dhclient conf (if any) for the old and new gateway interfaces.
   * This ensures that dhclient gets restarted with an updated conf file when
   * necessary. *)
  ( match !config.gateway_interface with
  | Some old_iface when name <> old_iface ->
      Dhclient.remove_conf_file name ;
      Dhclient.remove_conf_file old_iface
  | _ ->
      ()
  ) ;
  debug "Setting gateway interface to %s" name ;
  config := {!config with gateway_interface= Some name}

let set_dns_interface _dbg name =
  (* Remove dhclient conf (if any) for the old and new DNS interfaces.
   * This ensures that dhclient gets restarted with an updated conf file when
   * necessary. *)
  ( match !config.dns_interface with
  | Some old_iface when name <> old_iface ->
      Dhclient.remove_conf_file name ;
      Dhclient.remove_conf_file old_iface
  | _ ->
      ()
  ) ;
  debug "Setting DNS interface to %s" name ;
  config := {!config with dns_interface= Some name}

(* The enic driver is for Cisco UCS devices. The current driver adds VLAN0 headers
 * to all incoming packets, which confuses certain guests OSes. The workaround
 * constitutes adding a VLAN0 Linux device to strip those headers again.
 *)
let need_enic_workaround () =
  !backend_kind = Bridge && List.mem "enic" (Sysfs.list_pci_drivers ())

module Sriov = struct
  open S.Sriov

  let get_capabilities dev =
    let open Rresult.R.Infix in
    let maxvfs_modprobe =
      Sysfs.get_driver_name_err dev >>= fun driver ->
      Modprobe.get_config_from_comments driver |> Modprobe.get_maxvfs driver
    and maxvfs_sysfs = Sysfs.get_sriov_maxvfs dev in
    let supported =
      (* Always require sysfs to declare SR-IOV support for a NIC.
         If the VF-maxvfs modprobe parameters exist, these must also declare a
         non-zero maxvfs number (the user may override it). *)
      match (maxvfs_modprobe, maxvfs_sysfs) with
      | Ok v, Ok w ->
          v > 0 && w > 0
      | Error _, Ok v ->
          v > 0
      | _ ->
          false
    in
    if supported then ["sriov"] else []

  let config_sriov ~enable dev =
    let op = if enable then "enable" else "disable" in
    let open Rresult.R.Infix in
    Sysfs.get_driver_name_err dev >>= fun driver ->
    let config = Modprobe.get_config_from_comments driver in
    match Modprobe.get_vf_param config with
    | Some vf_param ->
        debug "%s SR-IOV on a device: %s via modprobe" op dev ;
        (if enable then Modprobe.get_maxvfs driver config else Ok 0)
        >>= fun numvfs ->
        (* CA-287340: Even if the current numvfs equals to the target numvfs, it
           is still needed to update SR-IOV modprobe config file, as the SR-IOV
           enabing takes effect after reboot. For example, a user enables SR-IOV
           and disables it immediately without a reboot.*)
        Modprobe.config_sriov driver vf_param numvfs >>= fun _ ->
        if numvfs = Sysfs.get_sriov_numvfs dev then
          Ok Modprobe_successful
        else
          Ok Modprobe_successful_requires_reboot
    | None ->
        (* enable: try sysfs interface to set numfvs = maxvfs. if fails, but vfs are enabled, assume manual configuration.
           disable: Net.Sriov.disable will not be called for manually configured interfaces, as determined by `require_operation_on_pci_device` *)
        let man_successful () =
          debug "SR-IOV/VFs %sd manually on device: %s" op dev ;
          Manual_successful
        in
        if enable then
          let present_numvfs = Sysfs.get_sriov_numvfs dev in
          match
            Sysfs.get_sriov_maxvfs dev >>= fun maxvfs ->
            maxvfs |> Sysfs.set_sriov_numvfs dev
          with
          | Ok _ ->
              debug "%s SR-IOV on a device: %s via sysfs" op dev ;
              Ok Sysfs_successful
          | Error _ when present_numvfs > 0 ->
              Ok (man_successful ())
          | exception _ when present_numvfs > 0 ->
              Ok (man_successful ())
          | Error err ->
              Error err
          | exception e ->
              let msg =
                Printf.sprintf
                  "Error: trying sysfs SR-IOV interface failed with exception \
                   %s on device: %s"
                  (Printexc.to_string e) dev
              in
              Error (Other, msg)
        else
          Sysfs.unbind_child_vfs dev >>= fun () ->
          Sysfs.set_sriov_numvfs dev 0 >>= fun _ ->
          debug "%s SR-IOV on a device: %s via sysfs" op dev ;
          Ok Sysfs_successful

  let enable dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Enable network SR-IOV by name: %s" name ;
        match config_sriov ~enable:true name with
        | Ok t ->
            (Ok t : enable_result)
        | Result.Error (_, msg) ->
            warn "Failed to enable SR-IOV on %s with error: %s" name msg ;
            Error msg
      )
      ()

  let disable dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Disable network SR-IOV by name: %s" name ;
        match config_sriov ~enable:false name with
        | Ok _ ->
            (Ok : disable_result)
        | Result.Error (_, msg) ->
            warn "Failed to disable SR-IOV on %s with error: %s" name msg ;
            Error msg
      )
      ()

  let make_vf_conf_internal pcibuspath mac vlan rate =
    let config_or_otherwise_reset config_f reset_f = function
      | None ->
          reset_f ()
      | Some a ->
          config_f a
    in
    let open Rresult.R.Infix in
    Sysfs.parent_device_of_vf pcibuspath >>= fun dev ->
    Sysfs.device_index_of_vf dev pcibuspath >>= fun index ->
    config_or_otherwise_reset (Ip.set_vf_mac dev index)
      (fun () -> Result.Ok ())
      mac
    >>= fun () ->
    (* In order to ensure the Networkd to be idempotent, configuring VF with no
       VLAN and rate have to reset vlan and rate, since the VF might have
       previous configuration. Refering to
       http://gittup.org/cgi-bin/man/man2html?ip-link+8, set VLAN and rate to 0
       means to reset them *)
    config_or_otherwise_reset (Ip.set_vf_vlan dev index)
      (fun () -> Ip.set_vf_vlan dev index 0)
      vlan
    >>= fun () ->
    config_or_otherwise_reset (Ip.set_vf_rate dev index)
      (fun () -> Ip.set_vf_rate dev index 0)
      rate

  let make_vf_config dbg pci_address (vf_info : sriov_pci_t) =
    Debug.with_thread_associated dbg
      (fun () ->
        let vlan = Option.map Int64.to_int vf_info.vlan
        and rate = Option.map Int64.to_int vf_info.rate
        and pcibuspath = Xcp_pci.string_of_address pci_address in
        debug "Config VF with pci address: %s" pcibuspath ;
        match make_vf_conf_internal pcibuspath vf_info.mac vlan rate with
        | Result.Ok () ->
            (Ok : config_result)
        | Result.Error (Fail_to_set_vf_rate, msg) ->
            debug "%s" msg ; Error Config_vf_rate_not_supported
        | Result.Error (_, msg) ->
            debug "%s" msg ; Error (Unknown msg)
      )
      ()
end

module Interface = struct
  let get_config name =
    get_config !config.interface_config default_interface name

  let update_config name data =
    config :=
      {
        !config with
        interface_config= update_config !config.interface_config name data
      }

  let get_all dbg () =
    Debug.with_thread_associated dbg (fun () -> Sysfs.list ()) ()

  let exists dbg name =
    Debug.with_thread_associated dbg
      (fun () -> List.mem name (Sysfs.list ()))
      ()

  let get_mac dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        match Linux_bonding.get_bond_master_of name with
        | Some master ->
            Proc.get_bond_slave_mac master name
        | None ->
            Ip.get_mac name
      )
      ()

  let get_pci_bus_path dbg name =
    Debug.with_thread_associated dbg (fun () -> Sysfs.get_pcibuspath name) ()

  let is_up dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        if List.mem name (Sysfs.list ()) then
          Ip.is_up name
        else
          false
      )
      ()

  let get_ipv4_addr dbg name =
    Debug.with_thread_associated dbg (fun () -> Ip.get_ipv4 name) ()

  let set_ipv4_conf dbg name conf =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Configuring IPv4 address for %s: %s" name
          (conf |> Rpcmarshal.marshal typ_of_ipv4 |> Jsonrpc.to_string) ;
        update_config name {(get_config name) with ipv4_conf= conf} ;
        match conf with
        | None4 ->
            if List.mem name (Sysfs.list ()) then (
              if Dhclient.is_running name then
                ignore (Dhclient.stop name) ;
              Ip.flush_ip_addr name
            )
        | DHCP4 ->
            let gateway =
              Option.fold ~none:[]
                ~some:(fun n -> [`gateway n])
                !config.gateway_interface
            in
            let dns =
              Option.fold ~none:[]
                ~some:(fun n -> [`dns n])
                !config.dns_interface
            in
            if not (Dhclient.is_running name) then (* Remove any static IPs *)
              Ip.flush_ip_addr name ;
            let options = gateway @ dns in
            Dhclient.ensure_running name options
        | Static4 addrs ->
            if Dhclient.is_running name then (
              ignore (Dhclient.stop name) ;
              Ip.flush_ip_addr name
            ) ;
            (* the function is meant to be idempotent and we want to avoid
               CA-239919 *)
            let cur_addrs = Ip.get_ipv4 name in
            let rm_addrs =
              Xapi_stdext_std.Listext.List.set_difference cur_addrs addrs
            in
            let add_addrs =
              Xapi_stdext_std.Listext.List.set_difference addrs cur_addrs
            in
            List.iter (Ip.del_ip_addr name) rm_addrs ;
            List.iter (Ip.set_ip_addr name) add_addrs
      )
      ()

  let get_ipv4_gateway dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        let output = Ip.route_show ~version:Ip.V4 name in
        try
          let line =
            List.find
              (fun s -> Astring.String.is_prefix ~affix:"default via" s)
              (Astring.String.cuts ~empty:false ~sep:"\n" output)
          in
          let addr =
            List.nth (Astring.String.cuts ~empty:false ~sep:" " line) 2
          in
          Some (Unix.inet_addr_of_string addr)
        with Not_found -> None
      )
      ()

  let set_ipv4_gateway _ dbg ~name ~address =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Configuring IPv4 gateway for %s: %s" name
          (Unix.string_of_inet_addr address) ;
        update_config name {(get_config name) with ipv4_gateway= Some address} ;
        if
          !config.gateway_interface = None
          || !config.gateway_interface = Some name
        then (
          debug "%s is the default gateway interface" name ;
          Ip.set_gateway name address
        ) else
          debug "%s is NOT the default gateway interface" name
      )
      ()

  let get_ipv6_addr dbg name =
    Debug.with_thread_associated dbg (fun () -> Ip.get_ipv6 name) ()

  let set_ipv6_conf _ dbg ~name ~conf =
    Debug.with_thread_associated dbg
      (fun () ->
        if Proc.get_ipv6_disabled () then
          warn "Not configuring IPv6 address for %s (IPv6 is disabled)" name
        else (
          debug "Configuring IPv6 address for %s: %s" name
            (conf |> Rpcmarshal.marshal typ_of_ipv6 |> Jsonrpc.to_string) ;
          update_config name {(get_config name) with ipv6_conf= conf} ;
          match conf with
          | None6 ->
              if List.mem name (Sysfs.list ()) then (
                if Dhclient.is_running ~ipv6:true name then
                  ignore (Dhclient.stop ~ipv6:true name) ;
                Sysctl.set_ipv6_autoconf name false ;
                Ip.flush_ip_addr ~ipv6:true name
              )
          | Linklocal6 ->
              if List.mem name (Sysfs.list ()) then (
                if Dhclient.is_running ~ipv6:true name then
                  ignore (Dhclient.stop ~ipv6:true name) ;
                Sysctl.set_ipv6_autoconf name false ;
                Ip.flush_ip_addr ~ipv6:true name ;
                Ip.set_ipv6_link_local_addr name
              )
          | DHCP6 ->
              if Dhclient.is_running ~ipv6:true name then
                ignore (Dhclient.stop ~ipv6:true name) ;
              Sysctl.set_ipv6_autoconf name false ;
              Ip.flush_ip_addr ~ipv6:true name ;
              Ip.set_ipv6_link_local_addr name ;
              ignore (Dhclient.ensure_running ~ipv6:true name [])
          | Autoconf6 ->
              if Dhclient.is_running ~ipv6:true name then
                ignore (Dhclient.stop ~ipv6:true name) ;
              Ip.flush_ip_addr ~ipv6:true name ;
              Ip.set_ipv6_link_local_addr name ;
              Sysctl.set_ipv6_autoconf name true
              (* Cannot link set down/up due to CA-89882 - IPv4 default route
                 cleared *)
          | Static6 addrs ->
              if Dhclient.is_running ~ipv6:true name then
                ignore (Dhclient.stop ~ipv6:true name) ;
              Sysctl.set_ipv6_autoconf name false ;
              (* add the link_local and clean the old one only when needed *)
              let cur_addrs =
                let addrs = Ip.get_ipv6 name in
                let maybe_link_local =
                  Ip.split_addr (Ip.get_ipv6_link_local_addr name)
                in
                match maybe_link_local with
                | Some addr ->
                    Xapi_stdext_std.Listext.List.setify (addr :: addrs)
                | None ->
                    addrs
              in
              let rm_addrs =
                Xapi_stdext_std.Listext.List.set_difference cur_addrs addrs
              in
              let add_addrs =
                Xapi_stdext_std.Listext.List.set_difference addrs cur_addrs
              in
              List.iter (Ip.del_ip_addr name) rm_addrs ;
              List.iter (Ip.set_ip_addr name) add_addrs
        )
      )
      ()

  let get_ipv6_gateway dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        let output = Ip.route_show ~version:Ip.V6 name in
        try
          let line =
            List.find
              (fun s -> Astring.String.is_prefix ~affix:"default via" s)
              (Astring.String.cuts ~empty:false ~sep:"\n" output)
          in
          let addr =
            List.nth (Astring.String.cuts ~empty:false ~sep:" " line) 2
          in
          Some (Unix.inet_addr_of_string addr)
        with Not_found -> None
      )
      ()

  let set_ipv6_gateway _ dbg ~name ~address =
    Debug.with_thread_associated dbg
      (fun () ->
        if Proc.get_ipv6_disabled () then
          warn "Not configuring IPv6 gateway for %s (IPv6 is disabled)" name
        else (
          debug "Configuring IPv6 gateway for %s: %s" name
            (Unix.string_of_inet_addr address) ;
          update_config name {(get_config name) with ipv6_gateway= Some address} ;
          if
            !config.gateway_interface = None
            || !config.gateway_interface = Some name
          then (
            debug "%s is the default gateway interface" name ;
            Ip.set_gateway name address
          ) else
            debug "%s is NOT the default gateway interface" name
        )
      )
      ()

  let set_ipv4_routes _ dbg ~name ~routes =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Configuring IPv4 static routes for %s: %s" name
          (String.concat ", "
             (List.map
                (fun r ->
                  Printf.sprintf "%s/%d/%s"
                    (Unix.string_of_inet_addr r.subnet)
                    r.netmask
                    (Unix.string_of_inet_addr r.gateway)
                )
                routes
             )
          ) ;
        update_config name {(get_config name) with ipv4_routes= routes} ;
        List.iter
          (fun r -> Ip.set_route ~network:(r.subnet, r.netmask) name r.gateway)
          routes
      )
      ()

  let get_dns dbg _name =
    Debug.with_thread_associated dbg
      (fun () ->
        let nameservers, domains =
          Xapi_stdext_unix.Unixext.file_lines_fold
            (fun (nameservers, domains) line ->
              if Astring.String.is_prefix ~affix:"nameserver" line then
                let server =
                  List.nth (Astring.String.fields ~empty:false line) 1
                in
                (Unix.inet_addr_of_string server :: nameservers, domains)
              else if Astring.String.is_prefix ~affix:"search" line then
                let domains =
                  List.tl (Astring.String.fields ~empty:false line)
                in
                (nameservers, domains)
              else
                (nameservers, domains)
            )
            ([], []) resolv_conf
        in
        (List.rev nameservers, domains)
      )
      ()

  let set_dns _ dbg ~name ~nameservers ~domains =
    Debug.with_thread_associated dbg
      (fun () ->
        update_config name {(get_config name) with dns= (nameservers, domains)} ;
        debug "Configuring DNS for %s: nameservers: [%s]; domains: [%s]" name
          (String.concat ", " (List.map Unix.string_of_inet_addr nameservers))
          (String.concat ", " domains) ;
        if !config.dns_interface = None || !config.dns_interface = Some name
        then (
          debug "%s is the DNS interface" name ;
          let domains' =
            if domains <> [] then
              ["search " ^ String.concat " " domains]
            else
              []
          in
          let nameservers' =
            List.map
              (fun ip -> "nameserver " ^ Unix.string_of_inet_addr ip)
              nameservers
          in
          let lines = domains' @ nameservers' in
          Xapi_stdext_unix.Unixext.write_string_to_file resolv_conf
            (String.concat "\n" lines ^ "\n")
        ) else
          debug "%s is NOT the DNS interface" name
      )
      ()

  let get_mtu dbg name =
    Debug.with_thread_associated dbg (fun () -> Ip.get_mtu name) ()

  let set_mtu _ dbg ~name ~mtu =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Configuring MTU for %s: %d" name mtu ;
        update_config name {(get_config name) with mtu} ;
        match !backend_kind with
        | Openvswitch -> (
          try ignore (Ovs.set_mtu name mtu) with _ -> Ip.link_set_mtu name mtu
        )
        | Bridge ->
            Ip.link_set_mtu name mtu
      )
      ()

  let set_ethtool_settings _ dbg ~name ~params =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Configuring ethtool settings for %s: %s" name
          (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) params)) ;
        let add_defaults =
          List.filter
            (fun (k, _) -> not (List.mem_assoc k params))
            default_interface.ethtool_settings
        in
        let params = params @ add_defaults in
        update_config name {(get_config name) with ethtool_settings= params} ;
        Ethtool.set_options name params
      )
      ()

  let set_ethtool_offload _ dbg ~name ~params =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Configuring ethtool offload settings for %s: %s" name
          (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) params)) ;
        let add_defaults =
          List.filter
            (fun (k, _) -> not (List.mem_assoc k params))
            default_interface.ethtool_offload
        in
        let params = params @ add_defaults in
        update_config name {(get_config name) with ethtool_offload= params} ;
        Ethtool.set_offload name params
      )
      ()

  let get_capabilities dbg name =
    Debug.with_thread_associated dbg
      (fun () -> Fcoe.get_capabilities name @ Sriov.get_capabilities name)
      ()

  let is_connected dbg name =
    Debug.with_thread_associated dbg (fun () -> Sysfs.get_carrier name) ()

  let is_physical dbg name =
    Debug.with_thread_associated dbg (fun () -> Sysfs.is_physical name) ()

  let has_vlan dbg name vlan =
    (* Identify the vlan is used by kernel which is unknown to XAPI *)
    Debug.with_thread_associated dbg
      (fun () ->
        let temp_interfaces =
          Sysfs.bridge_to_interfaces Network_config.temp_vlan
        in
        List.exists
          (fun (d, v, p) ->
            v = vlan && p = name && not (List.mem d temp_interfaces)
          )
          (Proc.get_vlans ())
      )
      ()

  let bring_up _ dbg ~name =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Bringing up interface %s" name ;
        Ip.link_set_up name
      )
      ()

  let bring_down dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Bringing down interface %s" name ;
        Ip.link_set_down name
      )
      ()

  let set_persistent dbg name value =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Making interface %s %spersistent" name
          (if value then "" else "non-") ;
        update_config name {(get_config name) with persistent_i= value}
      )
      ()

  let make_config dbg conservative config =
    Debug.with_thread_associated dbg
      (fun () ->
        (* Only attempt to configure interfaces that exist in the system *)
        let all = get_all dbg () in
        let config = List.filter (fun (name, _) -> List.mem name all) config in
        (* Handle conservativeness *)
        let config =
          if conservative then (
            (* Do not touch non-persistent interfaces *)
            debug "Only configuring persistent interfaces" ;
            List.filter
              (fun (_name, interface) -> interface.persistent_i)
              config
          ) else
            config
        in
        let config =
          if need_enic_workaround () then
            List.fold_left
              (fun accu (name, interface) ->
                if
                  Sysfs.is_physical name
                  && Linux_bonding.get_bond_master_of name = None
                  || Linux_bonding.is_bond_device name
                then
                  (name, interface) :: (Ip.vlan_name name 0, interface) :: accu
                else
                  (name, interface) :: accu
              )
              [] config
          else
            config
        in
        debug "** Configuring the following interfaces: %s%s"
          (String.concat ", " (List.map (fun (name, _) -> name) config))
          (if conservative then " (best effort)" else "") ;
        let exec f = if conservative then try f () with _ -> () else f () in
        List.iter
          (function
            | ( name
              , ( {
                    ipv4_conf
                  ; ipv4_gateway
                  ; ipv6_conf
                  ; ipv6_gateway
                  ; ipv4_routes
                  ; dns= nameservers, domains
                  ; mtu
                  ; ethtool_settings
                  ; ethtool_offload
                  ; _
                  } as c
                ) ) ->
                update_config name c ;
                exec (fun () ->
                    (* We only apply the DNS settings when not in a DHCP mode
                       to avoid conflicts. The `dns` field
                       should really be an option type so that we don't have to
                       derive the intention of the caller by looking at other
                       fields. *)
                    match (ipv4_conf, ipv6_conf) with
                    | Static4 _, _ | _, Static6 _ | _, Autoconf6 ->
                        set_dns () dbg ~name ~nameservers ~domains
                    | _ ->
                        ()
                ) ;
                exec (fun () -> set_ipv4_conf dbg name ipv4_conf) ;
                exec (fun () ->
                    match ipv4_gateway with
                    | None ->
                        ()
                    | Some gateway ->
                        set_ipv4_gateway () dbg ~name ~address:gateway
                ) ;
                (try set_ipv6_conf () dbg ~name ~conf:ipv6_conf with _ -> ()) ;
                ( try
                    match ipv6_gateway with
                    | None ->
                        ()
                    | Some gateway ->
                        set_ipv6_gateway () dbg ~name ~address:gateway
                  with _ -> ()
                ) ;
                exec (fun () -> set_ipv4_routes () dbg ~name ~routes:ipv4_routes) ;
                exec (fun () -> set_mtu () dbg ~name ~mtu) ;
                exec (fun () -> bring_up () dbg ~name) ;
                exec (fun () ->
                    set_ethtool_settings () dbg ~name ~params:ethtool_settings
                ) ;
                exec (fun () ->
                    set_ethtool_offload () dbg ~name ~params:ethtool_offload
                )
            )
          config
      )
      ()
end

module Bridge = struct
  let add_default = ref []

  let get_config name = get_config !config.bridge_config default_bridge name

  let remove_config name =
    config :=
      {!config with bridge_config= remove_config !config.bridge_config name}

  let update_config name data =
    config :=
      {
        !config with
        bridge_config= update_config !config.bridge_config name data
      }

  let determine_backend () =
    try
      let backend =
        String.trim (Xapi_stdext_unix.Unixext.string_of_file !network_conf)
      in
      match backend with
      | "openvswitch" | "vswitch" ->
          backend_kind := Openvswitch
      | "bridge" ->
          backend_kind := Bridge
      | backend ->
          warn "Network backend unknown (%s). Falling back to Open vSwitch."
            backend ;
          backend_kind := Openvswitch
    with _ ->
      warn "Network-conf file not found. Falling back to Open vSwitch." ;
      backend_kind := Openvswitch

  let get_all dbg () =
    Debug.with_thread_associated dbg
      (fun () ->
        match !backend_kind with
        | Openvswitch ->
            Ovs.list_bridges ()
        | Bridge ->
            Sysfs.get_all_bridges ()
      )
      ()

  (* Destroy any existing OVS bridge that isn't the "wanted bridge" and has the
   * given VLAN on it. *)
  let destroy_existing_vlan_ovs_bridge dbg wanted_bridge (parent, vlan) =
    let vlan_bridges =
      let raw =
        Ovs.vsctl
          [
            "--bare"
          ; "-f"
          ; "table"
          ; "--"
          ; "--columns=name"
          ; "find"
          ; "port"
          ; "fake_bridge=true"
          ; "tag=" ^ string_of_int vlan
          ]
      in
      if raw <> "" then
        Astring.String.cuts ~empty:false ~sep:"\n" (String.trim raw)
      else
        []
    in
    let existing_bridges =
      List.filter
        (fun bridge ->
          match Ovs.bridge_to_vlan bridge with
          | Some (p, v) ->
              p = parent && v = vlan
          | None ->
              false
        )
        vlan_bridges
    in
    List.iter
      (fun bridge ->
        if bridge <> wanted_bridge then (
          debug "Destroying existing bridge %s" bridge ;
          remove_config bridge ;
          Interface.set_ipv4_conf dbg bridge None4 ;
          ignore (Ovs.destroy_bridge bridge)
        )
      )
      existing_bridges

  (* Destroy any existing Linux bridge that isn't the "wanted bridge" and has the
   * given VLAN on it. *)
  let destroy_existing_vlan_linux_bridge dbg wanted_bridge vlan_device =
    List.iter
      (fun bridge ->
        if bridge <> wanted_bridge then
          let ifaces_on_bridge = Sysfs.bridge_to_interfaces bridge in
          if List.mem vlan_device ifaces_on_bridge then (
            debug "Destroying existing bridge %s" bridge ;
            Interface.bring_down dbg bridge ;
            remove_config bridge ;
            Interface.set_ipv4_conf dbg bridge None4 ;
            List.iter
              (fun dev -> Brctl.destroy_port bridge dev)
              ifaces_on_bridge ;
            ignore (Brctl.destroy_bridge bridge)
          )
      )
      (Sysfs.get_all_bridges ())

  let create dbg vlan mac igmp_snooping other_config name =
    Debug.with_thread_associated dbg
      (fun () ->
        let other_config = match other_config with Some l -> l | None -> [] in
        debug "Creating bridge %s%s" name
          ( match vlan with
          | None ->
              ""
          | Some (parent, vlan) ->
              Printf.sprintf " (VLAN %d on bridge %s)" vlan parent
          ) ;
        update_config name
          {
            (get_config name) with
            vlan
          ; bridge_mac= mac
          ; igmp_snooping
          ; other_config
          } ;
        ( match !backend_kind with
        | Openvswitch ->
            let fail_mode =
              if not (List.mem_assoc "vswitch-controller-fail-mode" other_config)
              then
                "standalone"
              else
                let mode =
                  List.assoc "vswitch-controller-fail-mode" other_config
                in
                if mode = "secure" || mode = "standalone" then (
                  ( try
                      if mode = "secure" && Ovs.get_fail_mode name <> "secure"
                      then
                        add_default := name :: !add_default
                    with _ -> ()
                  ) ;
                  mode
                ) else (
                  debug
                    "%s isn't a valid setting for \
                     other_config:vswitch-controller-fail-mode; defaulting to \
                     'standalone'"
                    mode ;
                  "standalone"
                )
            in
            let vlan_bug_workaround =
              if List.mem_assoc "vlan-bug-workaround" other_config then
                Some (List.assoc "vlan-bug-workaround" other_config = "true")
              else
                None
            in
            let external_id =
              if List.mem_assoc "network-uuids" other_config then
                Some
                  ("xs-network-uuids", List.assoc "network-uuids" other_config)
              else
                None
            in
            let disable_in_band =
              if not (List.mem_assoc "vswitch-disable-in-band" other_config)
              then
                Some None
              else
                let dib = List.assoc "vswitch-disable-in-band" other_config in
                if dib = "true" || dib = "false" then
                  Some (Some dib)
                else (
                  debug
                    "%s isn't a valid setting for \
                     other_config:vswitch-disable-in-band"
                    dib ;
                  None
                )
            in
            let old_igmp_snooping = Ovs.get_mcast_snooping_enable ~name in
            Option.iter (destroy_existing_vlan_ovs_bridge dbg name) vlan ;
            ignore
              (Ovs.create_bridge ?mac ~fail_mode ?external_id ?disable_in_band
                 ?igmp_snooping vlan vlan_bug_workaround name
              ) ;
            if igmp_snooping = Some true && not old_igmp_snooping then
              Ovs.inject_igmp_query ~name
        | Bridge -> (
            ignore (Brctl.create_bridge name) ;
            Brctl.set_forwarding_delay name 0 ;
            Sysfs.set_multicast_snooping name false ;
            Option.iter (Ip.set_mac name) mac ;
            match vlan with
            | None ->
                ()
            | Some (parent, vlan) ->
                let bridge_interfaces = Sysfs.bridge_to_interfaces name in
                let parent_bridge_interfaces =
                  List.filter
                    (fun n ->
                      Astring.String.is_prefix ~affix:"eth" n
                      || Astring.String.is_prefix ~affix:"bond" n
                    )
                    (Sysfs.bridge_to_interfaces parent)
                in
                let parent_bridge_interface =
                  match parent_bridge_interfaces with
                  | [] ->
                      let msg =
                        Printf.sprintf
                          {|Interface for bridge parent "%s" of vlan %i not found|}
                          parent vlan
                      in
                      error "%s" msg ;
                      raise (Network_error (Internal_error msg))
                  | iface :: _ ->
                      iface
                in
                let parent_interface =
                  if need_enic_workaround () then (
                    let n = String.length parent_bridge_interface in
                    let m = String.sub parent_bridge_interface 0 (n - 2) in
                    if vlan = 0 then
                      error
                        "The enic workaround is in effect. Bridge %s is used \
                         for VLAN 0 on %s."
                        parent m ;
                    m
                  ) else
                    parent_bridge_interface
                in
                let vlan_name = Ip.vlan_name parent_interface vlan in
                destroy_existing_vlan_linux_bridge dbg name vlan_name ;
                (* Check if the VLAN is already in use by something else *)
                List.iter
                  (fun (device, vlan', parent') ->
                    (* A device for the same VLAN (parent + tag), but with a
                       different * device name or not on the requested bridge is
                       bad. *)
                    if
                      parent' = parent
                      && vlan' = vlan
                      && (device <> vlan_name
                         || not (List.mem device bridge_interfaces)
                         )
                    then
                      raise (Network_error (Vlan_in_use (parent, vlan)))
                  )
                  (Proc.get_vlans ()) ;
                (* Robustness enhancement: ensure there are no other VLANs in
                   the bridge *)
                let current_interfaces =
                  List.filter
                    (fun n ->
                      Astring.String.is_prefix ~affix:"eth" n
                      || Astring.String.is_prefix ~affix:"bond" n
                    )
                    bridge_interfaces
                in
                debug
                  "Removing these non-VIF interfaces found on the bridge: %s"
                  (String.concat ", " current_interfaces) ;
                List.iter
                  (fun interface ->
                    Brctl.destroy_port name interface ;
                    Interface.bring_down dbg interface
                  )
                  current_interfaces ;
                (* Now create the new VLAN device and add it to the bridge *)
                Ip.create_vlan parent_interface vlan ;
                Interface.bring_up () dbg ~name:vlan_name ;
                Brctl.create_port name vlan_name
          )
        ) ;
        Interface.bring_up () dbg ~name
      )
      ()

  let destroy dbg force name =
    Debug.with_thread_associated dbg
      (fun () ->
        Interface.bring_down dbg name ;
        match !backend_kind with
        | Openvswitch ->
            let vlans_on_this_parent = Ovs.get_vlans name in
            if vlans_on_this_parent = [] || force then (
              debug "Destroying bridge %s" name ;
              remove_config name ;
              let interfaces =
                Ovs.bridge_to_interfaces name @ vlans_on_this_parent
              in
              List.iter
                (fun dev ->
                  Interface.set_ipv4_conf dbg dev None4 ;
                  Interface.bring_down dbg dev
                )
                interfaces ;
              Interface.set_ipv4_conf dbg name None4 ;
              ignore (Ovs.destroy_bridge name)
            ) else
              debug "Not destroying bridge %s, because it has VLANs on top" name
        | Bridge ->
            let ifs = Sysfs.bridge_to_interfaces name in
            let vlans_on_this_parent =
              let interfaces =
                List.filter
                  (fun n ->
                    Astring.String.is_prefix ~affix:"eth" n
                    || Astring.String.is_prefix ~affix:"bond" n
                  )
                  ifs
              in
              match interfaces with
              | [] ->
                  []
              | interface :: _ ->
                  List.filter
                    (Astring.String.is_prefix ~affix:(interface ^ "."))
                    (Sysfs.list ())
            in
            if vlans_on_this_parent = [] || force then (
              debug "Destroying bridge %s" name ;
              remove_config name ;
              List.iter
                (fun dev ->
                  Interface.set_ipv4_conf dbg dev None4 ;
                  Brctl.destroy_port name dev ;
                  Interface.bring_down dbg dev ;
                  if Linux_bonding.is_bond_device dev then
                    Linux_bonding.remove_bond_master dev ;
                  if
                    (Astring.String.is_prefix ~affix:"eth" dev
                    || Astring.String.is_prefix ~affix:"bond" dev
                    )
                    && String.contains dev '.'
                  then (
                    ignore (Ip.destroy_vlan dev) ;
                    let n = String.length dev in
                    if
                      String.sub dev (n - 2) 2 = ".0" && need_enic_workaround ()
                    then
                      let vlan_base = String.sub dev 0 (n - 2) in
                      if Linux_bonding.is_bond_device vlan_base then
                        Linux_bonding.remove_bond_master
                          (String.sub dev 0 (n - 2))
                  )
                )
                ifs ;
              Interface.set_ipv4_conf dbg name None4 ;
              ignore (Brctl.destroy_bridge name)
            ) else
              debug "Not destroying bridge %s, because it has VLANs on top" name
      )
      ()

  let get_kind dbg () =
    Debug.with_thread_associated dbg (fun () -> !backend_kind) ()

  let get_all_ports dbg from_cache =
    Debug.with_thread_associated dbg
      (fun () ->
        if from_cache then
          let ports =
            List.concat
              (List.map (fun (_, {ports; _}) -> ports) !config.bridge_config)
          in
          List.map (fun (port, {interfaces; _}) -> (port, interfaces)) ports
        else
          match !backend_kind with
          | Openvswitch ->
              List.concat (List.map Ovs.bridge_to_ports (Ovs.list_bridges ()))
          | Bridge ->
              raise (Network_error Not_implemented)
      )
      ()

  let get_all_bonds dbg from_cache =
    Debug.with_thread_associated dbg
      (fun () ->
        if from_cache then
          let ports =
            List.concat
              (List.map (fun (_, {ports; _}) -> ports) !config.bridge_config)
          in
          let names =
            List.map (fun (port, {interfaces; _}) -> (port, interfaces)) ports
          in
          List.filter (fun (_, ifs) -> List.length ifs > 1) names
        else
          match !backend_kind with
          | Openvswitch ->
              List.concat (List.map Ovs.bridge_to_ports (Ovs.list_bridges ()))
          | Bridge ->
              raise (Network_error Not_implemented)
      )
      ()

  type bond_link_info = {slave: iface; up: bool; active: bool}

  let get_bond_link_info _ dbg ~name =
    Debug.with_thread_associated dbg
      (fun () ->
        match !backend_kind with
        | Openvswitch ->
            let slaves, active_slave = Ovs.get_bond_link_status name in
            let mode = Ovs.get_bond_mode name in
            List.map
              (fun (slave, up) ->
                let active =
                  let ab = mode = Some "active-backup" in
                  (ab && active_slave = Some slave) || ((not ab) && up)
                in
                {slave; up; active}
              )
              slaves
        | Bridge ->
            let active_slave = Linux_bonding.get_bond_active_slave name in
            let slaves = Proc.get_bond_slave_info name "MII Status" in
            let bond_props = Linux_bonding.get_bond_properties name in
            List.map
              (fun (slave, status) ->
                let up = status = "up" in
                let active =
                  let ab =
                    List.mem_assoc "mode" bond_props
                    && Astring.String.is_prefix ~affix:"active-backup"
                         (List.assoc "mode" bond_props)
                  in
                  (ab && active_slave = Some slave) || ((not ab) && up)
                in
                {slave; up; active}
              )
              slaves
      )
      ()

  let add_default_flows _ dbg bridge mac interfaces =
    Debug.with_thread_associated dbg
      (fun () ->
        match !backend_kind with
        | Openvswitch ->
            Ovs.add_default_flows bridge mac interfaces
        | Bridge ->
            ()
      )
      ()

  let add_basic_port dbg bridge name {interfaces; bond_mac; bond_properties; _}
      =
    match !backend_kind with
    | Openvswitch -> (
        ( match interfaces with
        | [iface] ->
            Interface.bring_up () dbg ~name:iface ;
            ignore (Ovs.create_port iface bridge)
        | _ ->
            if bond_mac = None then
              warn "No MAC address specified for the bond" ;
            ignore
              (Ovs.create_bond ?mac:bond_mac name interfaces bridge
                 bond_properties
              ) ;
            List.iter (fun name -> Interface.bring_up () dbg ~name) interfaces
        ) ;
        if List.mem bridge !add_default then
          let mac =
            match bond_mac with
            | None -> (
              try Some (Ip.get_mac name) with _ -> None
            )
            | Some mac ->
                Some mac
          in
          match mac with
          | Some mac ->
              add_default_flows () dbg bridge mac interfaces ;
              add_default := List.filter (( <> ) bridge) !add_default
          | None ->
              warn
                "Could not add default flows for port %s on bridge %s because \
                 no MAC address was specified"
                name bridge
      )
    | Bridge ->
        ( match interfaces with
        | [iface] ->
            Interface.bring_up () dbg ~name:iface
        | _ ->
            Linux_bonding.add_bond_master name ;
            let bond_properties =
              match List.assoc_opt "mode" bond_properties with
              | Some "lacp" ->
                  Xapi_stdext_std.Listext.List.replace_assoc "mode" "802.3ad"
                    bond_properties
              | _ ->
                  bond_properties
            in
            Linux_bonding.set_bond_properties name bond_properties ;
            Linux_bonding.set_bond_slaves name interfaces ;
            ( match bond_mac with
            | Some mac ->
                Ip.set_mac name mac
            | None ->
                warn "No MAC address specified for the bond"
            ) ;
            Interface.bring_up () dbg ~name
        ) ;
        if need_enic_workaround () then (
          debug "Applying enic workaround: adding VLAN0 device to bridge" ;
          Ip.create_vlan name 0 ;
          let vlan0 = Ip.vlan_name name 0 in
          Interface.bring_up () dbg ~name:vlan0 ;
          ignore (Brctl.create_port bridge vlan0)
        ) else
          ignore (Brctl.create_port bridge name)

  let add_pvs_proxy_port dbg bridge name _port =
    match !backend_kind with
    | Openvswitch ->
        ignore (Ovs.create_port ~internal:true name bridge) ;
        let real_bridge = Ovs.get_real_bridge bridge in
        Ovs.mod_port real_bridge name "no-flood" ;
        Interface.bring_up () dbg ~name
    | Bridge ->
        raise (Network_error Not_implemented)

  let add_port dbg bond_mac bridge name interfaces bond_properties kind =
    Debug.with_thread_associated dbg
      (fun () ->
        let bond_properties =
          match bond_properties with Some l -> l | None -> []
        in
        let kind = match kind with Some v -> v | None -> Basic_port in
        let config = get_config bridge in
        let ports =
          if List.mem_assoc name config.ports then
            List.remove_assoc name config.ports
          else
            config.ports
        in
        let port = {interfaces; bond_mac; bond_properties; kind} in
        let ports = (name, port) :: ports in
        update_config bridge {config with ports} ;
        debug "Adding %s port %s to bridge %s with interface(s) %s%s"
          (string_of_port_kind kind) name bridge
          (String.concat ", " interfaces)
          (match bond_mac with Some mac -> " and MAC " ^ mac | None -> "") ;
        match kind with
        | Basic_port ->
            add_basic_port dbg bridge name port
        | PVS_proxy ->
            add_pvs_proxy_port dbg bridge name port
      )
      ()

  let remove_port dbg bridge name =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Removing port %s from bridge %s" name bridge ;
        let config = get_config bridge in
        ( if List.mem_assoc name config.ports then
            let ports = List.remove_assoc name config.ports in
            update_config bridge {config with ports}
        ) ;
        match !backend_kind with
        | Openvswitch ->
            ignore (Ovs.destroy_port name)
        | Bridge ->
            ignore (Brctl.destroy_port bridge name)
      )
      ()

  let get_interfaces dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        match !backend_kind with
        | Openvswitch ->
            Ovs.bridge_to_interfaces name
        | Bridge ->
            Sysfs.bridge_to_interfaces name
      )
      ()

  let get_physical_interfaces dbg name =
    Debug.with_thread_associated dbg
      (fun () ->
        match !backend_kind with
        | Openvswitch ->
            Ovs.get_real_bridge name
            |> Ovs.bridge_to_interfaces
            |> List.filter Sysfs.is_physical
        | Bridge -> (
            let ifaces = Sysfs.bridge_to_interfaces name in
            let vlan_ifaces =
              List.filter
                (fun (bridge, _, _) -> List.mem bridge ifaces)
                (Proc.get_vlans ())
            in
            let bond_ifaces = List.filter Linux_bonding.is_bond_device ifaces in
            let physical_ifaces = List.filter Sysfs.is_physical ifaces in
            match (vlan_ifaces, bond_ifaces) with
            | (_, _, parent) :: _, _ when Linux_bonding.is_bond_device parent ->
                Linux_bonding.get_bond_slaves parent
            | (_, _, parent) :: _, _ ->
                [parent]
            | _, bond_iface :: _ ->
                Linux_bonding.get_bond_slaves bond_iface
            | [], [] ->
                physical_ifaces
          )
      )
      ()

  let set_persistent dbg name value =
    Debug.with_thread_associated dbg
      (fun () ->
        debug "Making bridge %s %spersistent" name (if value then "" else "non-") ;
        update_config name {(get_config name) with persistent_b= value}
      )
      ()

  let make_config dbg conservative config =
    Debug.with_thread_associated dbg
      (fun () ->
        let vlans_go_last (_, {vlan= vlan_of_a; _}) (_, {vlan= vlan_of_b; _}) =
          if vlan_of_a = None && vlan_of_b = None then
            0
          else if vlan_of_a <> None && vlan_of_b = None then
            1
          else if vlan_of_a = None && vlan_of_b <> None then
            -1
          else
            0
        in
        let config =
          if conservative then (
            let persistent_config =
              List.filter (fun (_name, bridge) -> bridge.persistent_b) config
            in
            debug "Ensuring the following persistent bridges are up: %s"
              (String.concat ", "
                 (List.map (fun (name, _) -> name) persistent_config)
              ) ;
            let vlan_parents =
              List.filter_map
                (function
                  | _, {vlan= Some (parent, _); _} ->
                      if not (List.mem_assoc parent persistent_config) then
                        Some (parent, List.assoc parent config)
                      else
                        None
                  | _ ->
                      None
                  )
                persistent_config
            in
            debug
              "Additionally ensuring the following VLAN parent bridges are up: \
               %s"
              (String.concat ", "
                 (List.map (fun (name, _) -> name) vlan_parents)
              ) ;
            let config = vlan_parents @ persistent_config in
            (* Do not try to recreate bridges that already exist *)
            let current = get_all dbg () in
            List.filter
              (function name, _ -> not (List.mem name current))
              config
          ) else
            config
        in
        let config = List.sort vlans_go_last config in
        let exec f = if conservative then try f () with _ -> () else f () in
        debug "** Configuring the following bridges: %s%s"
          (String.concat ", " (List.map (fun (name, _) -> name) config))
          (if conservative then " (best effort)" else "") ;
        List.iter
          (function
            | ( bridge_name
              , ({ports; vlan; bridge_mac; igmp_snooping; other_config; _} as c)
              ) ->
                update_config bridge_name c ;
                exec (fun () ->
                    create dbg vlan bridge_mac igmp_snooping (Some other_config)
                      bridge_name ;
                    List.iter
                      (fun ( port_name
                           , {interfaces; bond_properties; bond_mac; kind}
                           ) ->
                        add_port dbg bond_mac bridge_name port_name interfaces
                          (Some bond_properties) (Some kind)
                      )
                      ports
                )
            )
          config
      )
      ()
end

module PVS_proxy = struct
  open S.PVS_proxy

  let path = ref "/opt/citrix/pvsproxy/socket/pvsproxy"

  let do_call call =
    try Jsonrpc_client.with_rpc ~path:!path ~call ()
    with e ->
      error "Error when calling PVS proxy: %s" (Printexc.to_string e) ;
      raise (Network_error PVS_proxy_connection_error)

  let configure_site _dbg config =
    debug "Configuring PVS proxy for site %s" config.site_uuid ;
    let call =
      {
        Rpc.name= "configure_site"
      ; params= [Rpcmarshal.marshal t.ty config]
      ; is_notification= false
      }
    in
    let _ = do_call call in
    ()

  let remove_site _dbg uuid =
    debug "Removing PVS proxy for site %s" uuid ;
    let call =
      Rpc.
        {
          name= "remove_site"
        ; params=
            [Dict [("site_uuid", Rpcmarshal.marshal Rpc.Types.string.ty uuid)]]
        ; is_notification= false
        }
    in

    let _ = do_call call in
    ()
end

let on_startup () =
  let dbg = "startup" in
  Debug.with_thread_associated dbg
    (fun () ->
      Bridge.determine_backend () ;
      let remove_centos_config () =
        (* Remove DNSDEV and GATEWAYDEV from Centos networking file, because the
           interfere * with this daemon. *)
        try
          let file =
            String.trim
              (Xapi_stdext_unix.Unixext.string_of_file "/etc/sysconfig/network")
          in
          let args = Astring.String.cuts ~empty:false ~sep:"\n" file in
          let args =
            List.map
              (fun s ->
                match Astring.String.cuts ~sep:"=" s with
                | [k; v] ->
                    (k, v)
                | _ ->
                    ("", "")
              )
              args
          in
          let args =
            List.filter (fun (k, _) -> k <> "DNSDEV" && k <> "GATEWAYDEV") args
          in
          let s =
            String.concat "\n" (List.map (fun (k, v) -> k ^ "=" ^ v) args)
            ^ "\n"
          in
          Xapi_stdext_unix.Unixext.write_string_to_file "/etc/sysconfig/network"
            s
        with _ -> ()
      in
      try
        (* the following is best-effort *)
        read_config () ;
        remove_centos_config () ;
        if !backend_kind = Openvswitch then
          Ovs.set_max_idle 5000 ;
        Bridge.make_config dbg true !config.bridge_config ;
        Interface.make_config dbg true !config.interface_config ;
        (* If there is still a network.dbcache file, move it out of the way. *)
        if
          try
            Unix.access
              (Filename.concat "/var/lib/xcp" "network.dbcache")
              [Unix.F_OK] ;
            true
          with _ -> false
        then
          Unix.rename
            (Filename.concat "/var/lib/xcp" "network.dbcache")
            (Filename.concat "/var/lib/xcp" "network.dbcache.bak")
      with e ->
        debug "Error while configuring networks on startup: %s\n%s"
          (Printexc.to_string e)
          (Printexc.get_backtrace ())
    )
    ()
