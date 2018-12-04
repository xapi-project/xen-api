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

open Network_interface

let name = "networkd_db"

(* catch signals for clean shutdown *)
let stop _signal =
  exit 0

let handle_shutdown () =
  Sys.set_signal Sys.sigterm (Sys.Signal_handle stop);
  Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let _ =
  let bridge = ref "" in
  let iface = ref "" in
  let rc = ref 0 in
  Arg.parse (Arg.align [
      "-bridge", Arg.Set_string bridge, "Bridge name";
      "-iface", Arg.Set_string iface, "Interface name";
    ])
    (fun _ -> failwith "Invalid argument")
    (Printf.sprintf "Usage: %s [-bridge <bridge> | -iface <interface>]" name);

  try
    Coverage.init "network_db";
    let config = Network_config.read_config () in
    if !bridge <> "" then
      if List.mem_assoc !bridge config.bridge_config then begin
        let bridge_config = List.assoc !bridge config.bridge_config in
        let ifaces = List.flatten (List.map (fun (_, port) -> port.interfaces) bridge_config.ports) in
        Printf.printf "interfaces=%s\n" (String.concat "," ifaces);
        begin match bridge_config.vlan with
          | None -> ()
          | Some (parent, id) -> Printf.printf "vlan=%d\nparent=%s\n" id parent
        end
      end else begin
        rc := 1;
        Printf.fprintf stderr "Could not find bridge %s\n" !bridge;
      end;
    if !iface <> "" then
      if List.mem_assoc !iface config.interface_config then begin
        let interface_config = List.assoc !iface config.interface_config in
        let datav4 = match interface_config.ipv4_conf with
          | DHCP4 ->
            ["mode", "dhcp"]
          | Static4 conf ->
            let mode = ["mode", "static"] in
            let addrs =
              List.flatten (List.map (fun (ip, plen) ->
                  ["ipaddr", Unix.string_of_inet_addr ip; "netmask", prefixlen_to_netmask plen]
                ) conf)
            in
            let gateway = match interface_config.ipv4_gateway with
              | None -> []
              | Some addr -> ["gateway", Unix.string_of_inet_addr addr]
            in
            let dns =
              let dns' = List.map Unix.string_of_inet_addr (fst interface_config.dns) in
              if dns' = [] then
                []
              else
                ["dns", String.concat "," dns']
            in
            let domains =
              let domains' = snd interface_config.dns in
              if domains' = [] then
                []
              else
                ["domain", String.concat "," domains']
            in
            mode @ addrs @ gateway @ dns @ domains
          | None4 -> []
        in
        let datav6 = match interface_config.ipv6_conf with
          | DHCP6 ->
            ["modev6", "dhcp"]
          | Autoconf6 ->
            ["modev6", "autoconf"]
          | Static6 conf ->
            let mode = ["modev6", "static"] in
            let addrs =
              List.flatten (List.map (fun (ip, plen) ->
                  ["ipv6addr", (Unix.string_of_inet_addr ip) ^ "/" ^ (string_of_int plen)]
                ) conf)
            in
            let gateway = match interface_config.ipv6_gateway with
              | None -> []
              | Some addr -> ["gatewayv6", Unix.string_of_inet_addr addr]
            in
            mode @ addrs @ gateway
          | None6 | Linklocal6 -> []
        in
        let data = datav4 @ datav6 in
        List.iter (fun (k, v) -> Printf.printf "%s=%s\n" k v) data
      end else begin
        rc := 1;
        Printf.fprintf stderr "Could not find interface %s\n" !iface;
      end;
  with Network_config.Read_error ->
    Printf.fprintf stderr "Failed to read %s\n" name;
    exit !rc;
