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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

module Net = Network_client.Client

exception Unexpected_address_type of string

let get_management_ip_addr ~dbg =
  let iface = Inventory.lookup Inventory._management_interface in
  let primary_address_type =
    match String.lowercase_ascii (Inventory.lookup Inventory._management_address_type ~default:"ipv4") with
    | "ipv4" -> `IPv4
    | "ipv6" -> `IPv6
    | s      -> raise (Unexpected_address_type (Printf.sprintf "Expected 'ipv4' or 'ipv6', got %s" s))
  in
  if iface = "" then
    None
  else
    try
      if Net.Interface.exists dbg iface then
        let addrs = match primary_address_type with
          | `IPv4 -> Net.Interface.get_ipv4_addr dbg iface
          | `IPv6 -> Net.Interface.get_ipv6_addr dbg iface
        in
        let addrs = List.map (fun (addr, _) -> Unix.string_of_inet_addr addr) addrs in
        (* Filter out link-local addresses *)
        let addrs = List.filter (fun addr -> String.sub addr 0 4 <> "fe80") addrs in
        Some (List.hd addrs)
      else
        None
    with _ -> None
