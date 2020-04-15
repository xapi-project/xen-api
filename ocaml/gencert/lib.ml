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
  try
    if iface = "" || not @@ Net.Interface.exists dbg iface then
      None
    else
      let addrs = match String.lowercase_ascii (Inventory.lookup Inventory._management_address_type ~default:"ipv4") with
        | "ipv4" -> Net.Interface.get_ipv4_addr dbg iface
        | "ipv6" -> Net.Interface.get_ipv6_addr dbg iface
        | s      -> raise (Unexpected_address_type (Printf.sprintf "Expected 'ipv4' or 'ipv6', got %s" s))
      in
      let addrs = List.map (fun (addr, _) -> Unix.string_of_inet_addr addr) addrs in
      (* Filter out link-local addresses *)
      let addrs = List.filter (fun addr -> String.sub addr 0 4 <> "fe80") addrs in
      Some (List.hd addrs)
  with _ -> None
