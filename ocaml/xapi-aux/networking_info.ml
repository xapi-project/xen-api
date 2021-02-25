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
module Net = Network_client.Client

let get_hostname () = try Unix.gethostname () with _ -> ""

exception Unexpected_address_type of string

(* Try to get all FQDNs, avoid localhost *)
let dns_names () =
  let hostname = get_hostname () in
  let fqdns =
    Unix.getaddrinfo hostname "" [Unix.AI_CANONNAME]
    |> List.map (fun x -> x.Unix.ai_canonname)
  in
  hostname :: fqdns
  |> List.filter_map (fun x ->
         let x = Astring.String.trim x in
         if
           String.equal "" x
           || String.equal "localhost" x
           || Ipaddr.of_string x |> Stdlib.Result.is_ok
         then
           None
         else
           Some x)
  |> Astring.String.uniquify

let ip_addr_of_string ip =
  Ipaddr.of_string ip
  |> Stdlib.Result.to_option
  |> Option.map (function
       | Ipaddr.V4 addr ->
           Cstruct.of_string (Ipaddr.V4.to_octets addr)
       | Ipaddr.V6 addr ->
           Cstruct.of_string (Ipaddr.V6.to_octets addr))

let get_management_ip_addr ~dbg =
  let iface = Inventory.lookup Inventory._management_interface in
  try
    if iface = "" || (not @@ Net.Interface.exists dbg iface) then
      None
    else
      let addrs =
        match
          String.lowercase_ascii
            (Inventory.lookup Inventory._management_address_type
               ~default:"ipv4")
        with
        | "ipv4" ->
            Net.Interface.get_ipv4_addr dbg iface
        | "ipv6" ->
            Net.Interface.get_ipv6_addr dbg iface
        | s ->
            raise
              (Unexpected_address_type
                 (Printf.sprintf "Expected 'ipv4' or 'ipv6', got %s" s))
      in
      let addrs =
        addrs
        |> List.map (fun (addr, _) -> Unix.string_of_inet_addr addr)
        |> (* Filter out link-local addresses *)
        List.filter (fun addr -> String.sub addr 0 4 <> "fe80")
        |> List.map (fun str ->
               Option.map (fun bytes -> (str, bytes)) (ip_addr_of_string str))
      in
      Option.join (List.nth_opt addrs 0)
  with _ -> None
