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

module L = Debug.Make (struct let name = __MODULE__ end)

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
           Some x
     )
  |> Astring.String.uniquify

let ipaddr_to_cstruct = function
  | Ipaddr.V4 addr ->
      Cstruct.of_string (Ipaddr.V4.to_octets addr)
  | Ipaddr.V6 addr ->
      Cstruct.of_string (Ipaddr.V6.to_octets addr)

let list_head lst = List.nth_opt lst 0

let get_management_ip_addr ~dbg =
  let iface = Inventory.lookup Inventory._management_interface in
  try
    if iface = "" || (not @@ Net.Interface.exists dbg iface) then
      None
    else
      let addrs =
        match
          String.lowercase_ascii
            (Inventory.lookup Inventory._management_address_type ~default:"ipv4")
        with
        | "ipv4" ->
            Net.Interface.get_ipv4_addr dbg iface
        | "ipv6" ->
            Net.Interface.get_ipv6_addr dbg iface
        | s ->
            let msg = Printf.sprintf "Expected 'ipv4' or 'ipv6', got %s" s in
            L.error "%s: %s" __FUNCTION__ msg ;
            raise (Unexpected_address_type msg)
      in
      addrs
      |> List.map (fun (addr, _) -> Ipaddr_unix.of_inet_addr addr)
      (* Filter out link-local addresses *)
      |> List.filter (fun addr -> Ipaddr.scope addr <> Ipaddr.Link)
      |> List.map (fun ip -> (Ipaddr.to_string ip, ipaddr_to_cstruct ip))
      |> list_head
  with _ -> None
