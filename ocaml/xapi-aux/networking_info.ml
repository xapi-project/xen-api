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

type management_ip_error =
  | Interface_missing
  | Unexpected_address_type of string
  | IP_missing
  | Other of exn

let management_ip_error_to_string = function
  | Interface_missing ->
      "Management interface is missing"
  | IP_missing ->
      "Management IP is missing"
  | Unexpected_address_type s ->
      Printf.sprintf
        "Unexpected address type. Expected 'ipv4' or 'ipv6', got %s" s
  | Other e ->
      Printexc.to_string e

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

let get_management_ip_addrs ~dbg =
  let iface = Inventory.lookup Inventory._management_interface in
  try
    if iface = "" || (not @@ Net.Interface.exists dbg iface) then
      Error Interface_missing
    else
      let ( let* ) = Result.bind in
      let* addrs =
        let ipv4 = Net.Interface.get_ipv4_addr dbg iface in
        let ipv6 = Net.Interface.get_ipv6_addr dbg iface in
        match
          String.lowercase_ascii
            (Inventory.lookup Inventory._management_address_type ~default:"ipv4")
        with
        | "ipv4" ->
            Ok (ipv4, ipv6)
        | "ipv6" ->
            Ok (ipv6, ipv4)
        | s ->
            Error (Unexpected_address_type s)
      in
      (* Filter out link-local addresses *)
      let no_local (addr, _) =
        let addr = Ipaddr_unix.of_inet_addr addr in
        if Ipaddr.scope addr <> Ipaddr.Link then
          Some addr
        else
          None
      in
      Ok
        ( List.filter_map no_local (fst addrs)
        , List.filter_map no_local (snd addrs)
        )
  with e -> Error (Other e)

let get_management_ip_addr ~dbg =
  match get_management_ip_addrs ~dbg with
  | Ok (preferred, _) ->
      List.nth_opt preferred 0
      |> Option.map (fun addr -> (Ipaddr.to_string addr, ipaddr_to_cstruct addr))
  | Error _ ->
      None

let get_host_certificate_subjects ~dbg =
  let ( let* ) = Result.bind in
  let* ips, preferred_ip =
    match get_management_ip_addrs ~dbg with
    | Error e ->
        Error e
    | Ok (preferred, others) ->
        let ips = List.(rev_append (rev preferred) others) in
        Option.fold ~none:(Error IP_missing)
          ~some:(fun ip -> Ok (List.map ipaddr_to_cstruct ips, ip))
          (List.nth_opt ips 0)
  in
  let dns_names = dns_names () in
  let name =
    match dns_names with [] -> Ipaddr.to_string preferred_ip | dns :: _ -> dns
  in
  Ok (name, dns_names, ips)
