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

module D = Debug.Make (struct let name = "gencert_lib" end)

exception Unexpected_address_type of string

(* Try to get all FQDNs, use the hostname if none are available *)
let hostnames () =
  let hostname = Unix.gethostname () in
  let fqdns =
    Unix.getaddrinfo hostname "" [Unix.AI_CANONNAME]
    |> List.filter_map (fun addrinfo ->
           match addrinfo.Unix.ai_canonname with
           | "" ->
               None
           | name ->
               Some name)
  in
  match fqdns with [] -> [hostname] | fqdns -> fqdns

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
        List.map (fun (addr, _) -> Unix.string_of_inet_addr addr) addrs
      in
      (* Filter out link-local addresses *)
      let addrs =
        List.filter (fun addr -> String.sub addr 0 4 <> "fe80") addrs
      in
      Some (List.hd addrs)
  with _ -> None

open Api_errors
open Rresult
open Validation

type t_certificate = Leaf | Chain

let install_server_certificate ?(pem_chain = None) ~pem_leaf ~pkcs8_private_key
    ~server_cert_path =
  let now = Ptime_clock.now () in
  validate_private_key pkcs8_private_key >>= fun priv ->
  validate_certificate Leaf pem_leaf now priv >>= fun cert ->
  Option.fold
    ~none:(Ok [pkcs8_private_key; pem_leaf])
    ~some:(fun pem_chain ->
      validate_certificate Chain pem_chain now priv >>= fun _ignored ->
      Ok [pkcs8_private_key; pem_leaf; pem_chain])
    pem_chain
  >>= fun server_cert_components ->
  server_cert_components
  |> String.concat "\n\n"
  |> Selfcert.write_certs server_cert_path
  |> R.reword_error (function `Msg msg -> `Msg (internal_error, [msg]))
  >>= fun () -> R.ok cert
