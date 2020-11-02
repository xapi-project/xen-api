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

let install_server_certificate ?(pem_chain = None) ~pem_leaf ~pkcs8_private_key
    ~server_cert_path ~ca_cert_bundle_path =
  let ( let* ) = ( >>= ) in

  let now = Ptime_clock.now () in
  let pem_intermediates = Option.value ~default:"" pem_chain in
  let* server, trust_roots =
    validating_trust_anchors ~pem_leaf ~pkcs8_private_key ~pem_intermediates
      ~time:now ~ca_cert_bundle_path
  in

  let root_fingerprints =
    List.map
      (X509.Certificate.fingerprint Mirage_crypto.Hash.(`SHA256))
      trust_roots
  in

  (* construct the PKCS12 file contents and write them to disk *)
  let pem_chain_list = Option.fold ~none:[] ~some:(fun c -> [c]) pem_chain in
  let server_cert_components =
    pkcs8_private_key :: pem_leaf :: pem_chain_list
  in
  let* () =
    server_cert_components
    |> String.concat "\n\n"
    |> Selfcert.write_certs server_cert_path
    |> R.reword_error (function `Msg msg -> `Msg (internal_error, [msg]))
  in
  Ok (server, root_fingerprints)
