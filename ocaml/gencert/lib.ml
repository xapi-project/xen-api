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

let fqdns_of_hostname hostname =
  Unix.getaddrinfo hostname "" [Unix.AI_CANONNAME]
  |> List.filter_map (fun addrinfo ->
         match addrinfo.Unix.ai_canonname with "" -> None | name -> Some name)

let fqdn_of_hostname hostname =
  fqdns_of_hostname hostname |> function [] -> None | x :: _ -> Some x

(* Try to get all FQDNs, use the hostname if none are available *)
let hostnames () =
  let hostname = Unix.gethostname () in
  match fqdns_of_hostname hostname with [] -> [hostname] | fqdns -> fqdns

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

type t_certificate = Leaf | Chain

let validate_private_key pkcs8_private_key =
  let ensure_key_length = function
    | `RSA priv ->
        let length = Mirage_crypto_pk.Rsa.priv_bits priv in
        if length < 2048 || length > 4096 then
          Error
            (`Msg
              ( server_certificate_key_rsa_length_not_supported
              , [Int.to_string length] ))
        else
          Ok (`RSA priv)
  in
  let raw_pem = Cstruct.of_string pkcs8_private_key in
  X509.Private_key.decode_pem raw_pem
  |> R.reword_error (fun (`Msg err_msg) ->
         let unknown_algorithm = "Unknown algorithm " in
         if Astring.String.is_prefix ~affix:"multi-prime RSA" err_msg then
           `Msg (server_certificate_key_rsa_multi_not_supported, [])
         else if Astring.String.is_prefix ~affix:unknown_algorithm err_msg then
           `Msg
             ( server_certificate_key_algorithm_not_supported
             , [
                 Astring.String.with_range
                   ~first:(String.length unknown_algorithm)
                   err_msg
               ] )
         else (
           D.info {|Failed to validate private key because "%s"|} err_msg ;
           `Msg (server_certificate_key_invalid, [])))
  >>= ensure_key_length

let validate_certificate kind pem now private_key =
  let ensure_keys_match private_key certificate =
    let public_key = X509.Certificate.public_key certificate in
    match (public_key, private_key) with
    | `RSA pub, `RSA priv when pub = Mirage_crypto_pk.Rsa.pub_of_priv priv ->
        Ok certificate
    | _ ->
        Error (`Msg (server_certificate_key_mismatch, []))
  in
  let ensure_validity ~time certificate =
    let to_string = Ptime.to_rfc3339 ~tz_offset_s:0 in
    let not_before, not_after = X509.Certificate.validity certificate in
    if Ptime.is_earlier ~than:not_before time then
      Error
        (`Msg
          ( server_certificate_not_valid_yet
          , [to_string time; to_string not_before] ))
    else if Ptime.is_later ~than:not_after time then
      Error
        (`Msg
          (server_certificate_expired, [to_string time; to_string not_after]))
    else
      Ok certificate
  in
  let ensure_sha256_signature_algorithm certificate =
    match X509.Certificate.signature_algorithm certificate with
    | Some (_, `SHA256) ->
        Ok certificate
    | _ ->
        Error (`Msg (server_certificate_signature_not_supported, []))
  in
  let raw_pem = Cstruct.of_string pem in
  match kind with
  | Leaf ->
      X509.Certificate.decode_pem raw_pem
      |> R.reword_error (fun (`Msg err_msg) ->
          D.info {|Failed to validate certificate because "%s"|} err_msg ;
          `Msg (server_certificate_invalid, []))
      >>= ensure_keys_match private_key
      >>= ensure_validity ~time:now
      >>= ensure_sha256_signature_algorithm
  | Chain -> (
      X509.Certificate.decode_pem_multiple raw_pem |> function
      | Ok (cert :: _) ->
          Ok cert
      | Ok [] ->
          D.info "Rejected certificate chain because it's empty." ;
          Error (`Msg (server_certificate_chain_invalid, []))
      | Error (`Msg err_msg) ->
          D.info {|Failed to validate certificate chain because "%s"|} err_msg ;
          Error (`Msg (server_certificate_chain_invalid, []))
    )

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
