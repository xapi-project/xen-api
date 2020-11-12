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

module D = Debug.Make (struct let name = "gencert_validation" end)

exception Unexpected_address_type of string

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
