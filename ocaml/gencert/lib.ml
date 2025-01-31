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

module D = Debug.Make (struct let name = "gencert_lib" end)

open Api_errors
open Rresult

let validate_private_key pkcs8_private_key =
  let ensure_rsa_key_length = function
    | `RSA priv ->
        let length = Mirage_crypto_pk.Rsa.priv_bits priv in
        if length < 2048 || length > 4096 then
          Error
            (`Msg
              ( server_certificate_key_rsa_length_not_supported
              , [Int.to_string length]
              )
              )
        else
          Ok (`RSA priv)
    | key ->
        let key_type = X509.(Key_type.to_string (Private_key.key_type key)) in
        Error (`Msg (server_certificate_key_algorithm_not_supported, [key_type]))
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
               ]
             )
         else (
           D.info {|Failed to validate private key because "%s"|} err_msg ;
           `Msg (server_certificate_key_invalid, [])
         )
     )
  >>= ensure_rsa_key_length

let pem_of_string x ~error_invalid =
  let raw_pem = Cstruct.of_string x in
  X509.Certificate.decode_pem raw_pem
  |> R.reword_error (fun (`Msg err_msg) ->
         D.info {|Failed to validate certificate because "%s"|} err_msg ;
         `Msg (error_invalid, [])
     )

let assert_not_expired ~now certificate ~error_not_yet ~error_expired =
  let to_string = Ptime.to_rfc3339 ~tz_offset_s:0 in
  let not_before, not_after = X509.Certificate.validity certificate in
  if Ptime.is_earlier ~than:not_before now then
    Error (`Msg (error_not_yet, [to_string now; to_string not_before]))
  else if Ptime.is_later ~than:not_after now then
    Error (`Msg (error_expired, [to_string now; to_string not_after]))
  else
    Ok certificate

let _validate_not_expired ~now (blob : string) ~error_invalid ~error_not_yet
    ~error_expired =
  pem_of_string blob ~error_invalid >>= fun cert ->
  assert_not_expired ~now cert ~error_not_yet ~error_expired

let validate_not_expired x ~error_not_yet ~error_expired ~error_invalid =
  let now = Ptime_clock.now () in
  _validate_not_expired ~now x ~error_not_yet ~error_expired ~error_invalid
  |> Rresult.R.reword_error @@ fun (`Msg (e, msgs)) -> Server_error (e, msgs)

let validate_pem_chain ~pem_leaf ~pem_chain now private_key =
  let ensure_keys_match private_key certificate =
    let public_key = X509.Certificate.public_key certificate in
    match (public_key, private_key) with
    | `RSA pub, `RSA priv when pub = Mirage_crypto_pk.Rsa.pub_of_priv priv ->
        Ok certificate
    | _ ->
        Error (`Msg (server_certificate_key_mismatch, []))
  in
  let ensure_sha256_signature_algorithm certificate =
    match X509.Certificate.signature_algorithm certificate with
    | Some (_, `SHA256) ->
        Ok certificate
    | _ ->
        Error (`Msg (server_certificate_signature_not_supported, []))
  in
  let validate_chain pem_chain =
    let raw_pem = Cstruct.of_string pem_chain in
    X509.Certificate.decode_pem_multiple raw_pem |> function
    | Ok (_ :: _ as certs) ->
        Ok certs
    | Ok [] ->
        D.info "Rejected certificate chain because it's empty." ;
        Error (`Msg (server_certificate_chain_invalid, []))
    | Error (`Msg err_msg) ->
        D.info {|Failed to validate certificate chain because "%s"|} err_msg ;
        Error (`Msg (server_certificate_chain_invalid, []))
  in
  _validate_not_expired ~now pem_leaf ~error_invalid:server_certificate_invalid
    ~error_not_yet:server_certificate_not_valid_yet
    ~error_expired:server_certificate_expired
  >>= ensure_keys_match private_key
  >>= ensure_sha256_signature_algorithm
  >>= fun cert ->
  match Option.map validate_chain pem_chain with
  | None ->
      Ok (cert, None)
  | Some (Ok chain) ->
      Ok (cert, Some chain)
  | Some (Error msg) ->
      Error msg

(** Decodes the PEM-encoded objects (private key, leaf certificate, and
    certificate chain, reencodes them to make sure they are normalised, and
    finally it installs them as a server certificate to be ready to use by
    stunnel. It also ensures the objects maintian some cryptographic
    properties. *)
let install_server_certificate ~pem_chain ~pem_leaf ~pkcs8_private_key
    ~server_cert_path ~cert_gid =
  let now = Ptime_clock.now () in
  validate_private_key pkcs8_private_key >>= fun priv ->
  let pkcs8_private_key =
    X509.Private_key.encode_pem priv |> Cstruct.to_string
  in
  validate_pem_chain ~pem_leaf ~pem_chain now priv >>= fun (cert, chain) ->
  let pem_leaf = X509.Certificate.encode_pem cert |> Cstruct.to_string in
  Option.fold
    ~none:(Ok [pkcs8_private_key; pem_leaf])
    ~some:(fun chain ->
      let pem_chain =
        X509.Certificate.encode_pem_multiple chain |> Cstruct.to_string
      in
      Ok [pkcs8_private_key; pem_leaf; pem_chain]
    )
    chain
  >>= fun server_cert_components ->
  server_cert_components
  |> String.concat "\n\n"
  |> Selfcert.write_certs server_cert_path cert_gid
  |> R.reword_error (function `Msg msg -> `Msg (internal_error, [msg]))
  >>= fun () -> R.ok cert
