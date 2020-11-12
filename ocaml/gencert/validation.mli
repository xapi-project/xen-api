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

val validating_trust_anchors :
     pem_leaf:string
  -> pkcs8_private_key:string
  -> pem_intermediates:string
  -> time:Ptime.t
  -> ca_cert_bundle_path:string
  -> ( X509.Certificate.t * X509.Certificate.t list
     , [> `Msg of string * string list] )
     result
(** [validating_trust_anchors ~pem_leaf ~pkcs8_private_key ~pem_intermediates
   ~time ~ca_cert_bundle_path] returns the server certificate decoded from
   [pem_leaf] along the list of root certificates that verify is successfully,
   or an error if the leaf certificate, private key or intermediates failed
   verification.

   The function decodes and verifies the pem-encoded [pem_leaf] as a server
   certificate using [time], [pkcs8_private_key] as a private key and
   [pem_intermediates] as a chain of intermediate certificates, loads a
   pem-encoded list of trust anchors from [ca_cert_bundle_path], generates all
   possible chains and finally validates them to return only to return the
   root of the valid ones. *)

(* The following functions are only exposed to aid testing, please do not use
   them directly *)

val validate_private_key :
     string
  -> ( [`RSA of Mirage_crypto_pk.Rsa.priv]
     , [> `Msg of string * string list] )
     result

val validate_server_certificate :
     string
  -> Ptime.t
  -> [`RSA of Mirage_crypto_pk.Rsa.priv]
  -> (X509.Certificate.t, [> `Msg of string * string list]) result

val decode_certificate_chain :
  string -> (X509.Certificate.t list, [> `Msg of string * string list]) result
