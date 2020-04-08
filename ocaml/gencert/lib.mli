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

val get_management_ip_addr : dbg:string -> string option
val install_server_certificate :
  ?pem_chain:string option ->
  pem_leaf:string ->
  pkcs8_private_key:string ->
  server_cert_path:string ->
  (X509.Certificate.t, [> `Msg of string * string list ]) Result.result
(** [install_server_certificate pem_chain pem_leaf pkcs8_private_key
     server_cert_path] writes a PKCS12 containing [pkcs8_private_key],
     [pem_leaf] and [pem_chain] to the path [server_cert_path].

     The 3 elements of the PKCS12 are validated before writing the file and
     the file is written atomically.

     In the case validation fails or the file could not be written an error is
     returned, if the file was successfully written the leaf certificate is
     returned for further inspection, if needed. *)

(** The following functions are exposed exclusively for unit-testing, please
    do not use them directly, they are not stable *)

type t_certificate = Leaf | Chain

val validate_private_key :
  string ->
  ([> `RSA of Mirage_crypto_pk.Rsa.priv ],
   [> `Msg of string * string list ])
  Result.result

val validate_certificate :
  t_certificate ->
  string ->
  Ptime.t ->
  [> `RSA of Mirage_crypto_pk.Rsa.priv ] ->
  (X509.Certificate.t, [> `Msg of string * string list ])
  Rresult.result
