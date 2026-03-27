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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type version = TLS_1_2 | TLS_1_3

type cipher = AES_128_GCM | AES_256_GCM

type curve = Secp384r1

type kex = ECDHE_RSA

type policy = {
    versions: version list
  ; ciphers: cipher list
  ; curves: curve list
  ; kex: kex list
  ; server_preference: bool
}

(* ---- Default policy ------------------------------------------- *)

let default =
  {
    versions= [TLS_1_2]
  ; ciphers= [AES_256_GCM; AES_128_GCM]
  ; curves= [Secp384r1]
  ; kex= [ECDHE_RSA]
  ; server_preference= true
  }

(** Common interface both renderers implement. *)
module type Renderer = sig
  val string_of_versions : version list -> string
  (** Format-specific colon-joined version list from a version list. *)

  val string_of_ciphers : cipher list -> string
  (** Format-specific colon-joined cipher list from a cipher list. *)

  val string_of_policy : policy -> string
  (** Format-specific combined cipher suite string from a policy. *)

  val string_of_curves : curve list -> string
  (** Format-specific colon-joined curve list from a curve list. *)

  val string_of_server_preference : bool -> string
  (** Format-specific server-preference flag *)
end

(* ---- GnuTLS renderer ---------------------------------------------------- *)

module GnutlsImpl = struct
  let string_of_version = function
    | TLS_1_2 ->
        "+VERS-TLS1.2"
    | TLS_1_3 ->
        "+VERS-TLS1.3"

  let string_of_cipher = function
    | AES_128_GCM ->
        "+AES-128-GCM"
    | AES_256_GCM ->
        "+AES-256-GCM"

  let string_of_versions versions =
    List.map string_of_version versions |> String.concat ":"

  let string_of_ciphers ciphers =
    List.map string_of_cipher ciphers |> String.concat ":"

  let string_of_curve = function Secp384r1 -> "+GROUP-SECP384R1"

  let string_of_curves curves =
    List.map string_of_curve curves |> String.concat ":"

  let string_of_kex = function ECDHE_RSA -> "+ECDHE-RSA"

  let string_of_server_preference = function
    | true ->
        "%SERVER_PRECEDENCE"
    | false ->
        ""

  (** Build a GnuTLS priority string from a policy.
      Example:
      ["NONE:+VERS-TLS1.2:+AES-256-GCM:+AES-128-GCM:+AEAD:+ECDHE-RSA:+SIGN-ALL:+GROUP-SECP384R1:+COMP-NULL:%SERVER_PRECEDENCE"]
      Suitable for Samba's [tls priority =] in [smb.conf]. *)
  let string_of_policy {versions; ciphers; kex; curves; server_preference} =
    (* GnuTLS priority token order (from the GnuTLS manual):
       versions -> ciphers -> MACs -> KEX -> signatures -> groups -> compression -> flags *)
    let is_aead =
      List.exists (function AES_128_GCM | AES_256_GCM -> true) ciphers
    in
    let tokens =
      List.map string_of_version versions
      @ List.map string_of_cipher ciphers
      @ ( if is_aead then
            ["+AEAD"]
          else
            []
        )
      @ List.map string_of_kex kex
      @ ["+SIGN-ALL"]
      @ List.map string_of_curve curves
      @ ["+COMP-NULL"]
      @
      if server_preference then
        ["%SERVER_PRECEDENCE"]
      else
        []
    in
    Printf.sprintf "NONE:%s" (String.concat ":" tokens)
end

(* ---- OpenSSL renderer --------------------------------------------------- *)

module OpensslImpl = struct
  (* OpenSSL TLS 1.2 suite name: ECDHE-RSA-CIPHER-HASH.
     For GCM (AEAD) suites the hash is used only as the PRF:
       AES-256-GCM -> SHA-384, AES-128-GCM -> SHA-256. *)
  let string_of_cipher = function
    | AES_256_GCM ->
        "ECDHE-RSA-AES256-GCM-SHA384"
    | AES_128_GCM ->
        "ECDHE-RSA-AES128-GCM-SHA256"

  let string_of_version = function TLS_1_2 -> "TLSv1.2" | TLS_1_3 -> "TLSv1.3"

  let string_of_versions versions =
    List.map string_of_version versions |> String.concat ":"

  let string_of_ciphers ciphers =
    List.map string_of_cipher ciphers |> String.concat ":"

  let string_of_curve = function Secp384r1 -> "secp384r1"

  let string_of_curves curves =
    List.map string_of_curve curves |> String.concat ":"

  let string_of_server_preference = function
    | true ->
        "CIPHER_SERVER_PREFERENCE"
    | false ->
        ""

  let string_of_policy _ = failwith "Not supported"
end

(** Extends any [Renderer] with convenience values pre-applied to [default]. *)
module type RendererWithDefaults = sig
  include Renderer

  val default_policy : unit -> string

  val default_ciphers : string

  val default_version : string

  val default_curve : string

  val default_server_preference : string
end

module WithDefaults (R : Renderer) : RendererWithDefaults = struct
  include R

  let default_policy () = R.string_of_policy default

  let default_ciphers = R.string_of_ciphers default.ciphers

  let default_version = R.string_of_versions default.versions

  let default_curve = R.string_of_curves default.curves

  let default_server_preference =
    R.string_of_server_preference default.server_preference
end

module Gnutls = WithDefaults (GnutlsImpl)
module Openssl = WithDefaults (OpensslImpl)
