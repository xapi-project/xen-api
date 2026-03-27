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

(** TLS policy types and format-specific string renderers.

    Usage:
    {[
      let () = print_endline (Tls_policy.Gnutls.default_policy ())
      let () = print_endline (Tls_policy.Openssl.default_policy ())
      let () = print_endline Tls_policy.Openssl.default_curve
      (* or with a custom policy: *)
      let my_policy = { ... }
      let () = print_endline (Tls_policy.Openssl.string_of_policy my_policy)
    ]} *)

type version = TLS_1_2 | TLS_1_3

type cipher =
  | AES_128_GCM  (** AEAD; paired with SHA-256 in OpenSSL suite names *)
  | AES_256_GCM  (** AEAD; paired with SHA-384 in OpenSSL suite names *)

type curve = Secp384r1

type kex = ECDHE_RSA

type policy = {
    versions: version list
  ; ciphers: cipher list
  ; curves: curve list  (** Only the first curve is used for stunnel. *)
  ; kex: kex list
  ; server_preference: bool  (** When [true], the server picks the cipher. *)
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
  (** Format-specific server-preference flag, or ["" ] if disabled. *)
end

(** Extends [Renderer] with convenience values pre-applied to the default policy. *)
module type RendererWithDefaults = sig
  include Renderer

  val default_policy : unit -> string
  (** Format-specific combined cipher suite string for the default policy.

      This is a [unit -> string] function rather than a plain [string] value to
      avoid eager evaluation at module initialisation time.  OCaml evaluates
      every top-level [let] in a functor body when the functor is applied, so a
      plain [string] binding would call [string_of_policy] immediately —
      crashing for renderers (e.g. [Openssl]) where [string_of_policy] raises.
      The other [default_*] values are plain strings because their underlying
      functions are defined for every renderer. *)

  val default_ciphers : string
  (** Colon-joined cipher string for the default cipher list. *)

  val default_version : string
  (** Colon-joined version string for the default version list. *)

  val default_curve : string
  (** Colon-joined curve string for the default curve list. *)

  val default_server_preference : string
  (** Server-preference string for the default policy. *)
end

(** GnuTLS priority string renderer. *)
module Gnutls : RendererWithDefaults

(** OpenSSL cipher list renderer. *)
module Openssl : RendererWithDefaults
