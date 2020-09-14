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

module C = Cmdliner
module Rsa = Mirage_crypto_pk.Rsa
module UX = Xapi_stdext_unix.Unixext
open Rresult (* introduces >>= >>| and R *)

module D = Debug.Make (struct let name = "gencert_selfcert" end)

(** initialize the random number generator at program startup when this
module is loaded. *)
let () = Mirage_crypto_rng_unix.initialize ()

(** [write_cert] writes a PKCS12 file to [path]. The typical file
 extension would be ".pem". It attempts to do that atomically by
 writing to a temporary file in the same directory first and renaming
 the file at the end *)
let write_certs path pkcs12 =
  let f () =
    UX.atomic_write_to_file path 0o400 @@ fun fd ->
    UX.really_write fd pkcs12 0 (String.length pkcs12)
  in
  R.trap_exn f () |> R.error_exn_trap_to_msg

let expire_in days =
  let seconds = days * 24 * 60 * 60 in
  let start = Ptime_clock.now () in
  match Ptime.(add_span start @@ Span.of_int_s seconds) with
  | Some expire ->
      R.ok (start, expire)
  | None ->
      R.error_msgf "can't represent %d as time span" days

let add_dns_names extension = function
  | [] ->
      extension
  | names ->
      X509.Extension.(
        add Subject_alt_name
          (false, X509.General_name.(singleton DNS names))
          extension)

let sign days key pubkey issuer req alt_names =
  expire_in days >>= fun (valid_from, valid_until) ->
  match (key, pubkey) with
  | `RSA priv, `RSA pub when Rsa.pub_of_priv priv = pub ->
      let extensions = add_dns_names X509.Extension.empty alt_names in
      X509.Signing_request.sign ~valid_from ~valid_until ~extensions req key
        issuer
      |> R.reword_error (fun _ -> Printf.sprintf "signing failed" |> R.msg)
  | _ ->
      R.error_msgf "public/private keys don't match (%s)" __LOC__

(** call openssl and return stdout, stderr as strings *)
let call_openssl args =
  let openssl = !Constants.openssl_path in
  let home =
    match Sys.getenv_opt "HOME" with
    | None ->
        D.warn
          "environment variable 'HOME' is unavailable, falling back to \
           HOME=/root" ;
        "/root"
    | Some path ->
        path
  in
  let env =
    [|"PATH=" ^ String.concat ":" Forkhelpers.default_path; "HOME=" ^ home|]
  in
  Forkhelpers.execute_command_get_output openssl ~env args

(** [generate_private_key] calls openssl to generate an RSA key of
  [length] bits. *)
let generate_private_key length =
  let args = ["genrsa"; string_of_int length] in
  let stdout, _stderr = call_openssl args in
  stdout

let selfsign name alt_names length days certfile =
  let rsa =
    try
      generate_private_key length
      |> Cstruct.of_string
      |> X509.Private_key.decode_pem
      |> R.failwith_error_msg
      |> function
      | `RSA x ->
          x
    with e ->
      let msg =
        Printf.sprintf "generating RSA key for %s failed: %s" certfile
          (Printexc.to_string e)
      in
      D.error "%s" msg ; failwith msg
  in
  let privkey = `RSA rsa in
  let pubkey = `RSA (Rsa.pub_of_priv rsa) in
  let issuer =
    [X509.Distinguished_name.(Relative_distinguished_name.singleton (CN name))]
  in
  let req = X509.Signing_request.create issuer privkey in
  sign days privkey pubkey issuer req alt_names >>= fun cert ->
  let key_pem = X509.Private_key.encode_pem privkey in
  let cert_pem = X509.Certificate.encode_pem cert in
  let pkcs12 =
    String.concat "\n\n" [Cstruct.to_string key_pem; Cstruct.to_string cert_pem]
  in
  write_certs certfile pkcs12

let host name alt_names pemfile =
  let expire_days = 3650 in
  let length = 2048 in
  (* make sure name is part of alt_names because CN is deprecated and
     that there are no duplicates *)
  let alt = Astring.String.uniquify (name :: alt_names) in
  selfsign name alt length expire_days pemfile |> R.failwith_error_msg
