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

(** initialize the random number generator at program startup when this
module is loaded. *)
let () = Mirage_crypto_rng_unix.initialize ()

let defer f = Fun.protect ~finally:f

(** [write_cert] writes a PEM file to [path]. It attempts to do that
  atomically by writing to a temporary file in the same directory first and
  renaming the file at the end *)
let write_certs path key cert =
  let delim = "\n" in
  let temp_dir = Filename.dirname path in
  let tmp = Filename.temp_file ~temp_dir "certify-" ".tmp" in
  try
    let fd = Unix.openfile tmp [Unix.O_WRONLY] 0o600 in
    defer (fun () -> Unix.close fd) @@ fun () ->
    UX.really_write fd (Cstruct.to_string key) 0 (Cstruct.len key) ;
    UX.really_write_string fd delim ;
    UX.really_write fd (Cstruct.to_string cert) 0 (Cstruct.len cert) ;
    Unix.rename tmp path ;
    R.ok ()
  with e -> R.error_msg (Printexc.to_string e)

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

let selfsign name alt_names length days certfile =
  let rsa = Rsa.generate ~bits:length () in
  let privkey = `RSA rsa in
  let pubkey = `RSA (Rsa.pub_of_priv rsa) in
  let issuer =
    [X509.Distinguished_name.(Relative_distinguished_name.singleton (CN name))]
  in
  let req = X509.Signing_request.create issuer privkey in
  sign days privkey pubkey issuer req alt_names >>= fun cert ->
  let cert_pem = X509.Certificate.encode_pem cert in
  let key_pem = X509.Private_key.encode_pem privkey in
  write_certs certfile key_pem cert_pem

let host name alt_names pemfile =
  let expire_days = 3650 in
  let length = 2048 in
  selfsign name alt_names length expire_days pemfile |> R.failwith_error_msg

module CLI = struct
  let help =
    [
      `P "These options are common to all commands."
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
    ; `S "BUGS"
    ; `P "Check bug reports at https://github.com/lindig/certify/issues"
    ]

  let pemfile =
    C.Arg.(
      value
      & opt string "certify.pem"
      & info ["o"; "pem"; "out"] ~docv:"FILE.PEM" ~doc:"Target for PEM cert.")

  let hostname =
    C.Arg.(
      value
      & pos 0 string "localhost"
      & info [] ~docv:"NAME" ~doc:"hostname for certificate")

  let alt_names =
    C.Arg.(
      value
      & opt_all string []
      & info ["d"; "dns"] ~docv:"DNS" ~doc:"Alternative hostname")

  let certify =
    let doc = "Create a self-signed certificate for a host" in
    C.Term.
      ( const host $ hostname $ alt_names $ pemfile
      , info "certify" ~doc ~man:help )

  let main () = C.Term.(exit @@ eval certify)
end
