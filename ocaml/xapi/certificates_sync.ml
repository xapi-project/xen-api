module D = Debug.Make (struct let name = "certificates_sync" end)

open D
module Unixext = Xapi_stdext_unix.Unixext
module Date = Xapi_stdext_date.Date
module G = Gencertlib
module GP = Gencertlib.Pem
open Api_errors
open Rresult

let ( let* ) = ( >>= )

let is_self_signed cert =
  R.return
    X509.(
      Distinguished_name.equal (Certificate.subject cert)
        (Certificate.issuer cert))

let pool_size ~__context = Db.Pool.get_all ~__context |> List.length

let remove_cert ~__context cert =
  info "Removing certificate %s" (Ref.string_of cert) ;
  Db.Certificate.destroy ~__context ~self:cert

(* the host cert is under a fixed name - so the new cert in the file
   system already replaced the old one. We must not remove it from the
   file system *)

(** verify and install a certificate or return with an error *)
let install ~__context ~host cert =
  try
    let host_pem = cert.GP.host_cert in
    let* host_cert =
      Cstruct.of_string host_pem
      |> X509.Certificate.decode_pem
      |> R.reword_error (fun (`Msg msg) ->
             D.info {|Failed to decode certificate because "%s"|} msg ;
             `Msg (server_certificate_invalid, []))
    in
    let* self_signed = is_self_signed host_cert in
    match self_signed with
    | true ->
        let raw = X509.Certificate.encode_pem host_cert in
        let _pem = Cstruct.to_string raw in
        (* pass own cert as root *)
        warn "not implemented: host cert database entry needs to be updated" ;
        R.ok ()
    | false ->
        let _ca_cert_bundle_path = !Xapi_globs.stunnel_cert_path in
        let _pem_leaf = cert.GP.host_cert in
        let _pkcs8_private_key = cert.GP.private_key in
        let _pem_intermediates = String.concat "\n\n" cert.GP.other_certs in
        let _time = Ptime_clock.now () in
        warn "not implemented: host cert database entry needs to be updated" ;
        R.ok ()
  with e ->
    error "certificates_sync.install exception: %s" (Printexc.to_string e) ;
    Error (`Msg ("installation of host certificate failed", []))

(** determine if the database is up to date by comparing the fingerprint
  of xapi-ssl.pem with the entry in the database *)
let is_unchanged ~__context cert_ref cert =
  let host_pem = cert.GP.host_cert in
  let ref_hash = Db.Certificate.get_fingerprint ~__context ~self:cert_ref in
  let* host_cert =
    Cstruct.of_string host_pem
    |> X509.Certificate.decode_pem
    |> R.reword_error (fun (`Msg msg) ->
           D.info {|Failed to decode certificate because "%s"|} msg ;
           `Msg (server_certificate_invalid, []))
  in
  let cert_hash =
    X509.Certificate.fingerprint Mirage_crypto.Hash.(`SHA256) host_cert
    |> Certificates.pp_hash
  in
  Ok (cert_hash = ref_hash)

(** [get_server_cert] loads xapi-ssl.pem from the file system and
  returns it as a record containing the PEM strings *)
let get_server_cert () =
  match G.Pem.parse_file !Xapi_globs.server_cert_path with
  | Error msg ->
      Error (`Msg (msg, []))
  | Ok _ as ok ->
      ok

let update ~__context =
  let host = Helpers.get_localhost ~__context in
  let host_uuid = Helpers.get_localhost_uuid () in
  let cert_refs = Db.Host.get_certificates ~__context ~self:host in
  let* cert = get_server_cert () in
  match cert_refs with
  | [] ->
      info "Host %s has no active server certificate" host_uuid ;
      install ~__context ~host cert
  | [cert_ref] ->
      let* unchanged = is_unchanged ~__context cert_ref cert in
      if unchanged then (
        info "Active server certificate for host %s is unchanged" host_uuid ;
        Ok ()
      ) else (
        info "Server certificate for host %s changed - updating" host_uuid ;
        let* () = install ~__context ~host cert in
        remove_cert ~__context cert_ref ;
        Ok ()
      )
  | cert_refs ->
      warn "The host has more than one certificate: %s"
        (String.concat ", " (List.map Ref.string_of cert_refs)) ;
      info "Server certificate for host %s changed - updating" host_uuid ;
      let* () = install ~__context ~host cert in
      List.iter (remove_cert ~__context) cert_refs ;
      Ok ()
