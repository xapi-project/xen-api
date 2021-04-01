module D = Debug.Make (struct let name = "certificates_sync" end)

open D
module Unixext = Xapi_stdext_unix.Unixext
module Date = Xapi_stdext_date.Date
module G = Gencertlib
module GP = Gencertlib.Pem
open Api_errors
open Rresult

let ( let* ) = ( >>= )

let uninstall ~__context cert =
  info "Removing certificate %s" (Ref.string_of cert) ;
  Db.Certificate.destroy ~__context ~self:cert

(* the host cert is under a fixed name - so the new cert in the file
   system already replaced the old one. We must not remove it from the
   file system *)

(** add a certificate to the database. The certificate is already in the
  file system. This creates a new entry in the database. *)
let install ~__context ~host cert =
  try
    let ref =
      Certificates.Db_util.add_cert ~__context ~type':(`host host) cert
    in
    info "Adding host certificicate %s to database" (Ref.string_of ref) ;
    R.ok ()
  with e ->
    error "certificates_sync.install exception: %s" (Printexc.to_string e) ;
    Error (`Msg ("installation of host certificate failed", []))

(** determine if the database is up to date by comparing the fingerprint
  of xapi-ssl.pem with the entry in the database *)
let is_unchanged ~__context cert_ref cert =
  let ref_hash = Db.Certificate.get_fingerprint ~__context ~self:cert_ref in
  let cert_hash =
    X509.Certificate.fingerprint Mirage_crypto.Hash.(`SHA256) cert
    |> Certificates.pp_hash
  in
  cert_hash = ref_hash

(** [get_server_cert] loads xapi-ssl.pem from the file system and
  returns it decoded *)
let get_server_cert () =
  match G.Pem.parse_file !Xapi_globs.server_cert_path with
  | Error msg ->
      Error (`Msg (msg, []))
  | Ok cert ->
      let host_pem = cert.GP.host_cert in
      let* host_cert =
        Cstruct.of_string host_pem
        |> X509.Certificate.decode_pem
        |> R.reword_error (fun (`Msg msg) ->
               D.info {|Failed to decode certificate because "%s"|} msg ;
               `Msg (server_certificate_invalid, []))
      in
      Ok host_cert

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
      let unchanged = is_unchanged ~__context cert_ref cert in
      if unchanged then (
        info "Active server certificate for host %s is unchanged" host_uuid ;
        Ok ()
      ) else (
        info "Server certificate for host %s changed - updating" host_uuid ;
        let* () = install ~__context ~host cert in
        uninstall ~__context cert_ref ;
        Ok ()
      )
  | cert_refs ->
      warn "The host has more than one certificate: %s"
        (String.concat ", " (List.map Ref.string_of cert_refs)) ;
      info "Server certificate for host %s changed - updating" host_uuid ;
      let* () = install ~__context ~host cert in
      List.iter (uninstall ~__context) cert_refs ;
      Ok ()
