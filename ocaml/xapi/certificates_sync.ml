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
let install ~__context ~host:_ ~type' cert =
  try
    let ref = Certificates.Db_util.add_cert ~__context ~type' cert in
    info "Adding host certificicate %s to database" (Ref.string_of ref) ;
    R.ok ()
  with e ->
    error "certificates_sync.install exception: %s" (Printexc.to_string e) ;
    Error (`Msg ("installation of host certificate failed", []))

type to_update = Certificate | Hashes of {sha256: string; sha1: string}

(** determine if the database is up to date by comparing the fingerprint
  of xapi-ssl.pem with the entry in the database *)
let to_update ~__context cert_ref cert =
  let ref_hash =
    Db.Certificate.get_fingerprint_sha256 ~__context ~self:cert_ref
  in
  let sha256 = Certificates.pp_fingerprint ~hash_type:`SHA256 cert in
  if ref_hash = "" then
    (* We must be upgrading from a version predating fingerprint_sha256, so check fingerprint instead *)
    if sha256 = Db.Certificate.get_fingerprint ~__context ~self:cert_ref then
      let sha1 = Certificates.pp_fingerprint ~hash_type:`SHA1 cert in
      Some (Hashes {sha256; sha1})
    else
      Some Certificate
  else if sha256 = ref_hash then
    None
  else
    Some Certificate

(** [get_server_cert] loads [path] from the file system and
  returns it decoded *)
let get_server_cert path =
  match G.Pem.parse_file path with
  | Error msg ->
      Error (`Msg (msg, []))
  | Ok cert ->
      let host_pem = cert.GP.host_cert in
      let* host_cert =
        Cstruct.of_string host_pem
        |> X509.Certificate.decode_pem
        |> R.reword_error (fun (`Msg msg) ->
               D.info {|Failed to decode certificate because "%s"|} msg ;
               `Msg (server_certificate_invalid, [])
           )
      in
      Ok host_cert

let sync ~__context ~type' =
  let host = Helpers.get_localhost ~__context in
  let host_uuid = Helpers.get_localhost_uuid () in
  let refs = Certificates.Db_util.get_host_certs ~__context ~type' ~host in
  let type', path =
    (* this shadows type'. We have two tags here: with and without an
       argument and we need the one with host as an argument below *)
    match type' with
    | `host ->
        (`host host, !Xapi_globs.server_cert_path)
    | `host_internal ->
        (`host_internal host, !Xapi_globs.server_cert_internal_path)
  in
  let* cert = get_server_cert path in
  match refs with
  | [] ->
      info "Host %s has no active server certificate" host_uuid ;
      install ~__context ~host ~type' cert
  | [cert_ref] -> (
    match to_update ~__context cert_ref cert with
    | Some Certificate ->
        info "Server certificate for host %s changed - updating" host_uuid ;
        let* () = install ~__context ~host ~type' cert in
        uninstall ~__context cert_ref ;
        Ok ()
    | Some (Hashes {sha256; sha1}) ->
        info "Active server certificate for host %s is unchanged" host_uuid ;
        Db.Certificate.set_fingerprint_sha256 ~__context ~self:cert_ref
          ~value:sha256 ;
        Db.Certificate.set_fingerprint_sha1 ~__context ~self:cert_ref
          ~value:sha1 ;
        info "Populated new fingerprint fields: sha256= %s; sha1= %s" sha256
          sha1 ;
        Ok ()
    | None ->
        info "Active server certificate for host %s is unchanged" host_uuid ;
        Ok ()
  )
  | cert_refs ->
      warn "The host has more than one certificate: %s"
        (String.concat ", " (List.map Ref.string_of cert_refs)) ;
      info "Server certificate for host %s changed - updating" host_uuid ;
      let* () = install ~__context ~host ~type' cert in
      List.iter (uninstall ~__context) cert_refs ;
      Ok ()

let update ~__context =
  let* () = sync ~__context ~type':`host in
  let* () = sync ~__context ~type':`host_internal in
  Ok ()

let internal_error fmt =
  fmt
  |> Printf.ksprintf @@ fun msg ->
     error "%s" msg ;
     raise Api_errors.(Server_error (internal_error, [msg]))

let remove_from_db ~__context cert =
  try
    Db.Certificate.destroy ~__context ~self:cert ;
    info "removed host certificate %s from db" (Ref.string_of cert)
  with e ->
    internal_error "failed to remove cert %s: %s" (Ref.string_of cert)
      (Printexc.to_string e)

let path host_uuid =
  let prefix = !Xapi_globs.trusted_pool_certs_dir in
  Filename.concat prefix (Printf.sprintf "%s.pem" host_uuid)

let host_certs_of ~__context host =
  List.concat
    [
      Certificates.Db_util.get_host_certs ~__context ~host ~type':`host
    ; Certificates.Db_util.get_host_certs ~__context ~host ~type':`host_internal
    ]

let eject_certs_from_db ~__context certs =
  certs |> List.iter (remove_from_db ~__context)

let eject_certs_from_fs_for ~__context host =
  (* the cert is not identified by its UUID in the file system but
     by the UUID of the host it belongs to *)
  let host_uuid = Db.Host.get_uuid ~__context ~self:host in
  let file = path host_uuid in
  try
    match Sys.file_exists file with
    | true ->
        Sys.remove file ;
        Certificates.update_ca_bundle () ;
        info "removed host certificate %s" file
    | false ->
        info "host %s has no certificate %s to remove" host_uuid file
  with e ->
    internal_error "failed to remove cert %s on pool eject: %s" file
      (Printexc.to_string e)
