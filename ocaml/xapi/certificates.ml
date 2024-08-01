(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
module Unixext = Xapi_stdext_unix.Unixext
open Api_errors
open Client

module D = Debug.Make (struct let name = "certificates" end)

open D

let () = Mirage_crypto_rng_unix.initialize ()

(* Certificate locations:
   * a) stunnel external             = /etc/xensource/xapi-ssl.pem
   * b) stunnel SNI (internal)       = /etc/xensource/xapi-pool-tls.pem
   * c) user trusted cert folder     = /etc/stunnel/certs/
   * d) internal trusted cert folder = /etc/stunnel/certs-pool/
   * e) appliance trusted bundle     = /etc/stunnel/xapi-stunnel-ca-bundle.pem
   * f) host-in-pool trusted bundle  = /etc/stunnel/xapi-pool-ca-bundle.pem
   *
   * Note that the bundles (e) and (f) are generated automatically using the contents of (c) and (d) respectively *)

type t_trusted = CA_Certificate | CRL

let pem_of_string x =
  match Cstruct.of_string x |> X509.Certificate.decode_pem with
  | Error _ ->
      D.error "pem_of_string: failed to parse certificate string" ;
      raise
        Api_errors.(Server_error (invalid_value, ["certificate"; "<omitted>"]))
  | Ok x ->
      x

let library_path = function
  | CA_Certificate ->
      !Xapi_globs.trusted_certs_dir
  | CRL ->
      Stunnel.crl_path

let library_filename kind name = Filename.concat (library_path kind) name

let mkdir_cert_path kind = Unixext.mkdir_rec (library_path kind) 0o700

let rehash' path =
  ignore (Forkhelpers.execute_command_get_output !Xapi_globs.c_rehash [path])

let rehash () =
  mkdir_cert_path CA_Certificate ;
  mkdir_cert_path CRL ;
  rehash' (library_path CA_Certificate) ;
  rehash' (library_path CRL)

let update_ca_bundle () = Helpers.update_ca_bundle ()

let to_string = function CA_Certificate -> "CA certificate" | CRL -> "CRL"

(** {pp_hash hash} outputs the hexadecimal representation of the {hash}
    adding a colon between every octet, in uppercase.
 *)
let pp_hash hash =
  let hex = Hex.(show @@ of_cstruct hash) in
  let length = (3 * String.length hex / 2) - 1 in
  let value_of i =
    match (i + 1) mod 3 with
    | 0 ->
        ':'
    | _ ->
        Char.uppercase_ascii hex.[i - ((i + 1) / 3)]
  in
  String.init length value_of

let pp_fingerprint ~hash_type cert =
  X509.Certificate.fingerprint hash_type cert |> pp_hash

let safe_char c =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' ->
      true
  | _ ->
      false

let not_safe_chars name =
  let n = String.length name in
  let rec f i =
    if i = n then
      false
    else if not (safe_char name.[i]) then
      true
    else
      f (i + 1)
  in
  f 0

let is_unsafe name =
  name.[0] = '.'
  || (not (Astring.String.is_suffix ~affix:".pem" name))
  || not_safe_chars name

let raise_server_error parameters err = raise (Server_error (err, parameters))

let raise_name_invalid kind n =
  let err =
    match kind with
    | CA_Certificate ->
        certificate_name_invalid
    | CRL ->
        crl_name_invalid
  in
  raise_server_error [n] err

let validate_name kind name =
  if is_unsafe name then
    raise_name_invalid kind name

let raise_already_exists kind n =
  let err =
    match kind with
    | CA_Certificate ->
        certificate_already_exists
    | CRL ->
        crl_already_exists
  in
  raise_server_error [n] err

let raise_does_not_exist kind n =
  let err =
    match kind with
    | CA_Certificate ->
        certificate_does_not_exist
    | CRL ->
        crl_does_not_exist
  in
  raise_server_error [n] err

let raise_corrupt kind n =
  let err =
    match kind with CA_Certificate -> certificate_corrupt | CRL -> crl_corrupt
  in
  raise_server_error [n] err

let raise_library_corrupt () =
  raise (Server_error (certificate_library_corrupt, []))

module Db_util : sig
  type name = string

  val add_cert :
       __context:Context.t
    -> type':
         [< `host of API.ref_host | `host_internal of API.ref_host | `ca of name]
    -> X509.Certificate.t
    -> API.ref_Certificate

  val remove_cert_by_ref : __context:Context.t -> API.ref_Certificate -> unit

  val remove_ca_cert_by_name : __context:Context.t -> name -> unit

  val get_host_certs :
       __context:Context.t
    -> type':[< `host | `host_internal]
    -> host:API.ref_host
    -> API.ref_Certificate list
  (** [get_host_certs ~__context ~type' ~host] gets all the host certs in the database
    * of type [type'] belonging to [host] (the term 'host' is overloaded here) *)

  val get_ca_certs : __context:Context.t -> API.ref_Certificate list
end = struct
  module Date = Xapi_stdext_date.Date

  type name = string

  let get_ca_certs ~__context name =
    let expr =
      let open Xapi_database.Db_filter_types in
      let type' = Eq (Field "type", Literal "ca") in
      let name' = Eq (Field "name", Literal name) in
      And (type', name')
    in
    Db.Certificate.get_refs_where ~__context ~expr

  let get_host_certs ~__context ~type' ~host =
    let open Xapi_database.Db_filter_types in
    let type' =
      Eq (Field "type", Literal (Record_util.certificate_type_to_string type'))
    in
    let host' = Eq (Field "host", Literal (Ref.string_of host)) in
    let expr = And (type', host') in
    Db.Certificate.get_refs_where ~__context ~expr

  let remove_cert_by_ref ~__context self =
    debug "deleting cert ref=%s from the database" (Ref.string_of self) ;
    Db.Certificate.destroy ~__context ~self

  let add_cert ~__context ~type' certificate =
    let name, host, _type, post_action =
      match type' with
      | `host host ->
          ("", host, `host, Fun.id)
      | `host_internal host ->
          ("", host, `host_internal, Fun.id)
      | `ca name ->
          let certs = get_ca_certs ~__context name in
          let remove_obsoleted_copies () =
            List.iter (remove_cert_by_ref ~__context) certs
          in
          (name, Ref.null, `ca, remove_obsoleted_copies)
    in
    let date_of_ptime time = Date.of_float (Ptime.to_float_s time) in
    let dates_of_ptimes (a, b) = (date_of_ptime a, date_of_ptime b) in
    let not_before, not_after =
      dates_of_ptimes (X509.Certificate.validity certificate)
    in
    let fingerprint_sha256 = pp_fingerprint ~hash_type:`SHA256 certificate in
    let fingerprint_sha1 = pp_fingerprint ~hash_type:`SHA1 certificate in
    let uuid = Uuidx.(to_string (make ())) in
    let ref' = Ref.make () in
    Db.Certificate.create ~__context ~ref:ref' ~uuid ~host ~not_before
      ~not_after ~fingerprint:fingerprint_sha256 ~fingerprint_sha256
      ~fingerprint_sha1 ~name ~_type ;
    debug "added cert %s under uuid=%s ref=%s" name uuid (Ref.string_of ref') ;
    post_action () ;
    ref'

  let remove_ca_cert_by_name ~__context name =
    let certs =
      match get_ca_certs ~__context name with
      | [x] ->
          [x]
      | [] ->
          D.error "unable to find certificate with name='%s'" name ;
          raise
            Api_errors.(Server_error (invalid_value, ["certificate:name"; name]))
      | xs ->
          let ref_str =
            xs |> List.map Ref.short_string_of |> String.concat ", "
          in
          D.warn
            "expected 1 certificate with name='%s', but found multiple: [ %s ]"
            name ref_str ;
          xs
    in
    List.iter (remove_cert_by_ref ~__context) certs

  let get_ca_certs ~__context =
    let expr =
      let open Xapi_database.Db_filter_types in
      Eq (Field "type", Literal "ca")
    in
    Db.Certificate.get_refs_where ~__context ~expr
end

let local_list kind =
  mkdir_cert_path kind ;
  List.filter
    (fun n ->
      let stat = Unix.lstat (library_filename kind n) in
      stat.Unix.st_kind = Unix.S_REG
    )
    (Array.to_list (Sys.readdir (library_path kind)))

let local_sync () =
  try rehash ()
  with e ->
    warn "Exception rehashing certificates: %s" (ExnHelper.string_of_exn e) ;
    raise_library_corrupt ()

let cert_perms kind =
  let stat = Unix.stat (library_path kind) in
  let mask = 0o666 in
  let perm = stat.Unix.st_perm land mask in
  debug "%d %d" perm stat.Unix.st_perm ;
  perm

let host_install kind ~name ~cert =
  validate_name kind name ;
  let filename = library_filename kind name in
  if Sys.file_exists filename then
    raise_already_exists kind name ;
  debug "Installing %s %s" (to_string kind) name ;
  try
    mkdir_cert_path kind ;
    Unixext.write_string_to_file filename cert ;
    Unix.chmod filename (cert_perms kind) ;
    update_ca_bundle ()
  with e ->
    warn "Exception installing %s %s: %s" (to_string kind) name
      (ExnHelper.string_of_exn e) ;
    raise_library_corrupt ()

let host_uninstall kind ~name =
  validate_name kind name ;
  let filename = library_filename kind name in
  if not (Sys.file_exists filename) then
    raise_does_not_exist kind name ;
  debug "Uninstalling %s %s" (to_string kind) name ;
  try Sys.remove filename ; update_ca_bundle ()
  with e ->
    warn "Exception uninstalling %s %s: %s" (to_string kind) name
      (ExnHelper.string_of_exn e) ;
    raise_corrupt kind name

let get_cert kind name =
  validate_name kind name ;
  let filename = library_filename kind name in
  try Unixext.string_of_file filename
  with e ->
    warn "Exception reading %s %s: %s" (to_string kind) name
      (ExnHelper.string_of_exn e) ;
    raise_corrupt kind name

let sync_all_hosts ~__context hosts =
  let exn = ref None in
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      List.iter
        (fun host ->
          try Client.Host.certificate_sync ~rpc ~session_id ~host
          with e -> exn := Some e
        )
        hosts
  ) ;
  match !exn with Some e -> raise e | None -> ()

let sync_certs_crls kind list_func install_func uninstall_func ~__context
    master_certs host =
  Helpers.call_api_functions ~__context (fun rpc session_id ->
      let host_certs = list_func rpc session_id host in
      List.iter
        (fun c ->
          if not (List.mem c master_certs) then
            uninstall_func rpc session_id host c
        )
        host_certs ;
      List.iter
        (fun c ->
          if not (List.mem c host_certs) then
            install_func rpc session_id host c (get_cert kind c)
        )
        master_certs
  )

let sync_certs kind ~__context master_certs host =
  match kind with
  | CA_Certificate ->
      sync_certs_crls CA_Certificate
        (fun rpc session_id host ->
          Client.Host.certificate_list ~rpc ~session_id ~host
        )
        (fun rpc session_id host name cert ->
          Client.Host.install_ca_certificate ~rpc ~session_id ~host ~name ~cert
        )
        (fun rpc session_id host name ->
          Client.Host.uninstall_ca_certificate ~rpc ~session_id ~host ~name
        )
        ~__context master_certs host
  | CRL ->
      sync_certs_crls CRL
        (fun rpc session_id host -> Client.Host.crl_list ~rpc ~session_id ~host)
        (fun rpc session_id host name crl ->
          Client.Host.crl_install ~rpc ~session_id ~host ~name ~crl
        )
        (fun rpc session_id host name ->
          Client.Host.crl_uninstall ~rpc ~session_id ~host ~name
        )
        ~__context master_certs host

let sync_certs_all_hosts kind ~__context master_certs hosts_but_master =
  let exn = ref None in
  List.iter
    (fun host ->
      try sync_certs kind ~__context master_certs host with e -> exn := Some e
    )
    hosts_but_master ;
  match !exn with Some e -> raise e | None -> ()

let pool_sync ~__context =
  let hosts = Db.Host.get_all ~__context in
  let master = Helpers.get_localhost ~__context in
  let hosts_but_master = List.filter (fun h -> h <> master) hosts in
  sync_all_hosts ~__context hosts ;
  let master_certs = local_list CA_Certificate in
  let master_crls = local_list CRL in
  sync_certs_all_hosts CA_Certificate ~__context master_certs hosts_but_master ;
  sync_certs_all_hosts CRL ~__context master_crls hosts_but_master

let pool_install kind ~__context ~name ~cert =
  host_install kind ~name ~cert ;
  try pool_sync ~__context
  with exn ->
    ( try host_uninstall kind ~name
      with e ->
        warn "Exception unwinding install of %s %s: %s" (to_string kind) name
          (ExnHelper.string_of_exn e)
    ) ;
    raise exn

let pool_uninstall kind ~__context ~name =
  host_uninstall kind ~name ; pool_sync ~__context

(* Extracts the server certificate from the server certificate pem file.
   It strips the private key as well as the rest of the certificate chain. *)
let read_public_certficate_from_pkcs12 path =
  match Gencertlib.Pem.parse_file path with
  | Ok Gencertlib.Pem.{host_cert; _} ->
      host_cert
  | Error e ->
      warn "Error parsing %s: %s" path e ;
      raise_library_corrupt ()
  | exception e ->
      warn "Exception reading PKCS #12: %s" (ExnHelper.string_of_exn e) ;
      raise_library_corrupt ()

let get_server_certificate () =
  read_public_certficate_from_pkcs12 !Xapi_globs.server_cert_path

let get_internal_server_certificate () =
  read_public_certficate_from_pkcs12 !Xapi_globs.server_cert_internal_path

open Rresult

let hostnames_of_pem_cert pem =
  Cstruct.of_string pem
  |> X509.Certificate.decode_pem
  >>| X509.Certificate.hostnames

let install_server_certificate ~pem_chain ~pem_leaf ~pkcs8_private_key ~path =
  let installation =
    Gencertlib.Lib.install_server_certificate ~pem_chain ~pem_leaf
      ~pkcs8_private_key ~server_cert_path:path
      ~cert_gid:!Xapi_globs.server_cert_group_id
  in
  match installation with
  | Ok cert ->
      cert
  | Error (`Msg (err, msg)) ->
      raise_server_error msg err
