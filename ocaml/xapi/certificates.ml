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

(* Certificate locations:
 * a) stunnel external             = /etc/xensource/xapi-ssl.pem
 * b) stunnel SNI (internal)       = /etc/xensource/xapi-pool-tls.pem
 * c) user trusted cert folder     = /etc/stunnel/certs/
 * d) internal trusted cert folder = /etc/stunnel/certs-pool/
 * e) appliance trusted bundle     = /etc/stunnel/xapi-stunnel-ca-bundle.pem
 * f) host-in-pool trusted bundle  = /etc/stunnel/xapi-pool-ca-bundle.pem
 *
 * Note that the bundles (e) and (f) are generated automatically using the contents of (c) and (d) respectively *)

type t_trusted =
  | Root_legacy
  | CRL
  | Root of API.certificate_purpose list
  | Pinned of API.certificate_purpose list

type category = [`Root_legacy | `CRL | `Root | `Pinned]

let all_purposes = [] :: List.map (fun x -> [x]) API.certificate_purpose__all

let all_trusted_kinds =
  List.concat
    [
      List.map (fun p -> Root p) all_purposes
    ; List.map (fun p -> Pinned p) all_purposes
    ; [Root_legacy; CRL]
    ]

let pem_of_string x =
  match X509.Certificate.decode_pem x with
  | Error _ ->
      D.error "pem_of_string: failed to parse certificate string" ;
      raise
        Api_errors.(Server_error (invalid_value, ["certificate"; "<omitted>"]))
  | Ok x ->
      x

let library_path = function
  | Root_legacy ->
      !Xapi_globs.trusted_certs_dir
  | CRL ->
      Stunnel.crl_path
  | Root _ | Pinned _ ->
      !Xapi_globs.trusted_certs_by_purpose_dir

let ( // ) = Filename.concat

let library_filename kind name = library_path kind // name

let ps = Printf.sprintf

let of_purposes = function
  | [] ->
      ["general"]
  | purposes ->
      List.map (fun p -> Record_util.certificate_purpose_to_string p) purposes

type trusted_store = {cert_dir: string; bundle: (string * string) option}

let trusted_store_locations kind =
  let parent = library_path kind in
  match kind with
  | CRL ->
      [{cert_dir= parent; bundle= None}]
  | Root_legacy ->
      [
        {
          cert_dir= parent
        ; bundle=
            Some
              ( Filename.dirname !Xapi_globs.stunnel_bundle_path
              , Filename.basename !Xapi_globs.stunnel_bundle_path
              )
        }
      ]
  | Root purposes ->
      List.map
        (fun p ->
          {
            cert_dir= parent // ps "ca-%s" p
          ; bundle= Some (parent, ps "ca-bundle-%s.pem" p)
          }
        )
        (of_purposes purposes)
  | Pinned purposes ->
      List.map
        (fun p ->
          {
            cert_dir= parent // ps "pinned-%s" p
          ; bundle= Some (parent, ps "pinned-bundle-%s.pem" p)
          }
        )
        (of_purposes purposes)

let with_cert_store kind f =
  trusted_store_locations kind
  |> List.iter (fun store -> f ~cert_dir:store.cert_dir ~bundle:store.bundle)

let with_cert_paths kind name f =
  with_cert_store kind @@ fun ~cert_dir ~bundle ->
  f cert_dir (cert_dir // name) bundle

let mkdir_cert_path kind =
  trusted_store_locations kind
  |> List.iter (fun store ->
      Unixext.mkdir_rec store.cert_dir 0o700 ;
      Option.iter (fun (dir, _) -> Unixext.mkdir_rec dir 0o700) store.bundle ;
      ()
  )

let rehash path =
  match Sys.file_exists !Xapi_globs.c_rehash with
  | true ->
      Forkhelpers.execute_command_get_output !Xapi_globs.c_rehash [path]
      |> ignore
  | false ->
      (* c_rehash will be replaced with openssl sub-command in newer version *)
      Forkhelpers.execute_command_get_output !Constants.openssl_path
        ["rehash"; path]
      |> ignore

let update_ca_bundle () = Helpers.update_ca_bundle ()

let to_string = function
  | Root_legacy ->
      "legacy root CA certificate"
  | CRL ->
      "CRL"
  | Root purposes ->
      List.map API.certificate_purpose_to_string purposes
      |> String.concat "; "
      |> Printf.sprintf "root CA certificate [%s]"
  | Pinned purposes ->
      List.map API.certificate_purpose_to_string purposes
      |> String.concat "; "
      |> Printf.sprintf "pinned leaf certificate [%s]"

(** {pp_hash hash} outputs the hexadecimal representation of the {hash}
    adding a colon between every octet, in uppercase.
 *)
let pp_hash hash =
  let hex = Hex.(show @@ of_string hash) in
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
    | Root_legacy | Root _ | Pinned _ ->
        certificate_name_invalid
    | CRL ->
        crl_name_invalid
  in
  raise_server_error [n] err

let validate_name kind name =
  if is_unsafe name then raise_name_invalid kind name

let raise_already_exists kind n =
  let err =
    match kind with
    | Root_legacy | Root _ | Pinned _ ->
        certificate_already_exists
    | CRL ->
        crl_already_exists
  in
  raise_server_error [n] err

let raise_does_not_exist kind n =
  let err =
    match kind with
    | Root_legacy | Root _ | Pinned _ ->
        certificate_does_not_exist
    | CRL ->
        crl_does_not_exist
  in
  raise_server_error [n] err

let raise_corrupt kind n =
  let err =
    match kind with
    | Root_legacy | Root _ | Pinned _ ->
        certificate_corrupt
    | CRL ->
        crl_corrupt
  in
  raise_server_error [n] err

let raise_library_corrupt () =
  raise (Server_error (certificate_library_corrupt, []))

module Db_util : sig
  type name = string

  val add_cert :
       __context:Context.t
    -> type':
         [< `host of API.ref_host
         | `host_internal of API.ref_host
         | `ca of name
         | `pinned ]
    -> purpose:API.certificate_purpose list
    -> X509.Certificate.t
    -> API.ref_Certificate * string

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

  val get_trusted_certs :
       __context:Context.t
    -> [`ca | `pinned]
    -> (API.ref_Certificate * API.certificate_t) list
end = struct
  module Date = Clock.Date

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

  module PurposeSet = Set.Make (struct
    type t = API.certificate_purpose

    let compare = Stdlib.compare
  end)

  let add_cert ~__context ~type' ~purpose certificate =
    let name, host, _type, post_action =
      match type' with
      | `host host ->
          ("", host, `host, Fun.id)
      | `host_internal host ->
          ("", host, `host_internal, Fun.id)
      | `ca name when name <> "" ->
          let certs = get_ca_certs ~__context name in
          let remove_obsoleted_copies () =
            List.iter (remove_cert_by_ref ~__context) certs
          in
          (name, Ref.null, `ca, remove_obsoleted_copies)
      | `ca _name ->
          ("", Ref.null, `ca, Fun.id)
      | `pinned ->
          ("", Ref.null, `pinned, Fun.id)
    in
    let date_of_ptime time = Date.of_unix_time (Ptime.to_float_s time) in
    let dates_of_ptimes (a, b) = (date_of_ptime a, date_of_ptime b) in
    let not_before, not_after =
      dates_of_ptimes (X509.Certificate.validity certificate)
    in
    let fingerprint_sha256 = pp_fingerprint ~hash_type:`SHA256 certificate in
    let fingerprint_sha1 = pp_fingerprint ~hash_type:`SHA1 certificate in
    let expr =
      let open Xapi_database.Db_filter_types in
      let type' = Record_util.certificate_type_to_string _type in
      let type' = Eq (Field "type", Literal type') in
      let fingerprint_sha256 =
        Eq (Field "fingerprint_sha256", Literal fingerprint_sha256)
      in
      And (type', fingerprint_sha256)
    in
    Db.Certificate.get_records_where ~__context ~expr
    |> List.filter (fun (_, cert_rec) -> cert_rec.API.certificate_name = "")
    |> List.filter (fun (_, cert_rec) ->
        let open PurposeSet in
        let s1 = of_list purpose in
        let s2 = of_list cert_rec.API.certificate_purpose in
        equal s1 s2 || not (is_empty (inter s1 s2))
    )
    |> List.iter (fun _ ->
        raise_server_error [fingerprint_sha256]
          trusted_certificate_already_exists
    ) ;
    let uuid = Uuidx.(to_string (make ())) in
    let ref' = Ref.make () in
    Db.Certificate.create ~__context ~ref:ref' ~uuid ~host ~not_before
      ~not_after ~fingerprint:fingerprint_sha256 ~fingerprint_sha256
      ~fingerprint_sha1 ~name ~_type ~purpose ;
    debug "added cert (name='%s') under uuid=%s ref=%s" name uuid
      (Ref.string_of ref') ;
    post_action () ;
    (ref', uuid)

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
      let type' = Eq (Field "type", Literal "ca") in
      let name = Not (Eq (Field "name", Literal "")) in
      And (type', name)
    in
    Db.Certificate.get_refs_where ~__context ~expr

  let get_trusted_certs ~__context cert_type =
    let cert_type = Record_util.certificate_type_to_string cert_type in
    let expr =
      let open Xapi_database.Db_filter_types in
      let type' = Eq (Field "type", Literal cert_type) in
      (* Unlike Root_legacy, the Root and Pinned certificates are of empty names always *)
      let name' = Eq (Field "name", Literal "") in
      And (type', name')
    in
    Db.Certificate.get_records_where ~__context ~expr
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

let update_bundle kind cert_dir bundle_path =
  match kind with
  | Root_legacy | CRL ->
      update_ca_bundle ()
  | Root _ | Pinned _ ->
      () (* TODO: implementation in the following commit *)

let host_install kind ~name ~cert =
  validate_name kind name ;
  with_cert_paths kind name @@ fun cert_dir cert_path bundle ->
  if Sys.file_exists cert_path then raise_already_exists kind name ;
  debug "%s: Installing %s (name='%s') under %s" __FUNCTION__ (to_string kind)
    name cert_path ;
  try
    mkdir_cert_path kind ;
    Unixext.write_string_to_file cert_path cert ;
    Unix.chmod cert_path 0o644 ;
    Option.iter
      (fun (bundle_dir, bundle_name) ->
        debug "%s: Updating bundle %s for %s (name='%s')" __FUNCTION__
          (bundle_dir // bundle_name)
          (to_string kind) name ;
        update_bundle cert_dir (bundle_dir // bundle_name) ;
        Unix.chmod (bundle_dir // bundle_name) 0o644
      )
      bundle ;
    rehash cert_dir
  with e ->
    warn "Exception installing %s %s: %s" (to_string kind) name
      (ExnHelper.string_of_exn e) ;
    raise_library_corrupt ()

let host_uninstall kind ~name ~force =
  validate_name kind name ;
  with_cert_store kind @@ fun ~cert_dir ~bundle ->
  let cert_path = cert_dir // name in
  debug "Uninstalling %s %s" (to_string kind) cert_path ;
  if Sys.file_exists cert_path then (
    try
      Sys.remove cert_path ;
      rehash cert_dir ;
      () (* TODO: implementation in the following commit *)
    with e ->
      warn "Exception uninstalling %s %s: %s" (to_string kind) name
        (ExnHelper.string_of_exn e) ;
      raise_corrupt kind name
  ) else if force then
    info "Certificate file %s is non-existent but ignoring this due to force."
      name
  else
    raise_does_not_exist kind name

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
  X509.Certificate.decode_pem pem >>| X509.Certificate.hostnames

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

let name_of_uuid uuid = Printf.sprintf "%s.pem" uuid

let db_type_of_category category =
  match category with `Root -> `ca | `Pinned -> `pinned
