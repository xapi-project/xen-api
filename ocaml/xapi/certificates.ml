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

type t_trusted = CA_Certificate | CRL | Root_CA | Leaf_Pinned

let all_trusted_kinds = [CA_Certificate; CRL; Root_CA; Leaf_Pinned]

let all_purposes = [] :: List.map (fun x -> [x]) API.certificate_purpose__all

let pem_of_string x =
  match X509.Certificate.decode_pem x with
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
  | Root_CA | Leaf_Pinned ->
      !Xapi_globs.trusted_certs_by_purpose_dir

let ( // ) = Filename.concat

let library_filename kind name = library_path kind // name

let string_of_purpose =
  Option.fold ~none:"general" ~some:Record_util.certificate_purpose_to_string

let ps = Printf.sprintf

let trusted_cert_dirs kind (purpose : API.certificate_purpose option) =
  let p = string_of_purpose purpose in
  let parent = library_path kind in
  match kind with
  | CA_Certificate | CRL ->
      [parent]
  | Root_CA ->
      [parent // ps "ca-%s" p]
  | Leaf_Pinned ->
      [parent // ps "pinned-%s" p]

let trusted_bundle_location kind (purpose : API.certificate_purpose option) =
  let parent = library_path kind in
  let p = string_of_purpose purpose in
  match kind with
  | CA_Certificate | CRL ->
      ( Filename.dirname !Xapi_globs.stunnel_bundle_path
      , [Filename.basename !Xapi_globs.stunnel_bundle_path]
      )
  | Root_CA ->
      (parent, [ps "ca-bundle-%s.pem" p])
  | Leaf_Pinned ->
      (parent, [ps "pinned-bundle-%s.pem" p])

let with_cert_store kind (purposes : API.certificate_purpose list) f =
  let ( let* ) l f = List.iter f l in
  match (kind, purposes) with
  | CA_Certificate, _ | CRL, _ | _, [] ->
      let* cert_dir = trusted_cert_dirs kind None in
      let bundle_dir, bundle_names = trusted_bundle_location kind None in
      let* bundle_name = bundle_names in
      f ~purpose:None ~cert_dir ~bundle_dir ~bundle_name
  | Root_CA, purposes | Leaf_Pinned, purposes ->
      let* purpose = purposes in
      let p' = Some purpose in
      let* cert_dir = trusted_cert_dirs kind p' in
      let bundle_dir, bundle_names = trusted_bundle_location kind p' in
      let* bundle_name = bundle_names in
      f ~purpose:p' ~cert_dir ~bundle_dir ~bundle_name

let with_cert_paths kind name (purposes : API.certificate_purpose list) f =
  with_cert_store kind purposes
  @@ fun ~purpose ~cert_dir ~bundle_dir ~bundle_name ->
  f purpose (cert_dir // name) (bundle_dir // bundle_name)

let mkdir_cert_path kind (purpose : API.certificate_purpose option) =
  trusted_cert_dirs kind purpose
  |> List.iter (fun cert_dir -> Unixext.mkdir_rec cert_dir 0o700) ;
  let bundle_dir, _ = trusted_bundle_location kind purpose in
  Unixext.mkdir_rec bundle_dir 0o700 ;
  ()

let mkdir_cert_paths kind purposes =
  match (kind, purposes) with
  | CA_Certificate, _ | CRL, _ | _, [] ->
      mkdir_cert_path kind None
  | Root_CA, purposes | Leaf_Pinned, purposes ->
      List.iter (fun p -> mkdir_cert_path kind (Some p)) purposes

let rehash' path =
  match Sys.file_exists !Xapi_globs.c_rehash with
  | true ->
      Forkhelpers.execute_command_get_output !Xapi_globs.c_rehash [path]
      |> ignore
  | false ->
      (* c_rehash will be replaced with openssl sub-command in newer version *)
      Forkhelpers.execute_command_get_output !Constants.openssl_path
        ["rehash"; path]
      |> ignore

let rehash () =
  let ( let* ) l f = List.iter f l in
  let* kind = all_trusted_kinds in
  let* purposes = all_purposes in
  with_cert_store kind purposes
  @@ fun ~purpose ~cert_dir ~bundle_dir:_ ~bundle_name:_ ->
  mkdir_cert_path kind purpose ;
  rehash' cert_dir

let update_pool_bundle () = Helpers.update_pool_bundle ()

let update_trusted_bundle cert_dir bundle_path =
  (* it is not safe for multiple instances of this bash script to be
   * running at the same time, so we must lock it.
   *
   * NB: we choose not to implement the lock inside the bash script
   * itself *)
  let m = Mutex.create () in
  fun () ->
    Xapi_stdext_threads.Threadext.Mutex.execute m (fun () ->
        let (_ : string), (_ : string) =
          Forkhelpers.execute_command_get_output
            "/opt/xensource/bin/update-ca-bundle.sh" [cert_dir; bundle_path]
        in
        ()
    )

let to_string = function
  | CA_Certificate ->
      "CA certificate"
  | CRL ->
      "CRL"
  | Root_CA ->
      "root CA certificate"
  | Leaf_Pinned ->
      "pinned leaf certificate"

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
    | CA_Certificate | Root_CA | Leaf_Pinned ->
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
    | CA_Certificate | Root_CA | Leaf_Pinned ->
        certificate_already_exists
    | CRL ->
        crl_already_exists
  in
  raise_server_error [n] err

let raise_does_not_exist kind n =
  let err =
    match kind with
    | CA_Certificate | Root_CA | Leaf_Pinned ->
        certificate_does_not_exist
    | CRL ->
        crl_does_not_exist
  in
  raise_server_error [n] err

let raise_corrupt kind n =
  let err =
    match kind with
    | CA_Certificate | Root_CA | Leaf_Pinned ->
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

  let add_cert ~__context ~type' ~purpose certificate =
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
    let uuid = Uuidx.(to_string (make ())) in
    let ref' = Ref.make () in
    Db.Certificate.create ~__context ~ref:ref' ~uuid ~host ~not_before
      ~not_after ~fingerprint:fingerprint_sha256 ~fingerprint_sha256
      ~fingerprint_sha1 ~name ~_type ~purpose ;
    debug "added cert %s under uuid=%s ref=%s" name uuid (Ref.string_of ref') ;
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
      Eq (Field "type", Literal "ca")
    in
    Db.Certificate.get_refs_where ~__context ~expr

  let get_trusted_certs ~__context cert_type =
    let cert_type = Record_util.certificate_type_to_string cert_type in
    let expr =
      let open Xapi_database.Db_filter_types in
      let type' = Eq (Field "type", Literal cert_type) in
      let name' = Eq (Field "name", Literal "") in
      And (type', name')
    in
    Db.Certificate.get_records_where ~__context ~expr
end

let name_of_uuid uuid = Printf.sprintf "%s.pem" uuid

let local_list' dir =
  List.filter
    (fun n ->
      let stat = Unix.lstat (dir // n) in
      stat.Unix.st_kind = Unix.S_REG
    )
    (Array.to_list (Sys.readdir dir))

let map_cert_store kind purposes f =
  match (kind, purposes) with
  | CA_Certificate, _ | CRL, _ | _, [] ->
      [f kind None]
  | Root_CA, purposes | Leaf_Pinned, purposes ->
      List.map (fun p -> f kind (Some p)) purposes

let local_list kind =
  all_purposes
  |> List.map (fun purposes ->
         map_cert_store kind purposes @@ fun kind purpose ->
         mkdir_cert_path kind purpose ;
         trusted_cert_dirs kind purpose
         |> List.map (fun cert_dir -> local_list' cert_dir)
         |> List.flatten
     )
  |> List.flatten
  |> List.flatten

let list_names ~__context kind =
  match kind with
  | CA_Certificate | CRL ->
      (* Retrieve from filesystem *)
      mkdir_cert_path kind None ;
      List.filter
        (fun n ->
          let stat = Unix.lstat (library_filename kind n) in
          stat.Unix.st_kind = Unix.S_REG
        )
        (Array.to_list (Sys.readdir (library_path kind)))
      |> List.map (fun name -> (name, []))
  | Root_CA | Leaf_Pinned ->
      (* Retrieve from XAPI database *)
      let cert_type = if kind = Root_CA then `ca else `pinned in
      Db_util.get_trusted_certs ~__context cert_type
      |> List.map (fun (_, cert_rec) ->
             mkdir_cert_paths kind cert_rec.API.certificate_purpose ;
             ( name_of_uuid cert_rec.API.certificate_uuid
             , cert_rec.API.certificate_purpose
             )
         )

let local_sync () =
  try rehash ()
  with e ->
    warn "Exception rehashing certificates: %s" (ExnHelper.string_of_exn e) ;
    raise_library_corrupt ()

let host_install kind ~name ~cert ~purpose =
  validate_name kind name ;
  with_cert_paths kind name purpose @@ fun purpose cert_path bundle_path ->
  if Sys.file_exists cert_path then
    raise_already_exists kind name ;
  debug "Installing %s %s under %s and in %s" (to_string kind) name cert_path
    bundle_path ;
  let cert_dir = Filename.dirname cert_path in
  try
    mkdir_cert_path kind purpose ;
    Unixext.write_string_to_file cert_path cert ;
    Unix.chmod cert_path 0o644 ;
    update_trusted_bundle cert_dir bundle_path () ;
    Unix.chmod bundle_path 0o644 ;
    rehash' cert_dir
  with e ->
    warn "Exception installing %s %s: %s" (to_string kind) name
      (ExnHelper.string_of_exn e) ;
    raise_library_corrupt ()

let host_uninstall kind ~name ~purpose ~force =
  validate_name kind name ;
  with_cert_store kind purpose
  @@ fun ~purpose:_ ~cert_dir ~bundle_dir ~bundle_name ->
  let cert_path = cert_dir // name in
  debug "Uninstalling %s %s" (to_string kind) cert_path ;
  let bundle_path = bundle_dir // bundle_name in
  if Sys.file_exists cert_path then (
    try
      Sys.remove cert_path ;
      update_trusted_bundle cert_dir bundle_path ()
    with e ->
      warn "Exception uninstalling %s %s: %s" (to_string kind) name
        (ExnHelper.string_of_exn e) ;
      raise_corrupt kind name
  ) else if force then
    info "Certificate file %s is non-existent but ignoring this due to force."
      name
  else
    raise_does_not_exist kind name

let get_cert kind purposes name =
  validate_name kind name ;
  let purpose =
    match purposes with [] -> None | purpose :: _ -> Some purpose
  in
  match trusted_cert_dirs kind purpose with
  | [] ->
      Helpers.internal_error
        "BUG: can't generate directory for %s with kind = %s and purpose = %s"
        name (to_string kind)
        (string_of_purpose purpose)
  | cert_dir :: _ -> (
      let filename = cert_dir // name in
      try Unixext.string_of_file filename
      with e ->
        warn "Exception reading %s %s: %s" (to_string kind) name
          (ExnHelper.string_of_exn e) ;
        raise_corrupt kind name
    )

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
      let names = List.map fst master_certs in
      List.iter
        (fun c ->
          if not (List.mem c names) then
            uninstall_func rpc session_id host c
        )
        host_certs ;
      List.iter
        (fun (c, purposes) ->
          if not (List.mem c host_certs) then
            install_func rpc session_id host c purposes
              (get_cert kind purposes c)
        )
        master_certs
  )

let sync_certs kind ~__context host =
  let main_certs = list_names ~__context kind in
  match kind with
  | CA_Certificate ->
      sync_certs_crls CA_Certificate
        (fun rpc session_id host ->
          Client.Host.certificate_list ~rpc ~session_id ~host
        )
        (fun rpc session_id host name _purpose cert ->
          Client.Host.install_ca_certificate ~rpc ~session_id ~host ~name ~cert
        )
        (fun rpc session_id host name ->
          Client.Host.uninstall_ca_certificate ~rpc ~session_id ~host ~name
            ~force:false
        )
        ~__context main_certs host
  | CRL ->
      sync_certs_crls CRL
        (fun rpc session_id host -> Client.Host.crl_list ~rpc ~session_id ~host)
        (fun rpc session_id host name _purpose crl ->
          Client.Host.crl_install ~rpc ~session_id ~host ~name ~crl
        )
        (fun rpc session_id host name ->
          Client.Host.crl_uninstall ~rpc ~session_id ~host ~name
        )
        ~__context main_certs host
  | Root_CA | Leaf_Pinned ->
      let ca = if kind = Root_CA then true else false in
      sync_certs_crls kind
        (fun rpc session_id host ->
          Client.Host.list_trusted_certificates ~rpc ~session_id ~host ~ca
        )
        (fun rpc session_id host name purpose cert ->
          Client.Host.install_trusted_certificate ~rpc ~session_id ~host ~ca
            ~name ~cert ~purpose
        )
        (fun rpc session_id host name ->
          Client.Host.uninstall_trusted_certificate ~rpc ~session_id ~host ~ca
            ~name ~force:false
        )
        ~__context main_certs host

let sync_certs_all_hosts kind ~__context hosts_but_master =
  let exn = ref None in
  List.iter
    (fun host -> try sync_certs kind ~__context host with e -> exn := Some e)
    hosts_but_master ;
  match !exn with Some e -> raise e | None -> ()

let pool_sync ~__context kinds =
  let hosts = Db.Host.get_all ~__context in
  let master = Helpers.get_localhost ~__context in
  let hosts_but_master = List.filter (fun h -> h <> master) hosts in
  sync_all_hosts ~__context hosts ;
  let kinds = if kinds = [] then all_trusted_kinds else kinds in
  debug "Sync '%s' across the pool"
    (List.map to_string kinds |> String.concat "; ") ;
  List.iter
    (fun kind -> sync_certs_all_hosts kind ~__context hosts_but_master)
    kinds

let pool_install kind ~__context ~name ~cert ~purpose =
  host_install kind ~name ~cert ~purpose ;
  try pool_sync ~__context [kind]
  with exn ->
    ( try host_uninstall kind ~name ~purpose ~force:false
      with e ->
        warn "Exception unwinding install of %s %s: %s" (to_string kind) name
          (ExnHelper.string_of_exn e)
    ) ;
    raise exn

let pool_uninstall kind ~__context ~name ~purpose ~force =
  host_uninstall kind ~name ~purpose ~force ;
  pool_sync ~__context []

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
