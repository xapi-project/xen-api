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

module D = Debug.Make (struct let name = "cert_refresh" end)

open D

(* given a [filename], replace its extension with [ext]. Does not change
   the file system *)
let replace_extension filename ~ext =
  let base =
    match Filename.extension filename with
    | "" ->
        filename
    | _ ->
        Filename.remove_extension filename
  in
  Printf.sprintf "%s.%s" base ext

(* Path to host server certificate (in PEM format) *)
let cert_path = function
  | `host ->
      !Xapi_globs.server_cert_path
  | `host_internal ->
      !Xapi_globs.server_cert_internal_path

(* Paths to certificates that we are about to use *)
let new_cert_path type' = replace_extension (cert_path type') ~ext:"new"

let backup_cert_path type' = replace_extension (cert_path type') ~ext:"bak"

module HostSet = Set.Make (struct
  type t = API.ref_host

  let compare = Stdlib.compare
end)

let unreachable_hosts ~__context =
  let live = Helpers.get_live_hosts ~__context in
  let pool = Xapi_pool_helpers.get_master_slaves_list ~__context in
  HostSet.(diff (of_list pool) (of_list live))

let maybe_update_clustering_tls_config ~__context =
  let open Xapi_clustering in
  with_clustering_lock __LOC__ @@ fun () ->
  let host = Helpers.get_localhost ~__context in
  match Xapi_clustering.find_cluster_host ~__context ~host with
  | None ->
      ()
  | Some self ->
      let verify = Stunnel_client.get_verify_by_default () in
      Xapi_cluster_host.set_tls_config ~__context ~self ~verify

(* On this host and for this host, create a new server certificate and
   distribute it in the pool *)
let host ~__context ~type' =
  let host = Helpers.get_localhost ~__context in
  let pem = cert_path type' in
  let path = new_cert_path type' in
  let valid_for_days = !Xapi_globs.cert_expiration_days in
  let uuid = Inventory.lookup Inventory._installation_uuid in
  let cert = Gencertlib.Selfcert.xapi_pool ~uuid ~valid_for_days path (-1) in
  let bak = backup_cert_path type' in
  let unreachable = unreachable_hosts ~__context in
  if not @@ HostSet.is_empty unreachable then
    raise
      Api_errors.(
        Server_error
          (cannot_contact_host, [Ref.string_of (HostSet.choose unreachable)])
      ) ;
  let content = X509.Certificate.encode_pem cert in
  (* distribute public part of new cert in pool *)
  Cert_distrib.distribute_new_host_cert ~__context ~host ~content ;
  (* replace certs in file system on host *)
  info "renaming cert %s to %s" path pem ;
  Sys.rename pem bak ;
  Sys.rename path pem ;
  maybe_update_clustering_tls_config ~__context ;
  (* remove old from database, add new *)
  Certificates.Db_util.get_host_certs ~__context ~type' ~host
  |> List.iter (Certificates.Db_util.remove_cert_by_ref ~__context) ;
  let ref =
    match type' with
    | `host ->
        Certificates.Db_util.add_cert ~__context ~type':(`host host) cert
    | `host_internal ->
        Certificates.Db_util.add_cert ~__context ~type':(`host_internal host)
          cert
  in
  (* We might have a slow client that connects using the old cert and
     has not picked up the new cert. To avoid that the connection fails,
     continue using the old cert for a small time before serving the new
     cert *)
  Thread.delay 5.0 ;
  Xapi_stunnel_server.reload () ;
  ref

(* The stunnel clients trust the old and the new [host] server cert.  On
   the local host, rename the old cert and re-create the cert bundle
   without it *)
let remove_stale_cert ~__context ~host ~type' =
  let uuid = Db.Host.get_uuid ~__context ~self:host in
  let directory =
    match type' with
    | `host ->
        !Xapi_globs.trusted_certs_dir
    | `host_internal ->
        !Xapi_globs.trusted_pool_certs_dir
  in
  let next = Filename.concat directory (Printf.sprintf "%s.new.pem" uuid) in
  let pem = Filename.concat directory (Printf.sprintf "%s.pem" uuid) in
  let bak = Filename.concat directory (Printf.sprintf "%s.bak" uuid) in
  if Sys.file_exists next && Sys.file_exists pem then (
    info "cleanup - renaming %s to %s" next pem ;
    Sys.rename pem bak ;
    Sys.rename next pem ;
    Certificates.update_ca_bundle ()
  ) else
    info "cleanup - no new cert %s found - skipping" next
