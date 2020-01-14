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
module Unixext = Stdext.Unixext

open Api_errors

open Client

module D=Debug.Make(struct let name="certificates" end)
open D

type t_trusted = Certificate | CRL

let c_rehash = "/usr/bin/c_rehash"
let pem_certificate_header = "-----BEGIN CERTIFICATE-----"
let pem_certificate_footer = "-----END CERTIFICATE-----"

let certificate_path = "/etc/stunnel/certs"

let library_path = function
  | Certificate -> certificate_path
  | CRL -> Stunnel.crl_path

let library_filename kind name =
  Filename.concat (library_path kind) name

let mkdir_cert_path kind =
  Unixext.mkdir_rec (library_path kind) 0o700

let rehash' path = ignore (Forkhelpers.execute_command_get_output c_rehash [ path ])

let rehash () =
  mkdir_cert_path Certificate;
  mkdir_cert_path CRL;
  rehash' (library_path Certificate);
  rehash' (library_path CRL)

let update_ca_bundle () = ignore (Forkhelpers.execute_command_get_output "/opt/xensource/bin/update-ca-bundle.sh" [])

let to_string = function
  | Certificate -> "certificate"
  | CRL -> "CRL"

let safe_char c =
  match c with
  | 'A'..'Z'
  | 'a'..'z'
  | '0'..'9'
  | '.' | '_' | '-' ->
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

let is_unsafe kind name =
  name.[0] = '.' ||
  not (Astring.String.is_suffix ~affix:".pem" name) ||
  not_safe_chars name

let raise_server_error parameters err =
  raise (Server_error(err, parameters))

let raise_name_invalid kind n =
  let err = match kind with
  | Certificate -> certificate_name_invalid
  | CRL -> crl_name_invalid
  in
  raise_server_error [n] err

let raise_already_exists kind n =
  let err = match kind with
  | Certificate -> certificate_already_exists
  | CRL -> crl_already_exists
  in
  raise_server_error [n] err

let raise_does_not_exist kind n =
  let err = match kind with
  | Certificate -> certificate_does_not_exist
  | CRL -> crl_does_not_exist
  in
  raise_server_error [n] err

let raise_corrupt kind n =
  let err = match kind with
  | Certificate -> certificate_corrupt
  | CRL -> crl_corrupt
  in
  raise_server_error [n] err

let raise_library_corrupt () =
  raise (Server_error (certificate_library_corrupt, []))

let local_list kind =
  mkdir_cert_path kind;
  List.filter
    (fun n ->
       let stat = Unix.lstat (library_filename kind n) in
       stat.Unix.st_kind = Unix.S_REG)
    (Array.to_list (Sys.readdir (library_path kind)))

let local_sync () =
  try
    rehash()
  with e ->
    warn "Exception rehashing certificates: %s"
      (ExnHelper.string_of_exn e);
    raise_library_corrupt()

let cert_perms kind =
  let stat = Unix.stat (library_path kind) in
  let mask = 0o666 in
  let perm = stat.Unix.st_perm land mask in
  debug "%d %d" perm stat.Unix.st_perm;
  perm

let host_install kind ~name ~cert =
  if is_unsafe kind name then
    raise_name_invalid kind name;
  let filename = library_filename kind name in
  if Sys.file_exists filename then
    raise_already_exists kind name;
  debug "Installing %s %s" (to_string kind) name;
  try
    mkdir_cert_path kind;
    Unixext.write_string_to_file filename cert;
    Unix.chmod filename (cert_perms kind);
    update_ca_bundle ()
  with e ->
    warn "Exception installing %s %s: %s" (to_string kind) name
      (ExnHelper.string_of_exn e);
    raise_library_corrupt()

let host_uninstall kind ~name =
  if is_unsafe kind name then
    raise_name_invalid kind name;
  let filename = library_filename kind name in
  if not (Sys.file_exists filename) then
    raise_does_not_exist kind name;
  debug "Uninstalling %s %s" (to_string kind) name;
  try
    Sys.remove filename;
    update_ca_bundle ()
  with e ->
    warn "Exception uninstalling %s %s: %s" (to_string kind) name
      (ExnHelper.string_of_exn e);
    raise_corrupt kind name

let get_cert kind name =
  if is_unsafe kind name then
    raise_name_invalid kind name;
  let filename = library_filename kind name in
  try
    Unixext.string_of_file filename
  with e ->
    warn "Exception reading %s %s: %s" (to_string kind) name
      (ExnHelper.string_of_exn e);
    raise_corrupt kind name

let sync_all_hosts ~__context hosts =
  let exn = ref None in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       List.iter
         (fun host ->
            try
              Client.Host.certificate_sync rpc session_id host
            with e ->
              exn := Some e)
         hosts);
  match !exn with
  | Some e -> raise e
  | None -> ()

let sync_certs_crls kind list_func install_func uninstall_func
    ~__context master_certs host =
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       let host_certs = list_func rpc session_id host in
       List.iter
         (fun c ->
            if not (List.mem c master_certs) then
              uninstall_func rpc session_id host c)
         host_certs;
       List.iter
         (fun c ->
            if not (List.mem c host_certs) then
              install_func rpc session_id host c (get_cert kind c))
         master_certs)

let sync_certs kind ~__context master_certs host =
  match kind with
  | Certificate ->
    sync_certs_crls Certificate
      (fun rpc session_id host ->
         Client.Host.certificate_list rpc session_id host)
      (fun rpc session_id host c cert ->
         Client.Host.certificate_install rpc session_id host c cert)
      (fun rpc session_id host c ->
         Client.Host.certificate_uninstall rpc session_id host c)
      ~__context master_certs host
  | CRL ->
    sync_certs_crls CRL
      (fun rpc session_id host ->
         Client.Host.crl_list rpc session_id host)
      (fun rpc session_id host c cert ->
         Client.Host.crl_install rpc session_id host c cert)
      (fun rpc session_id host c ->
         Client.Host.crl_uninstall rpc session_id host c)
      ~__context master_certs host

let sync_certs_all_hosts kind ~__context master_certs hosts_but_master =
  let exn = ref None in
  List.iter
    (fun host ->
       try
         sync_certs kind ~__context master_certs host
       with e ->
         exn := Some e)
    hosts_but_master;
  match !exn with
  | Some e -> raise e
  | None -> ()

let pool_sync ~__context =
  let hosts = Db.Host.get_all ~__context in
  let master = Helpers.get_localhost ~__context in
  let hosts_but_master = List.filter (fun h -> h <> master) hosts in

  sync_all_hosts ~__context hosts;
  let master_certs = local_list Certificate in
  let master_crls = local_list CRL in
  sync_certs_all_hosts Certificate ~__context master_certs hosts_but_master;
  sync_certs_all_hosts CRL ~__context master_crls hosts_but_master

let pool_install kind ~__context ~name ~cert =
  host_install kind ~name ~cert;
  try
    pool_sync ~__context
  with exn ->
    begin
      try
        host_uninstall kind ~name
      with e ->
        warn "Exception unwinding install of %s %s: %s"
          (to_string kind) name (ExnHelper.string_of_exn e)
    end;
    raise exn

let pool_uninstall kind ~__context ~name =
  host_uninstall kind ~name;
  pool_sync ~__context

let rec trim_cert = function
  | x :: xs ->
    if x = pem_certificate_header then
      trim_cert' [x] xs
    else
      trim_cert xs
  | [] ->
    []

and trim_cert' acc = function
  | x :: xs ->
    if x = pem_certificate_footer then
      List.rev (x :: acc)
    else
      trim_cert' (x :: acc) xs
  | [] ->
    []

let get_server_certificate () =
  try
    String.concat "\n"
      (trim_cert (String.split_on_char '\n' (Unixext.string_of_file !Xapi_globs.server_cert_path)))
  with e ->
    warn "Exception reading server certificate: %s"
      (ExnHelper.string_of_exn e);
    raise_library_corrupt ()

let hostnames_of_pem_cert pem =
  let open Rresult.R.Infix in
  Cstruct.of_string pem
  |> X509.Certificate.decode_pem
  >>| X509.Certificate.hostnames

let validate_private_key pkcs8_private_key =
  let ensure_key_length = function
  | `RSA priv ->
    let length = Nocrypto.Rsa.priv_bits priv in
    if length < 2048 || length > 4096 then
      Error (`Msg (server_certificate_key_rsa_length_not_supported,
        [Int.to_string length]))
    else
      Ok priv
  in

  let raw_pem = Cstruct.of_string pkcs8_private_key in
  let open Rresult in
  X509.Private_key.decode_pem raw_pem
  |> R.reword_error
      (fun (`Msg err_msg) ->
        let unknown_algorithm = "Unknown algorithm " in
        if Astring.String.is_prefix ~affix:"multi-prime RSA" err_msg then
          `Msg (server_certificate_key_rsa_multi_not_supported, [])
        else if Astring.String.is_prefix ~affix:unknown_algorithm err_msg then
          `Msg (server_certificate_key_algorithm_not_supported,
            [Astring.String.with_range ~first:(String.length unknown_algorithm) err_msg])
        else
          `Msg (server_certificate_key_invalid, []))
  >>= ensure_key_length
  |> function
    | Ok priv -> priv
    | Error `Msg (err, msg) -> raise_server_error msg err
