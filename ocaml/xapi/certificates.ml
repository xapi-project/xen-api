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
open Stdext
open Xstringext
open Unixext

open Forkhelpers

open Api_errors

open Client

module D=Debug.Make(struct let name="certificates" end)
open D

let c_rehash = "/usr/bin/c_rehash"
let pem_certificate_header = "-----BEGIN CERTIFICATE-----"
let pem_certificate_footer = "-----END CERTIFICATE-----"

let library_path is_cert =
  if is_cert then Stunnel.certificate_path else Stunnel.crl_path

let library_filename is_cert name =
  Filename.concat (library_path is_cert) name

let mkdir_cert_path is_cert =
  mkdir_rec (library_path is_cert) 0o700

let rehash' path = ignore (execute_command_get_output c_rehash [ path ])

let rehash () =
  mkdir_cert_path true;
  mkdir_cert_path false;
  rehash' (library_path true);
  rehash' (library_path false)

let get_type is_cert =
  if is_cert then "certificate" else "CRL"

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

let not_safe is_cert name =
  name.[0] = '.' ||
  not (String.endswith ".pem" name) ||
  not_safe_chars name

let raise_server_error n err =
  raise (Server_error(err, [n]))

let raise_name_invalid is_cert n =
  raise_server_error n
    (if is_cert then certificate_name_invalid else crl_name_invalid)

let raise_already_exists is_cert n =
  raise_server_error n
    (if is_cert then certificate_already_exists else crl_already_exists)

let raise_does_not_exist is_cert n =
  raise_server_error n
    (if is_cert then certificate_does_not_exist else crl_does_not_exist)

let raise_corrupt is_cert n =
  raise_server_error n
    (if is_cert then certificate_corrupt else crl_corrupt)

let raise_library_corrupt () =
  raise (Server_error (certificate_library_corrupt, []))

let local_list is_cert =
  mkdir_cert_path is_cert;
  List.filter
    (fun n ->
       let stat = Unix.lstat (library_filename is_cert n) in
       stat.Unix.st_kind = Unix.S_REG)
    (Array.to_list (Sys.readdir (library_path is_cert)))

let local_sync () =
  try
    rehash()
  with
  | e ->
    warn "Exception rehashing certificates: %s"
      (ExnHelper.string_of_exn e);
    raise_library_corrupt()

let cert_perms is_cert =
  let stat = Unix.stat (library_path is_cert) in
  debug "%d %d" (stat.Unix.st_perm land 0o666) stat.Unix.st_perm;
  stat.Unix.st_perm land 0o666

let host_install is_cert ~name ~cert =
  if not_safe is_cert name then
    raise_name_invalid is_cert name;
  let filename = library_filename is_cert name in
  if Sys.file_exists filename then
    raise_already_exists is_cert name;
  debug "Installing %s %s" (get_type is_cert) name;
  try
    mkdir_cert_path is_cert;
    write_string_to_file filename cert;
    Unix.chmod filename (cert_perms is_cert);
    ignore(execute_command_get_output "/opt/xensource/bin/update-ca-bundle.sh" [])
  with
  | e ->
    warn "Exception installing %s %s: %s" (get_type is_cert) name
      (ExnHelper.string_of_exn e);
    raise_library_corrupt()

let host_uninstall is_cert ~name =
  if not_safe is_cert name then
    raise_name_invalid is_cert name;
  let filename = library_filename is_cert name in
  if not (Sys.file_exists filename) then
    raise_does_not_exist is_cert name;
  debug "Uninstalling %s %s" (get_type is_cert) name;
  try
    Sys.remove filename;
    rehash()
  with
  | e ->
    warn "Exception uninstalling %s %s: %s" (get_type is_cert) name
      (ExnHelper.string_of_exn e);
    raise_corrupt is_cert name

let get_cert is_cert name =
  if not_safe is_cert name then
    raise_name_invalid is_cert name;
  let filename = library_filename is_cert name in
  try
    string_of_file filename
  with
  | e ->
    warn "Exception reading %s %s: %s" (get_type is_cert) name
      (ExnHelper.string_of_exn e);
    raise_corrupt is_cert name

let sync_all_hosts ~__context hosts =
  let exn = ref None in
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       List.iter
         (fun host ->
            try
              Client.Host.certificate_sync rpc session_id host
            with
            | e ->
              exn := Some e)
         hosts);
  match !exn with
  | Some e -> raise e
  | None -> ()

let sync_certs_crls is_cert list_func install_func uninstall_func
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
              install_func rpc session_id host c (get_cert is_cert c))
         master_certs)

let sync_certs is_cert ~__context master_certs host =
  if is_cert then
    sync_certs_crls true
      (fun rpc session_id host ->
         Client.Host.certificate_list rpc session_id host)
      (fun rpc session_id host c cert ->
         Client.Host.certificate_install rpc session_id host c cert)
      (fun rpc session_id host c ->
         Client.Host.certificate_uninstall rpc session_id host c)
      ~__context master_certs host
  else
    sync_certs_crls false
      (fun rpc session_id host ->
         Client.Host.crl_list rpc session_id host)
      (fun rpc session_id host c cert ->
         Client.Host.crl_install rpc session_id host c cert)
      (fun rpc session_id host c ->
         Client.Host.crl_uninstall rpc session_id host c)
      ~__context master_certs host

let sync_certs_all_hosts is_cert ~__context master_certs hosts_but_master =
  let exn = ref None in
  List.iter
    (fun host ->
       try
         sync_certs is_cert ~__context master_certs host
       with
       | e ->
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
  let master_certs = local_list true in
  let master_crls = local_list false in
  sync_certs_all_hosts true ~__context master_certs hosts_but_master;
  sync_certs_all_hosts false ~__context master_crls hosts_but_master

let pool_install is_cert ~__context ~name ~cert =
  host_install is_cert ~name ~cert;
  try
    pool_sync ~__context
  with
  | exn ->
    begin
      try
        host_uninstall is_cert ~name
      with
      | e ->
        warn "Exception unwinding install of %s %s: %s"
          (get_type is_cert) name (ExnHelper.string_of_exn e)
    end;
    raise exn

let pool_uninstall is_cert ~__context ~name =
  host_uninstall is_cert ~name;
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
      (trim_cert (String.split '\n' (string_of_file !Xapi_globs.server_cert_path)))
  with
  | e ->
    warn "Exception reading server certificate: %s"
      (ExnHelper.string_of_exn e);
    raise_library_corrupt()

let hostnames_of_pem_cert pem =
  Cstruct.of_string pem |>
  X509.Encoding.Pem.Certificate.of_pem_cstruct1 |>
  X509.hostnames
