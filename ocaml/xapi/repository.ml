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

module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "repository" end)

open D

let reposync_mutex = Mutex.create ()

let create_repository_record ~__context ~name_label ~name_description ~binary_url ~source_url =
  let ref = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.Repository.create
    ~__context
    ~ref
    ~uuid
    ~name_label
    ~name_description
    ~binary_url
    ~source_url
    ~hash:""
    ~up_to_date:false;
  ref

let assert_url_is_valid ~url =
  try
    let uri = Uri.of_string url in
    match Uri.scheme uri with
    | Some "http" | Some "https" -> 
      begin match Uri.host uri with
        | Some h ->
          begin match !Xapi_globs.repository_domain_name_allowlist with
            | [] -> ()
            | l ->
              begin match List.exists (fun d -> Astring.String.is_suffix ("." ^ d) h) l with
                | true -> ()
                | false ->
                  let msg = "host is not in allowlist" in
                  raise Api_errors.(Server_error (internal_error, [msg]))
              end
          end
        | None -> raise Api_errors.(Server_error (internal_error, ["invalid host in url"]))
      end
    | _ -> raise Api_errors.(Server_error (internal_error, ["invalid scheme in url"]))
  with e ->
    error "Invalid url %s: %s" url (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (invalid_base_url, [url]))

let introduce ~__context ~name_label ~name_description ~binary_url ~source_url =
  assert_url_is_valid ~url:binary_url;
  assert_url_is_valid ~url:source_url;
  Db.Repository.get_all ~__context
  |> List.iter (fun ref ->
      if name_label = Db.Repository.get_name_label ~__context ~self:ref
      || binary_url = Db.Repository.get_binary_url ~__context ~self:ref then
        raise Api_errors.( Server_error (repository_already_exists, [(Ref.string_of ref)]) ));
  create_repository_record  ~__context ~name_label ~name_description ~binary_url ~source_url

let forget ~__context ~self =
  let pool = Helpers.get_pool ~__context in
  let enabled = Db.Pool.get_repository ~__context ~self:pool in
  if enabled = self then
    raise Api_errors.(Server_error (repository_is_in_use, []))
  else
    Db.Repository.destroy ~__context ~self

let with_reposync_lock f =
  if Mutex.try_lock reposync_mutex then
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () -> f ())
      (fun () -> Mutex.unlock reposync_mutex)
  else
    raise Api_errors.(Server_error (reposync_in_progress, []))

let get_enabled_repository ~__context =
  let pool = Helpers.get_pool ~__context in
  match Db.Pool.get_repository ~__context ~self:pool with
  | ref when ref <> Ref.null -> ref
  | _ ->
      raise Api_errors.(Server_error (no_repository_enabled, []))

let clean_yum_cache name =
  try
    let params =
        [
          "--disablerepo=*";
          Printf.sprintf "--enablerepo=%s" name;
          "clean"; "all"
        ]
    in
    ignore (Helpers.call_script !Xapi_globs.yum_cmd params)
  with e ->
    warn "Unable to clean YUM cache for %s: %s" name (ExnHelper.string_of_exn e)

let cleanup_pool_repo () =
  try
    clean_yum_cache !Xapi_globs.pool_repo_name;
    Unixext.unlink_safe (Filename.concat !Xapi_globs.yum_repos_config_dir
                           !Xapi_globs.pool_repo_name);
    Helpers.rmtree !Xapi_globs.local_pool_repo_dir
  with e ->
    error "Failed to cleanup pool repository: %s" (ExnHelper.string_of_exn e);
    raise Api_errors.(Server_error (repository_cleanup_failed, []))
