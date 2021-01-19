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

let cmd_yum = "/usr/bin/yum"
let cmd_rm = "/usr/bin/rm"

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

let assert_name_is_valid ~name_label =
  match Re.Str.string_match (Re.Str.regexp "^[A-Za-z]+[A-Za-z0-9\\-]+$") name_label 0 with
  | true -> ()
  | false -> raise Api_errors.(Server_error (invalid_repository_name, [name_label]))

let assert_url_is_valid ~url =
  match !Xapi_globs.repository_domain_name_allowlist with
  | [] -> ()
  | l ->
    let result = l |> List.exists (fun domain_name ->
        let r = Re.Str.regexp
            (Printf.sprintf
               "^https?://[A-Za-z0-9\\-]+\\.%s\\(:[0-9]+\\)?/[A-Za-z0-9\\-/\\$]+$"
               domain_name)
        in
        Re.Str.string_match r url 0)
    in
    begin match result with
      | true -> ()
      | false -> raise Api_errors.(Server_error (invalid_base_url, [url]))
    end

let introduce ~__context ~name_label ~name_description ~binary_url ~source_url =
  assert_name_is_valid ~name_label;
  assert_url_is_valid ~url:binary_url;
  assert_url_is_valid ~url:source_url;
  match Db.Repository.get_by_name_label ~__context ~label:name_label with
  | [] ->
    create_repository_record  ~__context ~name_label ~name_description ~binary_url ~source_url
  | _ ->
    error "A repository with same name_label '%s' already exists" name_label;
    raise Api_errors.(Server_error (repository_already_exists, []))

let forget ~__context ~self =
  let pool = Helpers.get_pool ~__context in
  let enabled = Db.Pool.get_repository ~__context ~self:pool in
  if enabled = self then
    raise Api_errors.(Server_error (repository_is_in_use, []))
  else
    Db.Repository.destroy ~__context ~self

let reposync_try_lock () =
  Mutex.try_lock reposync_mutex

let reposync_unlock () =
  Mutex.unlock reposync_mutex

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

let clean_yum_cache ~prefix ~name =
  try
    let params =
        [
          "--disablerepo=*";
          Printf.sprintf "--enablerepo=%s-%s" prefix name;
          "clean";
          "expire-cache";
        ]
    in
    ignore (Helpers.call_script cmd_yum params)
  with _ -> warn "Unable to clean YUM cache of %s-%s" prefix name

let cleanup ~__context ~self ~prefix =
  if self <> Ref.null then
    let name = Db.Repository.get_name_label ~__context ~self in
    try
      clean_yum_cache ~prefix ~name;
      ignore (Helpers.call_script
                cmd_rm
                ["-f"; (Printf.sprintf "/etc/yum.repos.d/%s-%s.repo" prefix name)]);
      ignore (Helpers.call_script cmd_rm ["-rf"; !Xapi_globs.local_pool_repo_dir])
    with e ->
      error "Failed to cleanup local pool repository: %s" (ExnHelper.string_of_exn e);
      raise Api_errors.(Server_error (local_pool_repo_cleanup_failed, []))
