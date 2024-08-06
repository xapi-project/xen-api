(*
 * Copyright (c) Cloud Software Group, Inc.
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

module T = Test_common

let test_set_remote_and_bundle_repos () =
  let __context = T.make_test_database () in
  let name_label = "remote" in
  let name_description = "remote" in
  let binary_url = "https://repo.example.com" in
  let source_url = "https://repo-src.example.com" in
  let gpgkey_path = "" in
  let ref_remote =
    Repository.introduce ~__context ~name_label ~name_description ~binary_url
      ~source_url ~update:true ~gpgkey_path
  in
  let ref_bundle =
    Repository.introduce_bundle ~__context ~name_label:"bundle"
      ~name_description:"bundle"
  in
  let self = Helpers.get_pool ~__context in
  Alcotest.check_raises "test_set_remote_and_bundle_repos"
    Api_errors.(Server_error (bundle_repo_should_be_single_enabled, []))
    (fun () ->
      Xapi_pool.set_repositories ~__context ~self
        ~value:[ref_remote; ref_bundle]
    )

let test_add_bundle_repo () =
  let __context = T.make_test_database () in
  let name_label = "remote" in
  let name_description = "remote" in
  let binary_url = "https://repo.example.com" in
  let source_url = "https://repo-src.example.com" in
  let gpgkey_path = "" in
  let ref_remote =
    Repository.introduce ~__context ~name_label ~name_description ~binary_url
      ~source_url ~update:true ~gpgkey_path
  in
  let ref_bundle =
    Repository.introduce_bundle ~__context ~name_label:"bundle"
      ~name_description:"bundle"
  in
  let self = Helpers.get_pool ~__context in
  Alcotest.check_raises "test_add_bundle_repo"
    Api_errors.(Server_error (bundle_repo_should_be_single_enabled, []))
    (fun () ->
      Xapi_pool.set_repositories ~__context ~self ~value:[ref_remote] ;
      Xapi_pool.add_repository ~__context ~self ~value:ref_bundle
    )

let test_add_remote_repo () =
  let __context = T.make_test_database () in
  let name_label = "remote" in
  let name_description = "remote" in
  let binary_url = "https://repo.example.com" in
  let source_url = "https://repo-src.example.com" in
  let gpgkey_path = "" in
  let ref_remote =
    Repository.introduce ~__context ~name_label ~name_description ~binary_url
      ~source_url ~update:true ~gpgkey_path
  in
  let ref_bundle =
    Repository.introduce_bundle ~__context ~name_label:"bundle"
      ~name_description:"bundle"
  in
  let self = Helpers.get_pool ~__context in
  Alcotest.check_raises "test_add_remote_repo"
    Api_errors.(Server_error (bundle_repo_should_be_single_enabled, []))
    (fun () ->
      Xapi_pool.set_repositories ~__context ~self ~value:[ref_bundle] ;
      Xapi_pool.add_repository ~__context ~self ~value:ref_remote
    )

let test_can_not_enable_bundle_repo_auto_sync () =
  let __context = T.make_test_database () in
  let ref_bundle =
    Repository.introduce_bundle ~__context ~name_label:"bundle"
      ~name_description:"bundle"
  in
  let self = Helpers.get_pool ~__context in
  Alcotest.check_raises "test_can_not_enable_bundle_repo_auto_sync"
    Api_errors.(Server_error (can_not_sync_updates, []))
    (fun () ->
      Xapi_pool.set_repositories ~__context ~self ~value:[ref_bundle] ;
      Xapi_pool.set_update_sync_enabled ~__context ~self ~value:true
    )

let test =
  [
    ( "test_set_remote_and_bundle_repos"
    , `Quick
    , test_set_remote_and_bundle_repos
    )
  ; ("test_add_bundle_repo", `Quick, test_add_bundle_repo)
  ; ("test_add_remote_repo", `Quick, test_add_remote_repo)
  ; ( "test_can_not_enable_bundle_repo_auto_sync"
    , `Quick
    , test_can_not_enable_bundle_repo_auto_sync
    )
  ]

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Test Pool Repository suite" [("Test_pool_repository", test)]
