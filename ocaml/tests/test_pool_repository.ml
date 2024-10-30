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

let on_repositories f =
  let __context = T.make_test_database () in
  let pool = Helpers.get_pool ~__context in
  let binary_url_1 = "https://repo.example.com" in
  let binary_url_2 = "https://1.1.1.1/repository/enabled" in
  let source_url = "https://repo-src.example.com" in
  let ref_remote =
    Repository.introduce ~__context ~name_label:"remote"
      ~name_description:"remote" ~binary_url:binary_url_1 ~source_url
      ~update:true ~gpgkey_path:""
  in
  let ref_bundle =
    Repository.introduce_bundle ~__context ~name_label:"bundle"
      ~name_description:"bundle"
  in
  let ref_remote_pool =
    Repository.introduce_remote_pool ~__context ~name_label:"remote_pool"
      ~binary_url:binary_url_2 ~name_description:"remote_pool" ~certificate:""
  in
  f __context pool ref_remote ref_bundle ref_remote_pool

let test_set_repositories () =
  on_repositories (fun __context self ref_remote ref_bundle ref_remote_pool ->
      Xapi_pool.set_repositories ~__context ~self ~value:[ref_remote] ;
      Xapi_pool.set_repositories ~__context ~self ~value:[ref_bundle] ;
      Xapi_pool.set_repositories ~__context ~self ~value:[ref_remote_pool] ;
      Alcotest.check_raises "test_set_repositories_1"
        Api_errors.(
          Server_error
            ( repo_should_be_single_one_enabled
            , [Record_util.origin_to_string `bundle]
            )
        )
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self
            ~value:[ref_remote; ref_bundle]
        ) ;
      Alcotest.check_raises "test_set_repositories_2"
        Api_errors.(
          Server_error
            ( repo_should_be_single_one_enabled
            , [`bundle; `remote_pool] |> List.map Record_util.origin_to_string
            )
        )
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self
            ~value:[ref_remote_pool; ref_bundle]
        ) ;
      Alcotest.check_raises "test_set_repositories_3"
        Api_errors.(
          Server_error
            ( repo_should_be_single_one_enabled
            , [Record_util.origin_to_string `remote_pool]
            )
        )
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self
            ~value:[ref_remote; ref_remote_pool]
        )
  )

let test_add_repository () =
  on_repositories (fun __context self ref_remote ref_bundle ref_remote_pool ->
      Alcotest.check_raises "test_add_repository_1"
        Api_errors.(
          Server_error
            ( repo_should_be_single_one_enabled
            , [Record_util.origin_to_string `bundle]
            )
        )
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self ~value:[ref_remote] ;
          Xapi_pool.add_repository ~__context ~self ~value:ref_bundle
        ) ;
      Alcotest.check_raises "test_add_repository_2"
        Api_errors.(
          Server_error
            ( repo_should_be_single_one_enabled
            , [Record_util.origin_to_string `remote_pool]
            )
        )
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self ~value:[ref_remote] ;
          Xapi_pool.add_repository ~__context ~self ~value:ref_remote_pool
        ) ;
      Alcotest.check_raises "test_add_repository_3"
        Api_errors.(
          Server_error
            ( repo_should_be_single_one_enabled
            , [`remote_pool; `bundle] |> List.map Record_util.origin_to_string
            )
        )
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self ~value:[ref_remote_pool] ;
          Xapi_pool.add_repository ~__context ~self ~value:ref_bundle
        ) ;
      Alcotest.check_raises "test_add_repository_4"
        Api_errors.(
          Server_error
            ( repo_should_be_single_one_enabled
            , [`bundle; `remote_pool] |> List.map Record_util.origin_to_string
            )
        )
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self ~value:[ref_bundle] ;
          Xapi_pool.add_repository ~__context ~self ~value:ref_remote_pool
        )
  )

let test_enable_periodic_repo_sync () =
  on_repositories (fun __context self ref_remote ref_bundle ref_remote_pool ->
      Xapi_pool.set_repositories ~__context ~self ~value:[ref_remote] ;
      Xapi_pool.set_update_sync_enabled ~__context ~self ~value:true ;
      Alcotest.check_raises "test_enable_periodic_repo_sync_1"
        Api_errors.(Server_error (can_not_periodic_sync_updates, []))
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self ~value:[ref_bundle] ;
          Xapi_pool.set_update_sync_enabled ~__context ~self ~value:true
        ) ;
      Alcotest.check_raises "test_enable_periodic_repo_sync_2"
        Api_errors.(Server_error (can_not_periodic_sync_updates, []))
        (fun () ->
          Xapi_pool.set_repositories ~__context ~self ~value:[ref_remote_pool] ;
          Xapi_pool.set_update_sync_enabled ~__context ~self ~value:true
        )
  )

let test =
  [
    ("test_set_repositories", `Quick, test_set_repositories)
  ; ("test_add_repository", `Quick, test_add_repository)
  ; ("test_enable_periodic_repo_sync", `Quick, test_enable_periodic_repo_sync)
  ]

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Test Pool Repository suite" [("Test_pool_repository", test)]
