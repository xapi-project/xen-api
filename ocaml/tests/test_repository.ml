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

module T = Test_common

let test_introduce_duplicate_name () =
  let __context = T.make_test_database () in
  let name_label = "name" in
  let name_description = "description" in
  let name_description_1 = "description1" in
  let binary_url = "https://repo.example.com" in
  let binary_url_1 = "https://repo1.example.com" in
  let source_url = "https://repo-src.example.com" in
  let source_url_1 = "https://repo-src1.example.com" in
  let gpgkey_path = "" in
  let origin = `remote in
  let ref =
    Repository.introduce ~__context ~name_label ~name_description ~binary_url
      ~source_url ~update:true ~gpgkey_path ~origin
  in
  Alcotest.check_raises "test_introduce_duplicate_name"
    Api_errors.(Server_error (repository_already_exists, [Ref.string_of ref]))
    (fun () ->
      Repository.introduce ~__context ~name_label
        ~name_description:name_description_1 ~binary_url:binary_url_1
        ~source_url:source_url_1 ~update:true ~gpgkey_path ~origin
      |> ignore
    )

let test_introduce_duplicate_binary_url () =
  let __context = T.make_test_database () in
  let name_label = "name" in
  let name_label_1 = "name1" in
  let name_description = "description" in
  let name_description_1 = "description1" in
  let binary_url = "https://repo.example.com" in
  let source_url = "https://repo-src.example.com" in
  let source_url_1 = "https://repo-src1.example.com" in
  let gpgkey_path = "" in
  let origin = `remote in
  let ref =
    Repository.introduce ~__context ~name_label ~name_description ~binary_url
      ~source_url ~update:true ~gpgkey_path ~origin
  in
  Alcotest.check_raises "test_introduce_duplicate_name"
    Api_errors.(Server_error (repository_already_exists, [Ref.string_of ref]))
    (fun () ->
      Repository.introduce ~__context ~binary_url ~name_label:name_label_1
        ~name_description:name_description_1 ~source_url:source_url_1
        ~update:false ~gpgkey_path ~origin
      |> ignore
    )

let test_introduce_invalid_gpgkey_path () =
  let __context = T.make_test_database () in
  let name_label = "name" in
  let name_description = "description" in
  let binary_url = "https://repo.example.com" in
  let source_url = "https://repo-src.example.com" in
  let gpgkey_path_1 = "../some-file" in
  let origin = `remote in
  Alcotest.check_raises "test_introduce_invalid_gpgkey_path_1"
    Api_errors.(Server_error (Api_errors.invalid_gpgkey_path, [gpgkey_path_1]))
    (fun () ->
      Repository.introduce ~__context ~binary_url ~name_label ~name_description
        ~source_url ~update:false ~gpgkey_path:gpgkey_path_1 ~origin
      |> ignore
    ) ;
  let gpgkey_path_2 = "some.file" in
  Alcotest.check_raises "test_introduce_invalid_gpgkey_path_2"
    Api_errors.(Server_error (Api_errors.invalid_gpgkey_path, [gpgkey_path_2]))
    (fun () ->
      Repository.introduce ~__context ~binary_url ~name_label ~name_description
        ~source_url ~update:false ~gpgkey_path:gpgkey_path_2 ~origin
      |> ignore
    )

let test_introduce_duplicate_bundle_repo () =
  let __context = T.make_test_database () in
  let name_label = "name" in
  let name_label_1 = "name1" in
  let name_description = "description" in
  let name_description_1 = "description1" in
  let gpgkey_path = "" in
  let origin = `bundle in
  let ref =
    Repository.introduce ~__context ~name_label ~name_description ~binary_url:""
      ~source_url:"" ~update:true ~gpgkey_path ~origin
  in
  Alcotest.check_raises "test_introduce_duplicate_bundle_repo"
    Api_errors.(Server_error (repository_already_exists, [Ref.string_of ref]))
    (fun () ->
      Repository.introduce ~__context ~name_label:name_label_1
        ~name_description:name_description_1 ~binary_url:"" ~source_url:""
        ~update:true ~gpgkey_path ~origin
      |> ignore
    )

let test_introduce_bundle_repo_url_binary_url_not_empty () =
  let __context = T.make_test_database () in
  let name_label = "name" in
  let name_description = "description" in
  let gpgkey_path = "" in
  let origin = `bundle in
  Alcotest.check_raises "test_introduce_bundle_repo_url_binary_url_not_empty"
    Api_errors.(Server_error (bundle_repo_url_should_be_empty, []))
    (fun () ->
      Repository.introduce ~__context ~name_label ~name_description
        ~binary_url:"http://test.url" ~source_url:"" ~update:true ~gpgkey_path
        ~origin
      |> ignore
    )

let test_introduce_bundle_repo_url_source_url_not_empty () =
  let __context = T.make_test_database () in
  let name_label = "name" in
  let name_description = "description" in
  let gpgkey_path = "" in
  let origin = `bundle in
  Alcotest.check_raises "test_introduce_bundle_repo_url_source_url_not_empty"
    Api_errors.(Server_error (bundle_repo_url_should_be_empty, []))
    (fun () ->
      Repository.introduce ~__context ~name_label ~name_description
        ~binary_url:"" ~source_url:"http://test.url" ~update:true ~gpgkey_path
        ~origin
      |> ignore
    )

let test_introduce_bundle_repo_update_is_false () =
  let __context = T.make_test_database () in
  let name_label = "name" in
  let name_description = "description" in
  let gpgkey_path = "" in
  let origin = `bundle in
  Alcotest.check_raises "test_introduce_bundle_repo_update_is_false"
    Api_errors.(Server_error (bundle_repo_update_should_be_true, []))
    (fun () ->
      Repository.introduce ~__context ~name_label ~name_description
        ~binary_url:"" ~source_url:"" ~update:false ~gpgkey_path ~origin
      |> ignore
    )

let test =
  [
    ("test_introduce_duplicate_name", `Quick, test_introduce_duplicate_name)
  ; ( "test_introduce_duplicate_binary_url"
    , `Quick
    , test_introduce_duplicate_binary_url
    )
  ; ( "test_introduce_invalid_gpgkey_path"
    , `Quick
    , test_introduce_invalid_gpgkey_path
    )
  ; ( "test_introduce_duplicate_bundle_repo"
    , `Quick
    , test_introduce_duplicate_bundle_repo
    )
  ; ( "test_introduce_bundle_repo_url_binary_url_not_empty"
    , `Quick
    , test_introduce_bundle_repo_url_binary_url_not_empty
    )
  ; ( "test_introduce_bundle_repo_url_source_url_not_empty"
    , `Quick
    , test_introduce_bundle_repo_url_source_url_not_empty
    )
  ; ( "test_introduce_bundle_repo_update_is_false"
    , `Quick
    , test_introduce_bundle_repo_update_is_false
    )
  ]

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Test Repository suite" [("Test_repository", test)]
