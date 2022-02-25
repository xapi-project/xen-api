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
  let binary_url = "https://binary-url.com" in
  let binary_url_1 = "https://binary-url1.com" in
  let source_url = "https://source-url.com" in
  let source_url_1 = "https://source-url1.com" in
  let gpgkey_path = "" in
  let ref =
    Repository.introduce ~__context ~name_label ~name_description ~binary_url
      ~source_url ~update:true ~gpgkey_path
  in
  Alcotest.check_raises "test_introduce_duplicate_name"
    Api_errors.(Server_error (repository_already_exists, [Ref.string_of ref]))
    (fun () ->
      Repository.introduce ~__context ~name_label
        ~name_description:name_description_1 ~binary_url:binary_url_1
        ~source_url:source_url_1 ~update:true ~gpgkey_path
      |> ignore
    )

let test_introduce_duplicate_binary_url () =
  let __context = T.make_test_database () in
  let name_label = "name" in
  let name_label_1 = "name1" in
  let name_description = "description" in
  let name_description_1 = "description1" in
  let binary_url = "https://binary-url.com" in
  let source_url = "https://source-url.com" in
  let source_url_1 = "https://source-url1.com" in
  let gpgkey_path = "" in
  let ref =
    Repository.introduce ~__context ~name_label ~name_description ~binary_url
      ~source_url ~update:true ~gpgkey_path
  in
  Alcotest.check_raises "test_introduce_duplicate_name"
    Api_errors.(Server_error (repository_already_exists, [Ref.string_of ref]))
    (fun () ->
      Repository.introduce ~__context ~binary_url ~name_label:name_label_1
        ~name_description:name_description_1 ~source_url:source_url_1
        ~update:false ~gpgkey_path
      |> ignore
    )

let test =
  [
    ("test_introduce_duplicate_name", `Quick, test_introduce_duplicate_name)
  ; ( "test_introduce_duplicate_binary_url"
    , `Quick
    , test_introduce_duplicate_binary_url
    )
  ]

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Test Repository suite" [("Test_repository", test)]
