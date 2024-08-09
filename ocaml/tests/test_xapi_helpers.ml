(*
 * Copyright (C) Cloud Software Group, Inc
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

let strings =
  [
    ("foobar", "foobar")
  ; ("foobarproxy_username=password", "foobarproxy_username=(filtered)")
  ; ("barfooproxy_password=secret", "barfooproxy_password=(filtered)")
  ; ("password", "password")
  ; ("username=password", "username=password")
  ; ("password=password", "password=password")
  ; ("proxy_username=", "proxy_username=(filtered)")
  ]

let filtering_test =
  List.map
    (fun (input, expected) ->
      let test_filtering () =
        let filtered =
          match Helpers.filter_args [input] with x :: _ -> x | _ -> ""
        in
        Printf.printf "%s\n" input ;
        Alcotest.(check string) "secrets must be filtered out" expected filtered
      in
      ( Printf.sprintf {|Validation of argument filtering of "%s"|} input
      , `Quick
      , test_filtering
      )
    )
    strings

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Test XAPI Helpers suite" [("Test_xapi_helpers", filtering_test)]
