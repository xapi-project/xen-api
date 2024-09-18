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

open Daily_license_check

let pp_expiry fmt = function
  | Good ->
      Fmt.pf fmt "Good"
  | Expiring hosts ->
      Fmt.pf fmt "Expiring soon on %a" Fmt.(Dump.list @@ string) hosts
  | Expired hosts ->
      Fmt.pf fmt "Expired on %a" Fmt.(Dump.list @@ string) hosts

(** Testable type for Alcotest *)
let expiry =
  let equals x y =
    match (x, y) with
    | Good, Good ->
        true
    | (Expiring hosts_x, Expiring hosts_y | Expired hosts_x, Expired hosts_y)
      when hosts_x = hosts_y ->
        true
    | _ ->
        false
  in
  Alcotest.testable pp_expiry equals

let check_time =
  Xapi_stdext_date.Date.(to_unix_time (of_iso8601 "20160601T04:00:00Z"))

let test_expiry ((pool_license_state, all_license_params), expected) () =
  let result = check_license check_time pool_license_state all_license_params in
  Alcotest.check expiry "pool_expiry" expected result

let expiry_samples =
  [
    (([("expiry", "20170101T00:00:00Z")], []), Good)
  ; (([("expiry", "20160701T04:01:00Z")], []), Good)
  ; (([("expiry", "20160701T04:00:00Z")], []), Expiring [])
  ; (([("expiry", "20160616T00:00:00Z")], []), Expiring [])
  ; (([("expiry", "20160601T04:00:01Z")], []), Expiring [])
  ; (([("expiry", "20160601T04:00:00Z")], []), Expired [])
  ; (([("expiry", "20160101T00:00:00Z")], []), Expired [])
  ; ( ( [("expiry", "20160615T00:00:00Z")]
      , [
          ("host0", [("expiry", "20160615T00:00:00Z")])
        ; ("host1", [("expiry", "20160615T00:00:00Z")])
        ]
      )
    , Expiring ["host1"; "host0"]
    )
  ; ( ( [("expiry", "20160615T00:00:00Z")]
      , [
          ("host0", [("expiry", "20160615T00:00:00Z")])
        ; ("host1", [("expiry", "20160715T00:00:00Z")])
        ]
      )
    , Expiring ["host0"]
    )
  ; ( ( [("expiry", "20160101T00:00:00Z")]
      , [
          ("host0", [("expiry", "20160601T00:00:00Z")])
        ; ("host1", [("expiry", "20150601T00:00:00Z")])
        ]
      )
    , Expired ["host1"; "host0"]
    )
  ; ( ( [("expiry", "20160101T00:00:00Z")]
      , [
          ("host0", [("expiry", "20170601T00:00:00Z")])
        ; ("host1", [("expiry", "20150601T00:00:00Z")])
        ]
      )
    , Expired ["host1"]
    )
  ]

let test =
  List.mapi
    (fun i spec ->
      ( Printf.sprintf "Test licence checks as run daily #%d" i
      , `Quick
      , test_expiry spec
      )
    )
    expiry_samples
