(*
 * Copyright (C) 2024 Cloud Software Group
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

module Certcheck = Certcheck

let all_statuses =
  Certcheck.[Ok; Expired; NotYetValid; ParseError; NoCerts; InternalError]

(* A 64-byte buffer with incrementing byte values – long enough to exercise
   the parser but syntactically invalid as a UEFI variable store. *)
let garbage = Bytes.init 64 (fun i -> Char.chr (i land 0xFF))

let is_error s = s <> Certcheck.Ok

(* ---- status_string ---------------------------------------------------- *)

let status_string_non_empty () =
  List.iter
    (fun s ->
      let str = Certcheck.status_string s in
      Alcotest.(check bool) (str ^ " is non-empty") true (String.length str > 0)
    )
    all_statuses

let status_string_distinct () =
  let strings = List.map Certcheck.status_string all_statuses in
  let unique = List.sort_uniq String.compare strings in
  Alcotest.(check int)
    "all status strings are distinct"
    (List.length all_statuses)
    (List.length unique)

let status_string_tests =
  [
    ("non-empty", `Quick, status_string_non_empty)
  ; ("distinct", `Quick, status_string_distinct)
  ]

(* ---- check_expiration ------------------------------------------------- *)

let check_expiration_empty () =
  let result = Certcheck.check_expiration Bytes.empty in
  Alcotest.(check bool) "empty bytes returns error" true (is_error result)

let check_expiration_garbage () =
  let result = Certcheck.check_expiration garbage in
  Alcotest.(check bool) "garbage bytes returns error (no crash)" true
    (is_error result)

let check_expiration_tests =
  [
    ("empty input", `Quick, check_expiration_empty)
  ; ("invalid input", `Quick, check_expiration_garbage)
  ]

(* ---- check_expiration_at ---------------------------------------------- *)

let check_expiration_at_empty () =
  let t = Int64.of_float (Unix.gettimeofday ()) in
  let result = Certcheck.check_expiration_at Bytes.empty t in
  Alcotest.(check bool) "empty bytes returns error" true (is_error result)

let check_expiration_at_zero_time () =
  (* Unix epoch – must not crash regardless of what the library does. *)
  let result = Certcheck.check_expiration_at garbage 0L in
  Alcotest.(check bool) "time=0 returns error (no crash)" true (is_error result)

let check_expiration_at_far_future () =
  (* Year 2100 in seconds since epoch: well beyond any real cert lifetime. *)
  let year_2100 = Int64.of_string "4102444800" in
  let result = Certcheck.check_expiration_at garbage year_2100 in
  Alcotest.(check bool) "far-future time returns error (no crash)" true
    (is_error result)

let check_expiration_at_consistent () =
  (* Both functions must agree on the same invalid input when given the same
     reference time. *)
  let t = Int64.of_float (Unix.gettimeofday ()) in
  let r1 = Certcheck.check_expiration garbage in
  let r2 = Certcheck.check_expiration_at garbage t in
  Alcotest.(check bool) "agrees with check_expiration" true (r1 = r2)

let check_expiration_at_tests =
  [
    ("empty input", `Quick, check_expiration_at_empty)
  ; ("zero time", `Quick, check_expiration_at_zero_time)
  ; ("far future", `Quick, check_expiration_at_far_future)
  ; ("consistent with check_expiration", `Quick, check_expiration_at_consistent)
  ]

(* ---- get_cert_info ---------------------------------------------------- *)

let get_cert_info_empty () =
  let status, certs = Certcheck.get_cert_info Bytes.empty in
  Alcotest.(check bool) "empty bytes returns error status" true (is_error status) ;
  Alcotest.(check int) "empty bytes returns empty list" 0 (List.length certs)

let get_cert_info_garbage () =
  let status, _certs = Certcheck.get_cert_info garbage in
  Alcotest.(check bool) "garbage bytes returns error (no crash)" true
    (is_error status)

let get_cert_info_consistent () =
  (* The overall status returned by get_cert_info must match check_expiration
     for the same blob: the richer call must not silently succeed where the
     simpler one already reported an error. *)
  let info_status, _certs = Certcheck.get_cert_info garbage in
  let exp_status = Certcheck.check_expiration garbage in
  Alcotest.(check bool) "agrees with check_expiration" true
    (info_status = exp_status)

let get_cert_info_tests =
  [
    ("empty input", `Quick, get_cert_info_empty)
  ; ("invalid input", `Quick, get_cert_info_garbage)
  ; ("consistent with check_expiration", `Quick, get_cert_info_consistent)
  ]

(* ---- cert_info record ------------------------------------------------- *)

let cert_info_record_construction () =
  (* Compiling this test proves that all fields declared in certcheck.mli
     are present and have the expected types. *)
  let _ci : Certcheck.cert_info =
    { subject= "CN=test"
    ; issuer= "CN=issuer"
    ; not_before= 0L
    ; not_after= Int64.max_int
    ; is_expired= false
    ; is_not_yet_valid= false
    ; variable_name= "db"
    }
  in
  ()

let cert_info_tests =
  [("record construction", `Quick, cert_info_record_construction)]

(* ---- runner ----------------------------------------------------------- *)

let () =
  Alcotest.run "Certcheck"
    [
      ("status_string", status_string_tests)
    ; ("check_expiration", check_expiration_tests)
    ; ("check_expiration_at", check_expiration_at_tests)
    ; ("get_cert_info", get_cert_info_tests)
    ; ("cert_info", cert_info_tests)
    ]

