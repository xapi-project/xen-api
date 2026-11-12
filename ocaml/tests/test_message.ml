(*
 * Copyright (C) Cloud Software Group, Inc.
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

module Date = Clock.Date

let ok_ref = "OpaqueRef:9d2c1f80-3e4b-4a6d-8b12-0f7a5c3e9d44"

let ok_uuid = "7f3e2a10-88c4-4e5b-a9d1-2c6b0f4e7a35"

let ok_obj_uuid = "b4a0f7e2-0c5d-4a11-9f3c-6d2e8a17c9b1"

let xml_decl = {|<?xml version="1.0" encoding="UTF-8"?>|}

let message_xml ?(_ref = ok_ref) ?(uuid = ok_uuid) ?(obj_uuid = ok_obj_uuid)
    ?(cls = "VM") () =
  String.concat ""
    [
      "<message>"
    ; Printf.sprintf "<ref>%s</ref>" _ref
    ; "<name>VM_SHUTDOWN</name>"
    ; "<priority>5</priority>"
    ; Printf.sprintf "<cls>%s</cls>" cls
    ; Printf.sprintf "<obj_uuid>%s</obj_uuid>" obj_uuid
    ; "<timestamp>20260818T09:14:22Z</timestamp>"
    ; Printf.sprintf "<uuid>%s</uuid>" uuid
    ; "<body>Guest requested shutdown</body>"
    ; "</message>"
    ]

let test_message =
  {
    API.message_name= "VM_SHUTDOWN"
  ; API.message_uuid= ok_uuid
  ; API.message_priority= 5L
  ; API.message_cls= `VM
  ; API.message_obj_uuid= ok_obj_uuid
  ; API.message_timestamp= Date.of_iso8601 "20260818T09:14:22Z"
  ; API.message_body= "Guest requested shutdown"
  }

let of_xml_string str = Xapi_message.of_xml (Xmlm.make_input (`String (0, str)))

let import_string str =
  Xapi_message.import_xml (Xmlm.make_input (`String (0, str)))

let check_rejected what str =
  match of_xml_string str with
  | exception Failure _ ->
      ()
  | exception e ->
      Alcotest.failf "%s: expected Failure, got %s" what (Printexc.to_string e)
  | _ ->
      Alcotest.failf "%s: expected a rejection, but the message parsed" what

let check_accepted what str =
  match of_xml_string str with
  | _gen, _ref, message ->
      (_ref, message)
  | exception e ->
      Alcotest.failf "%s: expected success, got %s" what (Printexc.to_string e)

let test_accepts_well_formed () =
  let _ref, message = check_accepted "well-formed" (message_xml ()) in
  Alcotest.(check string) "ref" ok_ref (Ref.string_of _ref) ;
  Alcotest.(check string) "uuid" ok_uuid message.API.message_uuid ;
  Alcotest.(check string) "obj_uuid" ok_obj_uuid message.API.message_obj_uuid ;
  Alcotest.(check string) "name" "VM_SHUTDOWN" message.API.message_name

let test_expect_failure_bad_ref () =
  List.iter
    (fun _ref -> check_rejected ("ref " ^ _ref) (message_xml ~_ref ()))
    [
      "OpaqueRef:../../../../etc/cron.d/x"
    ; "../../../../etc/cron.d/x"
    ; "OpaqueRef:9d2c1f80-3e4b-4a6d-8b12-0f7a5c3e9d44/../../x"
    ; "OpaqueRef:NULL"
    ; "OpaqueRef:not-a-uuid"
    ; "OpaqueRef:"
    ; "DummyRef:|" ^ ok_uuid ^ "|a-name"
    ; ok_uuid
    ]

let test_expect_failure_bad_uuid () =
  List.iter
    (fun uuid -> check_rejected ("uuid " ^ uuid) (message_xml ~uuid ()))
    [
      "../../../root/.ssh/authorized_keys"
    ; ok_uuid ^ "/../../x"
    ; "not-a-uuid"
    ; ok_ref
    ]

let test_expect_failure_bad_obj_uuid () =
  List.iter
    (fun obj_uuid ->
      check_rejected ("obj_uuid " ^ obj_uuid) (message_xml ~obj_uuid ())
    )
    ["../../../tmp/x"; ok_obj_uuid ^ "/../../x"; "not-a-uuid"]

let test_expect_failure_missing_identifiers () =
  List.iter
    (fun (what, xml) -> check_rejected what xml)
    [
      ( "empty ref"
      , Printf.sprintf
          "<message><ref></ref><uuid>%s</uuid><obj_uuid>%s</obj_uuid></message>"
          ok_uuid ok_obj_uuid
      )
    ; ( "self-closing uuid"
      , Printf.sprintf
          "<message><ref>%s</ref><uuid/><obj_uuid>%s</obj_uuid></message>"
          ok_ref ok_obj_uuid
      )
    ; ( "absent obj_uuid"
      , Printf.sprintf "<message><ref>%s</ref><uuid>%s</uuid></message>" ok_ref
          ok_uuid
      )
    ; ("no identifiers at all", "<message/>")
    ]

(* Whatever to_xml writes must still be accepted by of_xml, or cross-pool
   migration would stop replicating messages. *)
let test_export_import_round_trip () =
  let body = Xapi_message.export_xml [(Ref.of_string ok_ref, test_message)] in
  match import_string body with
  | [(_gen, _ref, message)] ->
      Alcotest.(check string) "ref" ok_ref (Ref.string_of _ref) ;
      Alcotest.(check string) "uuid" ok_uuid message.API.message_uuid ;
      Alcotest.(check string)
        "obj_uuid" ok_obj_uuid message.API.message_obj_uuid ;
      Alcotest.(check string) "name" "VM_SHUTDOWN" message.API.message_name ;
      Alcotest.(check string)
        "body" "Guest requested shutdown" message.API.message_body
  | messages ->
      Alcotest.failf "expected 1 message, got %d" (List.length messages)

(* import_xml parses the whole batch before the handler writes any of it, so one
   bad message aborts the import instead of leaving its neighbours written. *)
let test_import_expect_failure_whole_batch () =
  let good = xml_decl ^ message_xml () in
  let bad = xml_decl ^ message_xml ~_ref:"OpaqueRef:../../../etc/x" () in
  List.iter
    (fun (what, body) ->
      match import_string body with
      | exception Failure _ ->
          ()
      | exception e ->
          Alcotest.failf "%s: expected Failure, got %s" what
            (Printexc.to_string e)
      | _ ->
          Alcotest.failf "%s: expected the batch to be rejected" what
    )
    [("bad message first", bad ^ good); ("bad message last", good ^ bad)] ;
  Alcotest.(check int)
    "a batch of good messages still imports" 2
    (List.length (import_string (good ^ good)))

(* The query params name the object the request was admitted for, and the
   handler has already resolved them in the database. A message that claims a
   different class or object must not be written under it. *)
let test_assert_query_matches_body () =
  let expect_no_failure what ~cls ~uuid message =
    match Xapi_message.assert_query_matches_body ~cls ~uuid message with
    | () ->
        ()
    | exception e ->
        Alcotest.failf "%s: unexpected %s" what (Printexc.to_string e)
  in
  let expect_failure what ~cls ~uuid message =
    match Xapi_message.assert_query_matches_body ~cls ~uuid message with
    | () ->
        Alcotest.failf "%s: expected a rejection" what
    | exception Failure _ ->
        ()
    | exception e ->
        Alcotest.failf "%s: expected Failure, got %s" what (Printexc.to_string e)
  in
  expect_no_failure "matching cls and obj_uuid" ~cls:`VM ~uuid:ok_obj_uuid
    test_message ;
  expect_failure "body claims another class" ~cls:`Host ~uuid:ok_obj_uuid
    test_message ;
  expect_failure "body claims another object" ~cls:`VM
    ~uuid:"11111111-2222-3333-4444-555555555555" test_message

let test =
  [
    ("test_accepts_well_formed", `Quick, test_accepts_well_formed)
  ; ("test_expect_failure_bad_ref", `Quick, test_expect_failure_bad_ref)
  ; ("test_expect_failure_bad_uuid", `Quick, test_expect_failure_bad_uuid)
  ; ( "test_expect_failure_bad_obj_uuid"
    , `Quick
    , test_expect_failure_bad_obj_uuid
    )
  ; ( "test_expect_failure_missing_identifiers"
    , `Quick
    , test_expect_failure_missing_identifiers
    )
  ; ("test_export_import_round_trip", `Quick, test_export_import_round_trip)
  ; ( "test_import_expect_failure_whole_batch"
    , `Quick
    , test_import_expect_failure_whole_batch
    )
  ; ("test_assert_query_matches_body", `Quick, test_assert_query_matches_body)
  ]
