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

open Expiry_alert

let date_of = Xapi_stdext_date.Date.of_iso8601

let test_expired = ("TEST_EXPIRED", 1L)

let test_expiring_07 = ("TEST_EXPIRING_07", 2L)

let test_expiring_14 = ("TEST_EXPIRING_14", 3L)

let test_expiring_30 = ("TEST_EXPIRING_30", 4L)

let alert_conditions =
  [
    (7, test_expiring_07)
  ; (0, test_expired)
  ; (30, test_expiring_30)
  ; (14, test_expiring_14)
  ]

module TestGenerateAlert = struct
  type test_case = {
      description: string
    ; check_time: string
    ; expire_time: string
    ; alert_conditions: (remaining_days * message_id) list
    ; obj_description: string
    ; expected:
        (string -> Xapi_stdext_date.Date.t -> (string * (string * int64)) option)
        option
  }

  let expired_result expired_message_id obj_description expiry =
    let expired = expired_message obj_description in
    Some (message_body expired expiry, expired_message_id)

  let expiring_result expiring_message_id obj_description expiry =
    let expiring = expiring_message obj_description in
    Some (message_body expiring expiry, expiring_message_id)

  let test_cases =
    [
      {
        description= "no alert, 31 days left"
      ; check_time= "20230612T17:00:00Z"
      ; expire_time= "20230713T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= None
      }
    ; {
        description= "no alert, 30 days and 1 second left"
      ; check_time= "20230612T17:00:00Z"
      ; expire_time= "20230712T17:00:01Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= None
      }
    ; {
        description= "no alert, 30 days left"
      ; check_time= "20230612T17:00:00Z"
      ; expire_time= "20230712T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= None
      }
    ; {
        description= "expiring alert, 1 second less than 30 days left"
      ; check_time= "20230612T17:00:01Z"
      ; expire_time= "20230712T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expiring_result test_expiring_30)
      }
    ; {
        description= "expiring alert, 14 days left"
      ; check_time= "20230612T17:00:00Z"
      ; expire_time= "20230626T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expiring_result test_expiring_30)
      }
    ; {
        description= "expiring alert, 1 second less than 14 days left"
      ; check_time= "20230612T17:00:01Z"
      ; expire_time= "20230626T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expiring_result test_expiring_14)
      }
    ; {
        description= "expiring alert, 7 days left"
      ; check_time= "20230612T17:00:00Z"
      ; expire_time= "20230619T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expiring_result test_expiring_14)
      }
    ; {
        description= "expiring alert, 1 second less than 7 days left"
      ; check_time= "20230612T17:00:01Z"
      ; expire_time= "20230619T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expiring_result test_expiring_07)
      }
    ; {
        description= "expiring alert, 1 second left"
      ; check_time= "20230612T17:00:00Z"
      ; expire_time= "20230612T17:00:01Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expiring_result test_expiring_07)
      }
    ; {
        description= "expiring alert, 0 second left"
      ; check_time= "20230612T17:00:00Z"
      ; expire_time= "20230612T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expiring_result test_expiring_07)
      }
    ; {
        description= "expired alert, 1 second passed"
      ; check_time= "20230612T17:00:01Z"
      ; expire_time= "20230612T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expired_result test_expired)
      }
    ; {
        description= "expired alert, 1 day passed"
      ; check_time= "20230612T17:00:00Z"
      ; expire_time= "20230611T17:00:00Z"
      ; alert_conditions
      ; obj_description= "TLS server certificate"
      ; expected= Some (expired_result test_expired)
      }
    ]

  let verify description expected actual =
    Alcotest.(check @@ option @@ pair string @@ pair string int64)
      description expected actual

  let testing
      {
        description
      ; check_time
      ; expire_time
      ; alert_conditions
      ; obj_description
      ; expected
      } () =
    let now = date_of check_time in
    let expiry = date_of expire_time in
    let actual =
      maybe_generate_alert now obj_description alert_conditions expiry
    in
    let expected_res =
      match expected with
      | None ->
          None
      | Some func ->
          func obj_description expiry
    in
    verify description expected_res actual

  let test_from_test_case ({description; _} as test_case) =
    (description, `Quick, testing test_case)

  let tests = List.map test_from_test_case test_cases
end

module TestUpdateMessageInternal = struct
  type message_ref = string

  type test_case = {
      description: string
    ; alert_conditions: (int * (string * int64)) list
    ; msg_obj_uuid: string
    ; alert: string * (string * int64)
    ; all_msgs_properties:
        (message_ref * (string * string * int64 * string)) list
    ; expected_outdated_msg_refs: message_ref list
    ; expected_current_msg_refs: message_ref list
  }

  let test_cases =
    [
      {
        description= "no existing messages"
      ; alert_conditions
      ; msg_obj_uuid= "uuid1"
      ; alert= ("msg_body", (fst test_expiring_30, 3L))
      ; all_msgs_properties= []
      ; expected_outdated_msg_refs= []
      ; expected_current_msg_refs= []
      }
    ; {
        description=
          "no related messages: both message_name and message_obj_uuid do not \
           match"
      ; alert_conditions
      ; msg_obj_uuid= "msg_obj_uuid"
      ; alert= ("msg_body", test_expiring_30)
      ; all_msgs_properties=
          [
            ( "msg_ref1"
            , ("other_msg_name1", "other_msg_body1", 3L, "other_msg_obj_uuid1")
            )
          ; ( "msg_ref2"
            , ("other_msg_name2", "other_msg_body2", 3L, "other_msg_obj_uuid2")
            )
          ]
      ; expected_outdated_msg_refs= []
      ; expected_current_msg_refs= []
      }
    ; {
        description=
          "no related messages: message_name match, message_obj_uuid different"
      ; alert_conditions
      ; msg_obj_uuid= "uuid1"
      ; alert= ("msg_body", test_expiring_30)
      ; all_msgs_properties=
          [
            ( "msg_ref1"
            , ("other_msg_name1", "other_msg_body1", 3L, "other_msg_obj_uuid1")
            )
          ; ( "msg_ref2"
            , ( fst test_expired
              , "other_msg_body2"
              , snd test_expired
              , "other_msg_obj_uuid2"
              )
            )
          ]
      ; expected_outdated_msg_refs= []
      ; expected_current_msg_refs= []
      }
    ; {
        description=
          "no related messages: message_name do not match, message_obj_uuid \
           equal"
      ; alert_conditions
      ; msg_obj_uuid= "msg_obj_uuid"
      ; alert= ("msg_body", test_expiring_30)
      ; all_msgs_properties=
          [
            ( "msg_ref1"
            , ("other_msg_name1", "other_msg_body1", 3L, "msg_obj_uuid")
            )
          ; ( "msg_ref2"
            , ("other_msg_name2", "other_msg_body2", 3L, "msg_obj_uuid")
            )
          ]
      ; expected_outdated_msg_refs= []
      ; expected_current_msg_refs= []
      }
    ; {
        description= "have outdated message"
      ; alert_conditions
      ; msg_obj_uuid= "msg_obj_uuid"
      ; alert= ("msg_body", test_expiring_14)
      ; all_msgs_properties=
          [
            ( "msg_ref1"
            , ("other_msg_name1", "other_msg_body1", 3L, "msg_obj_uuid")
            )
          ; ( "msg_ref2"
            , ( fst test_expiring_30
              , "other_msg_body2"
              , snd test_expiring_30
              , "msg_obj_uuid"
              )
            )
          ]
      ; expected_outdated_msg_refs= ["msg_ref2"]
      ; expected_current_msg_refs= []
      }
    ; {
        description= "already have the required message"
      ; alert_conditions
      ; msg_obj_uuid= "msg_obj_uuid"
      ; alert= ("msg_body", test_expiring_14)
      ; all_msgs_properties=
          [
            ( "msg_ref1"
            , ("other_msg_name1", "other_msg_body1", 3L, "msg_obj_uuid")
            )
          ; ( "msg_ref2"
            , ( fst test_expiring_14
              , "msg_body"
              , snd test_expiring_14
              , "msg_obj_uuid"
              )
            )
          ]
      ; expected_outdated_msg_refs= []
      ; expected_current_msg_refs= ["msg_ref2"]
      }
    ]

  let eq_api_message (a_ref, _a_record) (b_ref, _b_record) = a_ref = b_ref

  let string_of_message (ref, record) =
    Printf.sprintf
      "Message Ref: %s, message_name: %s, message_priority: %Ld, message_body: \
       %s, message_obj_uuid: %s"
      ref record.API.message_name record.API.message_priority
      record.API.message_body record.API.message_obj_uuid

  let pp_api_message = Fmt.of_to_string string_of_message

  let api_message = Alcotest.testable pp_api_message eq_api_message

  let verify description expected actual =
    Alcotest.(check @@ pair (list api_message) (list api_message))
      description expected actual

  let testing
      {
        description
      ; alert_conditions
      ; msg_obj_uuid
      ; alert
      ; all_msgs_properties
      ; expected_outdated_msg_refs
      ; expected_current_msg_refs
      } () =
    let all_msgs =
      List.map
        (fun (ref, (name, body, prio, obj_uuid)) ->
          ( ref
          , {
              API.message_name= name
            ; API.message_priority= prio
            ; API.message_obj_uuid= obj_uuid
            ; API.message_body= body
            ; API.message_uuid= Uuidx.to_string (Uuidx.make ()) (*not used*)
            ; API.message_cls= `Host (*not used*)
            ; API.message_timestamp= Xapi_stdext_date.Date.epoch (*not used*)
            }
          )
        )
        all_msgs_properties
    in
    let msg_name_list =
      List.map (fun (_, (msg_name, _)) -> msg_name) alert_conditions
    in
    let actual = filter_messages msg_name_list msg_obj_uuid alert all_msgs in
    let refs_to_msgs refs =
      List.map
        (fun ref ->
          ( ref
          , {
              API.message_name= "name" (*not used*)
            ; API.message_priority= 1L (*not used*)
            ; API.message_obj_uuid= "obj_uuid" (*not used*)
            ; API.message_body= "body" (*not used*)
            ; API.message_uuid= Uuidx.to_string (Uuidx.make ()) (*not used*)
            ; API.message_cls= `Host (*not used*)
            ; API.message_timestamp= Xapi_stdext_date.Date.epoch (*not used*)
            }
          )
        )
        refs
    in
    let expected_outdated_msgs = refs_to_msgs expected_outdated_msg_refs in
    let expected_current_msgs = refs_to_msgs expected_current_msg_refs in
    verify description (expected_outdated_msgs, expected_current_msgs) actual

  let test_from_test_case ({description; _} as test_case) =
    (description, `Quick, testing test_case)

  let tests = List.map test_from_test_case test_cases
end

let test = TestGenerateAlert.tests @ TestUpdateMessageInternal.tests
