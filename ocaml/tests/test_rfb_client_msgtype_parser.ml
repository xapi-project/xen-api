(*
 * Copyright (C) Cloud Software Group
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

open Rfb_client_msgtype_parser

(* Helper function to create binary data from byte values *)
let int_list_to_string ints =
  List.to_seq ints |> Seq.map Char.chr |> String.of_seq

(* Helper function to create test data for different message types *)
module TestData = struct
  (* Valid RFB protocol version string: "RFB 003.003\n" *)
  let protocol_version = "RFB 003.003\n"

  (* Invalid protocol versions for negative testing *)
  let invalid_protocol_version = "VNC 003.003\n"

  let invalid_protocol_version_rfb_004 = "RFB 003.004\n"

  let incomplete_protocol_version = "RFB 003.003" (* 11 bytes, incomplete *)

  (* ClientInit messages: 1 byte (shared flag) *)
  let client_init = int_list_to_string [1] (* Shared access *)

  let client_init_exclusive = int_list_to_string [0] (* Exclusive access *)

  let client_init_invalid = int_list_to_string [2] (* Invalid value *)

  let client_init_invalid_255 = int_list_to_string [255]
  (* Another invalid value *)

  (* SetPixelFormat: message-type(0) + padding(3) + pixel-format(16) *)
  let set_pixel_format =
    int_list_to_string ([0; 0; 0; 0] @ List.init 16 (fun _ -> 0))

  (* SetEncodings: message-type(2) + padding(1) + num-encodings(2) + encodings(4*n) *)
  let set_encodings_simple = int_list_to_string [2; 0; 0; 1; 0; 0; 0; 1]
  (* 1 encoding *)

  let set_encodings_multiple =
    int_list_to_string ([2; 0; 0; 2] @ [0; 0; 0; 1; 0; 0; 0; 2])
  (* 2 encodings *)

  (* FramebufferUpdateRequest: message-type(3) + incremental(1) + x(2) + y(2) + width(2) + height(2) *)
  let framebuffer_update_request =
    int_list_to_string [3; 0; 0; 0; 0; 0; 1; 0; 1; 0]

  (* KeyEvent: message-type(4) + down-flag(1) + padding(2) + key(4) *)
  let key_event = int_list_to_string [4; 1; 0; 0; 0; 0; 0; 65]

  (* PointerEvent: message-type(5) + button-mask(1) + x(2) + y(2) *)
  let pointer_event = int_list_to_string [5; 1; 0; 100; 0; 50]

  (* ClientCutText: message-type(6) + padding(3) + length(4) + text *)
  let client_cut_text =
    int_list_to_string
      ([6; 0; 0; 0; 0; 0; 0; 5] @ List.map Char.code ['H'; 'e'; 'l'; 'l'; 'o'])

  (* QEMUClientMessage: message-type(255) + data(11) *)
  let qemu_client_message =
    int_list_to_string ([255] @ List.init 11 (fun _ -> 0))

  (* Invalid/unsupported message types *)
  let unsupported_message = int_list_to_string [99]
  (* unsupported message type *)
end

(* Test individual message type parsing functions *)
module MessageParsingTests = struct
  let test_protocol_version_parsing () =
    let parser = create () in
    let result = parser (TestData.protocol_version ^ TestData.client_init) in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Complete handshake parsed correctly" ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_invalid_protocol_version () =
    let parser = create () in
    let result =
      parser (TestData.invalid_protocol_version ^ TestData.client_init)
    in
    match result with
    | Error msg ->
        Alcotest.(check bool)
          "Invalid protocol version returns error" true
          (String.starts_with ~prefix:"Parse error: BadHandshake:" msg)
    | Ok _ ->
        Alcotest.fail
          "Expected Error but got Ok - invalid handshake should fail"

  let test_invalid_protocol_version_rfb_004 () =
    let parser = create () in
    let result =
      parser (TestData.invalid_protocol_version_rfb_004 ^ TestData.client_init)
    in
    match result with
    | Error msg ->
        Alcotest.(check bool)
          "RFB 003.004 version returns error" true
          (String.starts_with ~prefix:"Parse error: BadHandshake:" msg)
    | Ok _ ->
        Alcotest.fail
          "Expected Error but got Ok - invalid handshake should fail"

  let test_incomplete_protocol_version () =
    let parser = create () in
    let result = parser TestData.incomplete_protocol_version in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Incomplete protocol version returns empty (partial)" []
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_client_init_shared () =
    let parser = create () in
    let result = parser (TestData.protocol_version ^ TestData.client_init) in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Complete handshake with shared ClientInit parsed correctly"
          ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_client_init_exclusive () =
    let parser = create () in
    let result =
      parser (TestData.protocol_version ^ TestData.client_init_exclusive)
    in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Complete handshake with exclusive ClientInit parsed correctly"
          ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_client_init_invalid () =
    let parser = create () in
    let result =
      parser (TestData.protocol_version ^ TestData.client_init_invalid)
    in
    match result with
    | Error msg ->
        Alcotest.(check bool)
          "Invalid ClientInit returns error" true
          (String.starts_with ~prefix:"Parse error: BadHandshake:" msg)
    | Ok _ ->
        Alcotest.fail
          "Expected Error but got Ok - invalid handshake should fail"

  let test_client_init_invalid_255 () =
    let parser = create () in
    let result =
      parser (TestData.protocol_version ^ TestData.client_init_invalid_255)
    in
    match result with
    | Error msg ->
        Alcotest.(check bool)
          "Invalid ClientInit (255) returns error" true
          (String.starts_with ~prefix:"Parse error: BadHandshake:" msg)
    | Ok _ ->
        Alcotest.fail
          "Expected Error but got Ok - invalid handshake should fail"

  let test_client_init_parsing () =
    let parser = create () in
    let result = parser (TestData.protocol_version ^ TestData.client_init) in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Complete handshake parsed correctly" ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_set_pixel_format_parsing () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.set_pixel_format in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "SetPixelFormat parsed correctly" ["SetPixelFormat"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_set_encodings_parsing () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.set_encodings_simple in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "SetEncodings parsed correctly" ["SetEncodings"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_multiple_encodings_parsing () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.set_encodings_multiple in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Multiple encodings parsed correctly" ["SetEncodings"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_framebuffer_update_request_parsing () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.framebuffer_update_request in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "FramebufferUpdateRequest parsed correctly"
          ["FramebufferUpdateRequest"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_key_event_parsing () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.key_event in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "KeyEvent parsed correctly" ["KeyEvent"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_pointer_event_parsing () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.pointer_event in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "PointerEvent parsed correctly" ["PointerEvent"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_client_cut_text_parsing () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.client_cut_text in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "ClientCutText parsed correctly" ["ClientCutText"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_qemu_client_message_parsing () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.qemu_client_message in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "QEMUClientMessage parsed correctly" ["QEMUClientMessage"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
end

(* Test protocol state transitions *)
module StateTransitionTests = struct
  let test_protocol_state_progression () =
    let parser = create () in

    (* Complete handshake should return Handshake *)
    let result1 = parser (TestData.protocol_version ^ TestData.client_init) in
    ( match result1 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Handshake state: complete handshake" ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;

    (* Should now accept client messages *)
    let result2 = parser TestData.key_event in
    match result2 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Post-handshake state: KeyEvent" ["KeyEvent"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_incomplete_message_in_state () =
    let parser = create () in
    (* Send incomplete handshake data (less than 13 bytes total) *)
    let result1 = parser "VNC 003" in
    ( match result1 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Incomplete handshake returns empty list" []
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;

    (* Complete with wrong protocol but valid client init *)
    let result2 = parser (".003\n" ^ TestData.client_init) in
    match result2 with
    | Error msg ->
        Alcotest.(check bool)
          "Completed wrong handshake returns error" true
          (String.starts_with ~prefix:"Parse error: BadHandshake:" msg)
    | Ok _ ->
        Alcotest.fail
          "Expected Error but got Ok - invalid handshake should fail"

  let test_complete_wrong_message_in_state () =
    let parser = create () in
    (* Send wrong handshake data *)
    let wrong_handshake_data = "VNC 003.003\n" ^ TestData.client_init in
    let result = parser wrong_handshake_data in
    match result with
    | Error msg ->
        Alcotest.(check bool)
          "Complete wrong handshake returns error" true
          (String.starts_with ~prefix:"Parse error: BadHandshake:" msg)
    | Ok _ ->
        Alcotest.fail
          "Expected Error but got Ok - invalid handshake should fail"

  let test_multiple_messages_in_sequence () =
    let parser = create () in
    (* Send protocol version and client init *)
    let _ = parser TestData.protocol_version in
    let _ = parser TestData.client_init in

    (* Send multiple client messages *)
    let result1 = parser TestData.key_event in
    let result2 = parser TestData.pointer_event in
    let result3 = parser TestData.framebuffer_update_request in

    ( match result1 with
    | Ok messages ->
        Alcotest.(check (list string))
          "First client message" ["KeyEvent"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;
    ( match result2 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Second client message" ["PointerEvent"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;
    match result3 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Third client message"
          ["FramebufferUpdateRequest"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
end

(* Test error handling and edge cases *)
module ErrorHandlingTests = struct
  let test_unsupported_message_handling () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.unsupported_message in
    match result with
    | Error msg ->
        Alcotest.(check bool)
          "unsupported message returns error" true
          (String.starts_with ~prefix:"Parse error: UnsupportedMsg:" msg)
    | Ok _ ->
        Alcotest.fail
          "Expected Error but got Ok - unsupported message should fail"

  let test_partial_message_handling () =
    let parser = create () in
    (* Send only part of handshake *)
    let result1 = parser "RFB 003" in
    ( match result1 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Partial message returns empty" []
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;

    (* Complete the handshake *)
    let result2 = parser (".003\n" ^ TestData.client_init) in
    match result2 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Completed handshake parses correctly" ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_empty_data_handling () =
    let parser = create () in
    let result = parser "" in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Empty data returns empty list" []
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_very_large_data_handling () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in

    (* Create a very large ClientCutText message *)
    let large_text_length = 1000 in
    let large_text_header = int_list_to_string [6; 0; 0; 0; 0; 0; 3; 232] in
    let large_text_data = String.make large_text_length 'A' in
    let large_message = large_text_header ^ large_text_data in

    let result = parser large_message in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Large message parsed correctly" ["ClientCutText"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
end

(* Test parser lifecycle and helper functions *)
module UtilityTests = struct
  let test_parser_creation () =
    let parser1 = create () in
    let parser2 = create () in
    (* Verify that different parser instances are independent *)
    let _ = parser1 (TestData.protocol_version ^ TestData.client_init) in
    let result = parser2 (TestData.protocol_version ^ TestData.client_init) in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "New parser starts fresh" ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  (* Note: string_of_message_type function is now internal to the module *)
end

(* Test concatenated messages and complex scenarios *)
module IntegrationTests = struct
  let test_concatenated_messages () =
    let parser = create () in
    (* Send protocol version and client init in one chunk *)
    let combined_data = TestData.protocol_version ^ TestData.client_init in
    let result = parser combined_data in
    match result with
    | Ok messages ->
        Alcotest.(check (list string))
          "Concatenated handshake parsed correctly" ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)

  let test_mixed_valid_invalid_messages () =
    let parser = create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in

    (* Send valid message followed by invalid *)
    let _ = parser TestData.key_event in
    let result = parser TestData.unsupported_message in
    match result with
    | Error msg ->
        Alcotest.(check bool)
          "Invalid message after valid returns error" true
          (String.starts_with ~prefix:"Parse error: UnsupportedMsg:" msg)
    | Ok _ ->
        Alcotest.fail
          "Expected Error but got Ok - unsupported message should fail"

  let test_complete_protocol_flow () =
    let parser = create () in

    (* Complete protocol handshake *)
    let result1 = parser (TestData.protocol_version ^ TestData.client_init) in

    (* Send various client messages *)
    let result2 = parser TestData.set_pixel_format in
    let result3 = parser TestData.set_encodings_simple in
    let result4 = parser TestData.framebuffer_update_request in
    let result5 = parser TestData.key_event in
    let result6 = parser TestData.pointer_event in

    ( match result1 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Complete handshake" ["Handshake"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;
    ( match result2 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Set pixel format" ["SetPixelFormat"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;
    ( match result3 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Set encodings" ["SetEncodings"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;
    ( match result4 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Framebuffer update request"
          ["FramebufferUpdateRequest"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;
    ( match result5 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Key event" ["KeyEvent"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
    ) ;
    match result6 with
    | Ok messages ->
        Alcotest.(check (list string))
          "Pointer event" ["PointerEvent"]
          (List.map string_of_msg messages)
    | Error msg ->
        Alcotest.fail ("Expected Ok but got Error: " ^ msg)
end

(* Alcotest test suite *)
let tests =
  [
    ( "Message Parsing"
    , [
        ( "test_protocol_version_parsing"
        , `Quick
        , MessageParsingTests.test_protocol_version_parsing
        )
      ; ( "test_invalid_protocol_version"
        , `Quick
        , MessageParsingTests.test_invalid_protocol_version
        )
      ; ( "test_invalid_protocol_version_rfb_004"
        , `Quick
        , MessageParsingTests.test_invalid_protocol_version_rfb_004
        )
      ; ( "test_incomplete_protocol_version"
        , `Quick
        , MessageParsingTests.test_incomplete_protocol_version
        )
      ; ( "test_client_init_shared"
        , `Quick
        , MessageParsingTests.test_client_init_shared
        )
      ; ( "test_client_init_exclusive"
        , `Quick
        , MessageParsingTests.test_client_init_exclusive
        )
      ; ( "test_client_init_invalid"
        , `Quick
        , MessageParsingTests.test_client_init_invalid
        )
      ; ( "test_client_init_invalid_255"
        , `Quick
        , MessageParsingTests.test_client_init_invalid_255
        )
      ; ( "test_client_init_parsing"
        , `Quick
        , MessageParsingTests.test_client_init_parsing
        )
      ; ( "test_set_pixel_format_parsing"
        , `Quick
        , MessageParsingTests.test_set_pixel_format_parsing
        )
      ; ( "test_set_encodings_parsing"
        , `Quick
        , MessageParsingTests.test_set_encodings_parsing
        )
      ; ( "test_multiple_encodings_parsing"
        , `Quick
        , MessageParsingTests.test_multiple_encodings_parsing
        )
      ; ( "test_framebuffer_update_request_parsing"
        , `Quick
        , MessageParsingTests.test_framebuffer_update_request_parsing
        )
      ; ( "test_key_event_parsing"
        , `Quick
        , MessageParsingTests.test_key_event_parsing
        )
      ; ( "test_pointer_event_parsing"
        , `Quick
        , MessageParsingTests.test_pointer_event_parsing
        )
      ; ( "test_client_cut_text_parsing"
        , `Quick
        , MessageParsingTests.test_client_cut_text_parsing
        )
      ; ( "test_qemu_client_message_parsing"
        , `Quick
        , MessageParsingTests.test_qemu_client_message_parsing
        )
      ]
    )
  ; ( "State Transitions"
    , [
        ( "test_protocol_state_progression"
        , `Quick
        , StateTransitionTests.test_protocol_state_progression
        )
      ; ( "test_incomplete_message_in_state"
        , `Quick
        , StateTransitionTests.test_incomplete_message_in_state
        )
      ; ( "test_complete_wrong_message_in_state"
        , `Quick
        , StateTransitionTests.test_complete_wrong_message_in_state
        )
      ; ( "test_multiple_messages_in_sequence"
        , `Quick
        , StateTransitionTests.test_multiple_messages_in_sequence
        )
      ]
    )
  ; ( "Error Handling"
    , [
        ( "test_unsupported_message_handling"
        , `Quick
        , ErrorHandlingTests.test_unsupported_message_handling
        )
      ; ( "test_partial_message_handling"
        , `Quick
        , ErrorHandlingTests.test_partial_message_handling
        )
      ; ( "test_empty_data_handling"
        , `Quick
        , ErrorHandlingTests.test_empty_data_handling
        )
      ; ( "test_very_large_data_handling"
        , `Quick
        , ErrorHandlingTests.test_very_large_data_handling
        )
      ]
    )
  ; ( "Utility Functions"
    , [("test_parser_creation", `Quick, UtilityTests.test_parser_creation)]
    )
  ; ( "Integration Tests"
    , [
        ( "test_concatenated_messages"
        , `Quick
        , IntegrationTests.test_concatenated_messages
        )
      ; ( "test_mixed_valid_invalid_messages"
        , `Quick
        , IntegrationTests.test_mixed_valid_invalid_messages
        )
      ; ( "test_complete_protocol_flow"
        , `Quick
        , IntegrationTests.test_complete_protocol_flow
        )
      ]
    )
  ]
