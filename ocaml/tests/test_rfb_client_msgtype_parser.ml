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
let bytes_to_string bytes =
  let buf = Bytes.create (List.length bytes) in
  List.iteri (fun i b -> Bytes.set_uint8 buf i b) bytes ;
  Bytes.to_string buf

(* Helper function to create test data for different message types *)
module TestData = struct
  (* Valid RFB protocol version string: "RFB 003.003\n" *)
  let protocol_version = "RFB 003.003\n"

  (* Invalid protocol versions for negative testing *)
  let invalid_protocol_version = "VNC 003.003\n"

  let invalid_protocol_version_rfb_004 = "RFB 003.004\n"

  let incomplete_protocol_version = "RFB 003.003" (* 11 bytes, incomplete *)

  (* ClientInit messages: 1 byte (shared flag) *)
  let client_init = bytes_to_string [1] (* Shared access *)

  let client_init_exclusive = bytes_to_string [0] (* Exclusive access *)

  let client_init_invalid = bytes_to_string [2] (* Invalid value *)

  let client_init_invalid_255 = bytes_to_string [255]
  (* Another invalid value *)

  (* SetPixelFormat: message-type(0) + padding(3) + pixel-format(16) *)
  let set_pixel_format =
    bytes_to_string ([0; 0; 0; 0] @ List.init 16 (fun _ -> 0))

  (* SetEncodings: message-type(2) + padding(1) + num-encodings(2) + encodings(4*n) *)
  let set_encodings_simple = bytes_to_string [2; 0; 0; 1; 0; 0; 0; 1]
  (* 1 encoding *)

  let set_encodings_multiple =
    bytes_to_string ([2; 0; 0; 2] @ [0; 0; 0; 1; 0; 0; 0; 2])
  (* 2 encodings *)

  (* FramebufferUpdateRequest: message-type(3) + incremental(1) + x(2) + y(2) + width(2) + height(2) *)
  let framebuffer_update_request =
    bytes_to_string [3; 0; 0; 0; 0; 0; 1; 0; 1; 0]

  (* KeyEvent: message-type(4) + down-flag(1) + padding(2) + key(4) *)
  let key_event = bytes_to_string [4; 1; 0; 0; 0; 0; 0; 65]
  (* 'A' key pressed *)

  (* PointerEvent: message-type(5) + button-mask(1) + x(2) + y(2) *)
  let pointer_event = bytes_to_string [5; 1; 0; 100; 0; 50]
  (* Left button at (100, 50) *)

  (* ClientCutText: message-type(6) + padding(3) + length(4) + text *)
  let client_cut_text =
    bytes_to_string
      ([6; 0; 0; 0; 0; 0; 0; 5] @ List.map Char.code ['H'; 'e'; 'l'; 'l'; 'o'])

  (* QEMUClientMessage: message-type(255) + data(11) *)
  let qemu_client_message = bytes_to_string ([255] @ List.init 11 (fun _ -> 0))

  (* Invalid/unknown message types *)
  let unknown_message = bytes_to_string [99] (* Unknown message type *)
end

(* Test individual message type parsing functions *)
module MessageParsingTests = struct
  let test_protocol_version_parsing () =
    let parser = RfbParser.create () in
    let result = parser (TestData.protocol_version ^ TestData.client_init) in
    Alcotest.(check (list string))
      "Complete handshake parsed correctly" ["Handshake"] result

  let test_invalid_protocol_version () =
    let parser = RfbParser.create () in
    let result =
      parser (TestData.invalid_protocol_version ^ TestData.client_init)
    in
    match result with
    | [] ->
        Alcotest.fail "Expected BadHandshake message but got empty list"
    | msg :: _ ->
        Alcotest.(check string)
          "Invalid protocol version returns BadHandshake" "BadHandshake" msg

  let test_invalid_protocol_version_rfb_004 () =
    let parser = RfbParser.create () in
    let result =
      parser (TestData.invalid_protocol_version_rfb_004 ^ TestData.client_init)
    in
    match result with
    | [] ->
        Alcotest.fail "Expected BadHandshake message but got empty list"
    | msg :: _ ->
        Alcotest.(check string)
          "RFB 003.004 version returns BadHandshake" "BadHandshake" msg

  let test_incomplete_protocol_version () =
    let parser = RfbParser.create () in
    let result = parser TestData.incomplete_protocol_version in
    Alcotest.(check (list string))
      "Incomplete protocol version returns empty (partial)" [] result

  let test_client_init_shared () =
    let parser = RfbParser.create () in
    let result = parser (TestData.protocol_version ^ TestData.client_init) in
    Alcotest.(check (list string))
      "Complete handshake with shared ClientInit parsed correctly" ["Handshake"]
      result

  let test_client_init_exclusive () =
    let parser = RfbParser.create () in
    let result =
      parser (TestData.protocol_version ^ TestData.client_init_exclusive)
    in
    Alcotest.(check (list string))
      "Complete handshake with exclusive ClientInit parsed correctly"
      ["Handshake"] result

  let test_client_init_invalid () =
    let parser = RfbParser.create () in
    let result =
      parser (TestData.protocol_version ^ TestData.client_init_invalid)
    in
    match result with
    | [] ->
        Alcotest.fail "Expected BadHandshake message but got empty list"
    | msg :: _ ->
        Alcotest.(check string)
          "Invalid ClientInit returns BadHandshake" "BadHandshake" msg

  let test_client_init_invalid_255 () =
    let parser = RfbParser.create () in
    let result =
      parser (TestData.protocol_version ^ TestData.client_init_invalid_255)
    in
    match result with
    | [] ->
        Alcotest.fail "Expected BadHandshake message but got empty list"
    | msg :: _ ->
        Alcotest.(check string)
          "Invalid ClientInit (255) returns BadHandshake" "BadHandshake" msg

  let test_client_init_parsing () =
    let parser = RfbParser.create () in
    let result = parser (TestData.protocol_version ^ TestData.client_init) in
    Alcotest.(check (list string))
      "Complete handshake parsed correctly" ["Handshake"] result

  let test_set_pixel_format_parsing () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.set_pixel_format in
    Alcotest.(check (list string))
      "SetPixelFormat parsed correctly" ["SetPixelFormat"] result

  let test_set_encodings_parsing () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.set_encodings_simple in
    Alcotest.(check (list string))
      "SetEncodings parsed correctly" ["SetEncodings"] result

  let test_multiple_encodings_parsing () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.set_encodings_multiple in
    Alcotest.(check (list string))
      "Multiple encodings parsed correctly" ["SetEncodings"] result

  let test_framebuffer_update_request_parsing () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.framebuffer_update_request in
    Alcotest.(check (list string))
      "FramebufferUpdateRequest parsed correctly"
      ["FramebufferUpdateRequest"]
      result

  let test_key_event_parsing () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.key_event in
    Alcotest.(check (list string))
      "KeyEvent parsed correctly" ["KeyEvent"] result

  let test_pointer_event_parsing () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.pointer_event in
    Alcotest.(check (list string))
      "PointerEvent parsed correctly" ["PointerEvent"] result

  let test_client_cut_text_parsing () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.client_cut_text in
    Alcotest.(check (list string))
      "ClientCutText parsed correctly" ["ClientCutText"] result

  let test_qemu_client_message_parsing () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.qemu_client_message in
    Alcotest.(check (list string))
      "QEMUClientMessage parsed correctly" ["QEMUClientMessage"] result
end

(* Test protocol state transitions *)
module StateTransitionTests = struct
  let test_protocol_state_progression () =
    let parser = RfbParser.create () in

    (* Complete handshake should return Handshake *)
    let result1 = parser (TestData.protocol_version ^ TestData.client_init) in
    Alcotest.(check (list string))
      "Handshake state: complete handshake" ["Handshake"] result1 ;

    (* Should now accept client messages *)
    let result2 = parser TestData.key_event in
    Alcotest.(check (list string))
      "Post-handshake state: KeyEvent" ["KeyEvent"] result2

  let test_incomplete_message_in_state () =
    let parser = RfbParser.create () in
    (* Send incomplete handshake data (less than 13 bytes total) *)
    let result1 = parser "VNC 003" in
    Alcotest.(check (list string))
      "Incomplete handshake returns empty list" [] result1 ;

    (* Complete with wrong protocol but valid client init *)
    let result2 = parser (".003\n" ^ TestData.client_init) in
    match result2 with
    | [] ->
        Alcotest.fail "Expected BadHandshake message but got empty list"
    | msg :: _ ->
        Alcotest.(check string)
          "Completed wrong handshake returns BadHandshake" "BadHandshake" msg

  let test_complete_wrong_message_in_state () =
    let parser = RfbParser.create () in
    (* Send wrong handshake data *)
    let wrong_handshake_data = "VNC 003.003\n" ^ TestData.client_init in
    let result = parser wrong_handshake_data in
    match result with
    | [] ->
        Alcotest.fail "Expected BadHandshake message but got empty list"
    | msg :: _ ->
        Alcotest.(check string)
          "Complete wrong handshake returns BadHandshake" "BadHandshake" msg

  let test_multiple_messages_in_sequence () =
    let parser = RfbParser.create () in
    (* Send protocol version and client init *)
    let _ = parser TestData.protocol_version in
    let _ = parser TestData.client_init in

    (* Send multiple client messages *)
    let result1 = parser TestData.key_event in
    let result2 = parser TestData.pointer_event in
    let result3 = parser TestData.framebuffer_update_request in

    Alcotest.(check (list string)) "First client message" ["KeyEvent"] result1 ;
    Alcotest.(check (list string))
      "Second client message" ["PointerEvent"] result2 ;
    Alcotest.(check (list string))
      "Third client message"
      ["FramebufferUpdateRequest"]
      result3
end

(* Test error handling and edge cases *)
module ErrorHandlingTests = struct
  let test_unknown_message_handling () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in
    let result = parser TestData.unknown_message in
    match result with
    | [] ->
        Alcotest.fail "Expected UnknownMsg message but got empty list"
    | msg :: _ ->
        Alcotest.(check string)
          "Unknown message returns UnknownMsg" "UnknownMsg" msg

  let test_partial_message_handling () =
    let parser = RfbParser.create () in
    (* Send only part of handshake *)
    let result1 = parser "RFB 003" in
    Alcotest.(check (list string)) "Partial message returns empty" [] result1 ;

    (* Complete the handshake *)
    let result2 = parser (".003\n" ^ TestData.client_init) in
    Alcotest.(check (list string))
      "Completed handshake parses correctly" ["Handshake"] result2

  let test_empty_data_handling () =
    let parser = RfbParser.create () in
    let result = parser "" in
    Alcotest.(check (list string)) "Empty data returns empty list" [] result

  let test_very_large_data_handling () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in

    (* Create a very large ClientCutText message *)
    let large_text_length = 1000 in
    let large_text_header = bytes_to_string [6; 0; 0; 0; 0; 0; 3; 232] in
    let large_text_data = String.make large_text_length 'A' in
    let large_message = large_text_header ^ large_text_data in

    let result = parser large_message in
    Alcotest.(check (list string))
      "Large message parsed correctly" ["ClientCutText"] result
end

(* Test parser lifecycle and helper functions *)
module UtilityTests = struct
  let test_parser_creation () =
    let parser1 = RfbParser.create () in
    let parser2 = RfbParser.create () in
    (* Verify that different parser instances are independent *)
    let _ = parser1 (TestData.protocol_version ^ TestData.client_init) in
    let result = parser2 (TestData.protocol_version ^ TestData.client_init) in
    Alcotest.(check (list string))
      "New parser starts fresh" ["Handshake"] result

  (* Note: string_of_message_type function is now internal to the module *)
end

(* Test concatenated messages and complex scenarios *)
module IntegrationTests = struct
  let test_concatenated_messages () =
    let parser = RfbParser.create () in
    (* Send protocol version and client init in one chunk *)
    let combined_data = TestData.protocol_version ^ TestData.client_init in
    let result = parser combined_data in
    Alcotest.(check (list string))
      "Concatenated handshake parsed correctly" ["Handshake"] result

  let test_mixed_valid_invalid_messages () =
    let parser = RfbParser.create () in
    (* Complete handshake first *)
    let _ = parser (TestData.protocol_version ^ TestData.client_init) in

    (* Send valid message followed by invalid *)
    let _ = parser TestData.key_event in
    let result = parser TestData.unknown_message in
    match result with
    | [] ->
        Alcotest.fail "Expected UnknownMsg message but got empty list"
    | msg :: _ ->
        Alcotest.(check string)
          "Invalid message after valid returns UnknownMsg" "UnknownMsg" msg

  let test_complete_protocol_flow () =
    let parser = RfbParser.create () in

    (* Complete protocol handshake *)
    let result1 = parser (TestData.protocol_version ^ TestData.client_init) in

    (* Send various client messages *)
    let result2 = parser TestData.set_pixel_format in
    let result3 = parser TestData.set_encodings_simple in
    let result4 = parser TestData.framebuffer_update_request in
    let result5 = parser TestData.key_event in
    let result6 = parser TestData.pointer_event in

    Alcotest.(check (list string)) "Complete handshake" ["Handshake"] result1 ;
    Alcotest.(check (list string)) "Set pixel format" ["SetPixelFormat"] result2 ;
    Alcotest.(check (list string)) "Set encodings" ["SetEncodings"] result3 ;
    Alcotest.(check (list string))
      "Framebuffer update request"
      ["FramebufferUpdateRequest"]
      result4 ;
    Alcotest.(check (list string)) "Key event" ["KeyEvent"] result5 ;
    Alcotest.(check (list string)) "Pointer event" ["PointerEvent"] result6
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
        ( "test_unknown_message_handling"
        , `Quick
        , ErrorHandlingTests.test_unknown_message_handling
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
