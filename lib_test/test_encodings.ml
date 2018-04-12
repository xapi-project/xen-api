(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
module E = Xapi_stdext_encodings.Encodings
(* Pull in the infix operators from Encodings used in this test *)
let (---), (+++), (<<<) = E.( (---), (+++), (<<<) )

(* === Mock exceptions  ==================================================== *)

(** Simulates a decoding error. *)
exception Decode_error

(* === Mock types ===========================================================*)

(** Generates mock character widths, in bytes. *)
module type WIDTH_GENERATOR = sig val next : unit -> int end

(* === Mock UCS validators ================================================= *)

(** A validator that always succeeds. *)
module Lenient_UCS_validator : E.UCS_VALIDATOR = struct
  let validate value = ()
end

(* === Mock character decoders ============================================= *)

(** A character decoder that logs every index it is called with. *)
module Logged_character_decoder (W : WIDTH_GENERATOR) = struct

  (** The indices already supplied to the decoder. *)
  let indices = ref ([] : int list)

  (** Clears the list of indices. *)
  let reset () = indices := []

  (** Records the given index in the list of indices. *)
  let decode_character string index =
    let width = W.next () in
    for index = index to index + width - 1 do
      ignore (string.[index])
    done;
    indices := (index :: !indices);
    0l, width

end

module Logged_1_byte_character_decoder = Logged_character_decoder
    (struct let next () = 1 end)
module Logged_2_byte_character_decoder = Logged_character_decoder
    (struct let next () = 2 end)
module Logged_n_byte_character_decoder = Logged_character_decoder
    (struct let last = ref 0 let next () = incr last; !last end)

(** A decoder that succeeds for all characters. *)
module Universal_character_decoder = struct
  let decode_character string index = (0l, 1)
end

(** A decoder that fails for all characters. *)
module Failing_character_decoder = struct
  let decode_character string index = raise Decode_error
end

(** A decoder that succeeds for all characters except the letter 'F'. *)
module Selective_character_decoder = struct
  let decode_character string index =
    if string.[index] = 'F' then raise Decode_error else (0l, 1)
end

(* === Mock codecs ========================================================= *)

module Lenient_UTF8_codec = E.UTF8_CODEC (Lenient_UCS_validator)

(* === Mock string validators ============================================== *)

module Logged_1_byte_character_string_validator = E.String_validator
    (Logged_1_byte_character_decoder)
module Logged_2_byte_character_string_validator = E.String_validator
    (Logged_2_byte_character_decoder)
module Logged_n_byte_character_string_validator = E.String_validator
    (Logged_n_byte_character_decoder)

(** A validator that accepts all strings. *)
module Universal_string_validator = E.String_validator
    (Universal_character_decoder)

(** A validator that rejects all strings. *)
module Failing_string_validator = E.String_validator
    (Failing_character_decoder)

(** A validator that rejects strings containing the character 'F'. *)
module Selective_string_validator = E.String_validator
    (Selective_character_decoder)

(* === Test helpers ======================================================== *)

let assert_true = Alcotest.(check bool) "true" true
let assert_false = Alcotest.(check bool) "false" false
let check_indices = Alcotest.(check (list int)) "indices"

let assert_raises_match exception_match fn =
  try
    fn ();
    Alcotest.fail "assert_raises_match: failure expected"
  with failure ->
    if not (exception_match failure)
    then raise failure
    else ()

(* === Tests =============================================================== *)

module String_validator = struct

  let test_is_valid () =
    assert_true  (Universal_string_validator.is_valid ""         );
    assert_true  (Universal_string_validator.is_valid "123456789");
    assert_true  (Selective_string_validator.is_valid ""         );
    assert_true  (Selective_string_validator.is_valid "123456789");
    assert_false (Selective_string_validator.is_valid "F23456789");
    assert_false (Selective_string_validator.is_valid "1234F6789");
    assert_false (Selective_string_validator.is_valid "12345678F");
    assert_false (Selective_string_validator.is_valid "FFFFFFFFF")

  let test_longest_valid_prefix () =
        Alcotest.(check string) "prefix" (Universal_string_validator.longest_valid_prefix ""         ) ""         ;
        Alcotest.(check string) "prefix" (Universal_string_validator.longest_valid_prefix "123456789") "123456789";
        Alcotest.(check string) "prefix" (Selective_string_validator.longest_valid_prefix ""         ) ""         ;
        Alcotest.(check string) "prefix" (Selective_string_validator.longest_valid_prefix "123456789") "123456789";
        Alcotest.(check string) "prefix" (Selective_string_validator.longest_valid_prefix "F23456789") ""         ;
        Alcotest.(check string) "prefix" (Selective_string_validator.longest_valid_prefix "1234F6789") "1234"     ;
        Alcotest.(check string) "prefix" (Selective_string_validator.longest_valid_prefix "12345678F") "12345678" ;
        Alcotest.(check string) "prefix" (Selective_string_validator.longest_valid_prefix "FFFFFFFFF") ""

  let test_validate_with_1_byte_characters () =
        Logged_1_byte_character_decoder.reset ();
        Logged_1_byte_character_string_validator.validate "0123456789";
        Alcotest.(check (list int)) "indices" !Logged_1_byte_character_decoder.indices [9;8;7;6;5;4;3;2;1;0]

  let test_validate_with_2_byte_characters () =
        Logged_2_byte_character_decoder.reset ();
        Logged_2_byte_character_string_validator.validate "0123456789";
        Alcotest.(check (list int)) "indices" !Logged_2_byte_character_decoder.indices [8;6;4;2;0]

  let test_validate_with_n_byte_characters () =
        Logged_n_byte_character_decoder.reset ();
        Logged_n_byte_character_string_validator.validate "0123456789";
        check_indices !Logged_n_byte_character_decoder.indices [6;3;1;0]

  (** Tests that validation does not fail for an empty string. *)
  let test_validate_with_empty_string () =
        Logged_1_byte_character_decoder.reset ();
        Logged_1_byte_character_string_validator.validate "";
        check_indices !Logged_1_byte_character_decoder.indices []

  let test_validate_with_incomplete_string () =
        Logged_2_byte_character_decoder.reset ();
        Alcotest.check_raises
      "Validation fails correctly for an incomplete string"
 E.String_incomplete
          (fun () -> Logged_2_byte_character_string_validator.validate "0")

  let test_validate_with_failing_decoders () =
        Failing_string_validator.validate "";
        assert_raises_match
          (function E.Validation_error (0, Decode_error) -> true | _ -> false)
          (fun () -> Selective_string_validator.validate "F");
        assert_raises_match
          (function E.Validation_error (0, Decode_error) -> true | _ -> false)
          (fun () -> Selective_string_validator.validate "F12345678");
        assert_raises_match
          (function E.Validation_error (4, Decode_error) -> true | _ -> false)
          (fun () -> Selective_string_validator.validate "0123F5678");
        assert_raises_match
          (function E.Validation_error (8, Decode_error) -> true | _ -> false)
          (fun () -> Selective_string_validator.validate "01234567F");
        assert_raises_match
          (function E.Validation_error (0, Decode_error) -> true | _ -> false)
          (fun () -> Selective_string_validator.validate "FFFFFFFFF")

  let tests =
    [ "test_is_valid", `Quick, test_is_valid
    ; "test_longest_valid_prefix", `Quick, test_longest_valid_prefix
    ; "test_validate_with_1_byte_characters", `Quick, test_validate_with_1_byte_characters
    ; "test_validate_with_2_byte_characters", `Quick, test_validate_with_2_byte_characters
    ; "test_validate_with_n_byte_characters", `Quick, test_validate_with_n_byte_characters
    ; "test_validate_with_empty_string", `Quick, test_validate_with_empty_string
    ; "test_validate_with_incomplete_string", `Quick, test_validate_with_incomplete_string
    ; "test_validate_with_failing_decoders", `Quick, test_validate_with_failing_decoders
    ]

end

module UCS = struct include E.UCS

  (** A list of UCS non-characters values, including:               *)
  (** a. non-characters within the basic multilingual plane;        *)
  (** b. non-characters at the end of the basic multilingual plane; *)
  (** c. non-characters at the end of the private use area.         *)
  let non_characters = [
    0x00fdd0l; 0x00fdefl; (* case a. *)
    0x00fffel; 0x00ffffl; (* case b. *)
    0x1ffffel; 0x1fffffl; (* case c. *)
  ]

  (** A list of UCS character values located immediately before or  *)
  (** after UCS non-character values, including:                    *)
  (** a. non-characters within the basic multilingual plane;        *)
  (** b. non-characters at the end of the basic multilingual plane; *)
  (** c. non-characters at the end of the private use area.         *)
  let valid_characters_next_to_non_characters = [
    0x00fdcfl; 0x00fdf0l; (* case a. *)
    0x00fffdl; 0x010000l; (* case b. *)
    0x1ffffdl; 0x200000l; (* case c. *)
  ]

  let test_is_non_character () =
        List.iter (fun value -> assert_true (is_non_character (value)))
          non_characters;
        List.iter (fun value -> assert_false (is_non_character (value)))
          valid_characters_next_to_non_characters

  let test_is_out_of_range () =
        assert_true  (is_out_of_range (min_value --- 1l));
        assert_false (is_out_of_range (min_value));
        assert_false (is_out_of_range (max_value));
        assert_true  (is_out_of_range (max_value +++ 1l))

  let test_is_surrogate () =
        assert_false (is_surrogate (0xd7ffl));
        assert_true  (is_surrogate (0xd800l));
        assert_true  (is_surrogate (0xdfffl));
        assert_false (is_surrogate (0xe000l))

  let tests =
    [ "test_is_non_character", `Quick, test_is_non_character
    ; "test_is_out_of_range", `Quick, test_is_out_of_range
    ; "test_is_surrogate", `Quick, test_is_surrogate
    ]

end

module XML = struct include E.XML

  let test_is_forbidden_control_character () =
        assert_true  (is_forbidden_control_character (0x00l));
        assert_true  (is_forbidden_control_character (0x19l));
        assert_false (is_forbidden_control_character (0x09l));
        assert_false (is_forbidden_control_character (0x0al));
        assert_false (is_forbidden_control_character (0x0dl));
        assert_false (is_forbidden_control_character (0x20l))

  let tests =
      [ "test_is_forbidden_control_character", `Quick, test_is_forbidden_control_character
      ]

end

module UTF8_UCS_validator = struct include E.UTF8_UCS_validator

  let test_validate () =
        let value = ref (UCS.min_value --- 1l) in
        while !value <= (UCS.max_value +++ 1l) do
          if UCS.is_out_of_range !value
          then Alcotest.check_raises "should fail"
              E.UCS_value_out_of_range
              (fun () -> validate !value)
          else
          if UCS.is_non_character !value
          || UCS.is_surrogate     !value
          then Alcotest.check_raises "should fail"
              E.UCS_value_prohibited_in_UTF8
              (fun () -> validate !value)
          else
            validate !value;
          value := !value +++ 1l
        done

  let tests =
    [ "test_vaidate", `Quick, test_validate
    ]

end

(** Tests the XML-specific UTF-8 UCS validation function. *)
module XML_UTF8_UCS_validator = struct include E.XML_UTF8_UCS_validator

  let test_validate () =
        let value = ref (UCS.min_value --- 1l) in
        while !value <= (UCS.max_value +++ 1l) do
          if UCS.is_out_of_range !value
          then Alcotest.check_raises "should fail" E.UCS_value_out_of_range
              (fun () -> validate !value)
          else
          if UCS.is_non_character !value
          || UCS.is_surrogate     !value
          then Alcotest.check_raises "should fail" E.UCS_value_prohibited_in_UTF8
              (fun () -> validate !value)
          else
          if XML.is_forbidden_control_character !value
          then Alcotest.check_raises "should fail" E.UCS_value_prohibited_in_XML
              (fun () -> validate !value)
          else
            validate !value;
          value := !value +++ 1l
        done

  let tests =
      [ "test_validate", `Quick, test_validate
      ]

end

module UTF8_codec = struct include E.UTF8_codec

  (** A list of canonical encoding widths of UCS values, *)
  (** represented by tuples of the form (v, w), where:   *)
  (** v = the UCS character value to be encoded; and     *)
  (** w = the width of the encoded character, in bytes.  *)
  let valid_ucs_value_widths =
    [
      (1l       , 1); ((1l <<<  7) --- 1l, 1);
      (1l <<<  7, 2); ((1l <<< 11) --- 1l, 2);
      (1l <<< 11, 3); ((1l <<< 16) --- 1l, 3);
      (1l <<< 16, 4); ((1l <<< 21) --- 1l, 4);
    ]

  let test_width_required_for_ucs_value () =
        List.iter
          (fun (value, width) ->
             Alcotest.(check int) "same ints" (width_required_for_ucs_value value) width)
          valid_ucs_value_widths

  (** A list of valid header byte decodings, represented by   *)
  (** tuples of the form (b, (v, w)), where:                  *)
  (** b = a valid header byte;                                *)
  (** v = the (partial) value contained within the byte; and  *)
  (** w = the total width of the encoded character, in bytes. *)
  let valid_header_byte_decodings =
    [
      (0b00000000, (0b00000000, 1));
      (0b00000001, (0b00000001, 1));
      (0b01111111, (0b01111111, 1));
      (0b11000000, (0b00000000, 2));
      (0b11000001, (0b00000001, 2));
      (0b11011111, (0b00011111, 2));
      (0b11100000, (0b00000000, 3));
      (0b11100001, (0b00000001, 3));
      (0b11101111, (0b00001111, 3));
      (0b11110000, (0b00000000, 4));
      (0b11110001, (0b00000001, 4));
      (0b11110111, (0b00000111, 4));
    ]

  (** A list of invalid header bytes that should not be decodable. *)
  let invalid_header_bytes =
    [
      0b10000000; 0b10111111;
      0b11111000; 0b11111011;
      0b11111100; 0b11111101;
      0b11111110; 0b11111111;
    ]

  let test_decode_header_byte_when_valid () =
        List.iter
          (fun (b, (v, w)) ->
             Alcotest.(check (pair int int)) "same ints" (decode_header_byte b) (v, w))
          valid_header_byte_decodings

  let test_decode_header_byte_when_invalid () =
        List.iter
          (fun b ->
             Alcotest.check_raises "should fail" E.UTF8_header_byte_invalid
               (fun () -> decode_header_byte b |> ignore))
          invalid_header_bytes

  (** A list of valid continuation byte decodings, represented *)
  (** by tuples of the form (b, v), where:                     *)
  (** b = a valid continuation byte; and                       *)
  (** v = the partial value contained within the byte.         *)
  let valid_continuation_byte_decodings =
    [
      (0b10000000, 0b00000000);
      (0b10000001, 0b00000001);
      (0b10111110, 0b00111110);
      (0b10111111, 0b00111111);
    ]

  (** A list of invalid continuation bytes that should not be decodable. *)
  let invalid_continuation_bytes =
    [
      0b00000000; 0b01111111;
      0b11000000; 0b11011111;
      0b11100000; 0b11101111;
      0b11110000; 0b11110111;
      0b11111000; 0b11111011;
      0b11111100; 0b11111101;
      0b11111111; 0b11111110;
    ]

  let test_decode_continuation_byte_when_valid () =
        List.iter
          (fun (byte, value) ->
             Alcotest.(check int) "same ints" (decode_continuation_byte byte) value)
          valid_continuation_byte_decodings

  let test_decode_continuation_byte_when_invalid () =
        List.iter
          (fun byte ->
             Alcotest.check_raises "should fail" E.UTF8_continuation_byte_invalid
               (fun () -> decode_continuation_byte byte |> ignore))
          invalid_continuation_bytes

  (** A list of valid character decodings represented by   *)
  (** tuples of the form (s, (v, w)), where:               *)
  (**                                                      *)
  (** s = a validly-encoded UTF-8 string;                  *)
  (** v = the UCS value represented by the string;         *)
  (**     (which may or may not be valid in its own right) *)
  (** w = the width of the encoded string, in bytes.       *)
  (**                                                      *)
  (** For each byte length b in [1...4], the list contains *)
  (** decodings for:                                       *)
  (**                                                      *)
  (** v_min = the smallest UCS value encodable in b bytes. *)
  (** v_max = the greatest UCS value encodable in b bytes. *)
  (**                                                      *)
  let valid_character_decodings = [
    (*               7654321   *)
    (* 0b0xxxxxxx                                  *)  (* 00000000000000xxxxxxx   *)
    "\x00"             (* 0b00000000                                  *), (0b000000000000000000000l, 1);
    "\x7f"             (* 0b01111111                                  *), (0b000000000000001111111l, 1);
    (*           10987654321   *)
    (* 0b110xxxsx 0b10xxxxxx                       *)  (* 0000000000xxxsxxxxxxx   *)
    "\xc2\x80"         (* 0b11000010 0b10000000                       *), (0b000000000000010000000l, 2);
    "\xdf\xbf"         (* 0b11011111 0b10111111                       *), (0b000000000011111111111l, 2);
    (*      6543210987654321   *)
    (* 0b1110xxxx 0b10sxxxxx 0b10xxxxxx            *)  (*      xxxxsxxxxxxxxxxx   *)
    "\xe0\xa0\x80"     (* 0b11100000 0b10100000 0b10000000            *), (0b000000000100000000000l, 3);
    "\xef\xbf\xbf"     (* 0b11101111 0b10111111 0b10111111            *), (0b000001111111111111111l, 3);
    (* 109876543210987654321   *)
    (* 0b11110xxx 0b10xsxxxx 0b10xxxxxx 0b10xxxxxx *)  (* xxxxsxxxxxxxxxxxxxxxx   *)
    "\xf0\x90\x80\x80" (* 0b11110000 0b10010000 0b10000000 0b10000000 *), (0b000010000000000000000l, 4);
    "\xf7\xbf\xbf\xbf" (* 0b11110111 0b10111111 0b10111111 0b10111111 *), (0b111111111111111111111l, 4);
  ]

  let test_decode_character_when_valid () =
        List.iter
          (fun (string, (value, width)) ->
             Alcotest.(check (pair int32 int)) "same pair"
               (Lenient_UTF8_codec.decode_character string 0)
               (value, width))
          valid_character_decodings

  (** A list of strings containing overlong character encodings. *)
  (** For each byte length b in [2...4], this list contains the  *)
  (** overlong encoding e (v), where v is the UCS value one less *)
  (** than the smallest UCS value validly-encodable in b bytes.  *)
  let overlong_character_encodings =
    [
      "\xc1\xbf"         (* 0b11000001 0b10111111                       *);
      "\xe0\x9f\xbf"     (* 0b11100000 0b10011111 0b10111111            *);
      "\xf0\x8f\xbf\xbf" (* 0b11110000 0b10001111 0b10111111 0b10111111 *);
    ]

  let test_decode_character_when_overlong () =
        List.iter
          (fun string ->
             Alcotest.check_raises "should fail" E.UTF8_encoding_not_canonical
               (fun () -> Lenient_UTF8_codec.decode_character string 0 |> ignore))
          overlong_character_encodings

  (** Encodes a valid UCS value and then decodes it again, testing: *)
  (** a. that the encoded width is canonical for the given value.   *)
  (** b. that the decoded value is identical to the original value. *)
  let test_encode_decode_cycle_for_value value =
    let string = Lenient_UTF8_codec.encode_character value in
    let decoded_value, decoded_width =
      Lenient_UTF8_codec.decode_character string 0 in
    let width = E.UTF8_codec.width_required_for_ucs_value value in
    if (value <> decoded_value) then Alcotest.fail
        (Printf.sprintf
           "expected value %06lx but decoded value %06lx\n"
           value decoded_value);
    if (width <> decoded_width) then Alcotest.fail
        (Printf.sprintf
           "expected width %i but decoded width %i\n"
           width decoded_width)

  let test_encode_decode_cycle () =
        let value = ref UCS.min_value in
        while !value <= UCS.max_value do
          test_encode_decode_cycle_for_value !value;
          value := Int32.add !value 1l;
        done

  let tests =
    [ "test_width_required_for_ucs_value", `Quick, test_width_required_for_ucs_value
    ; "test_decode_header_byte_when_valid", `Quick, test_decode_header_byte_when_valid
    ; "test_decode_header_byte_when_invalid", `Quick, test_decode_header_byte_when_invalid
    ; "test_decode_continuation_byte_when_valid", `Quick, test_decode_continuation_byte_when_valid
    ; "test_decode_continuation_byte_when_invalid", `Quick, test_decode_continuation_byte_when_invalid
    ; "test_decode_character_when_valid", `Quick, test_decode_character_when_valid
    ; "test_decode_character_when_overlong", `Quick, test_decode_character_when_overlong
    ; "test_encode_decode_cycle", `Quick, test_encode_decode_cycle
    ]

end

let tests =
      UCS                   .tests @
      XML                   .tests @
      String_validator      .tests @
      UTF8_UCS_validator    .tests @
      XML_UTF8_UCS_validator.tests @
      UTF8_codec            .tests
