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
exception UCS_value_out_of_range
exception UCS_value_prohibited_in_UTF8
exception UCS_value_prohibited_in_XML
exception UTF8_character_incomplete
exception UTF8_header_byte_invalid
exception UTF8_continuation_byte_invalid
exception UTF8_encoding_not_canonical
exception String_incomplete

(* === Utility Functions === *)

let ( +++ ) = Int32.add
let ( --- ) = Int32.sub
let ( &&& ) = Int32.logand
let ( ||| ) = Int32.logor
let ( <<< ) = Int32.shift_left
let ( >>> ) = Int32.shift_right_logical

(* === Unicode Functions === *)

module UCS = struct

  let min_value = 0x000000l
  let max_value = 0x1fffffl

  let is_non_character value = false
                               || (0xfdd0l <= value && value <= 0xfdefl) (* case 1 *)
                               || (Int32.logand 0xfffel value = 0xfffel) (* case 2 *)

  let is_out_of_range value =
    value < min_value || value > max_value

  let is_surrogate value =
    (0xd800l <= value && value <= 0xdfffl)

end

module XML = struct

  let is_forbidden_control_character value = value < 0x20l
                                             && value <> 0x09l
                                             && value <> 0x0al
                                             && value <> 0x0dl

end

(* === UCS Validators === *)

module type UCS_VALIDATOR = sig

  val validate : int32 -> unit

end

module UTF8_UCS_validator : UCS_VALIDATOR = struct

  let validate value =
    if UCS.is_out_of_range  value then raise UCS_value_out_of_range;
    if UCS.is_non_character value then raise UCS_value_prohibited_in_UTF8;
    if UCS.is_surrogate     value then raise UCS_value_prohibited_in_UTF8

end

module XML_UTF8_UCS_validator : UCS_VALIDATOR = struct

  let validate value =
    UTF8_UCS_validator.validate value;
    if XML.is_forbidden_control_character value
    then raise UCS_value_prohibited_in_XML

end

(* ==== Character Codecs ==== *)

module type CHARACTER_DECODER = sig
  val decode_character : string -> int -> int32 * int
end

module type CHARACTER_ENCODER = sig
  val encode_character : int32 -> string
end

module UTF8_CODEC (UCS_validator : UCS_VALIDATOR) = struct
  let width_required_for_ucs_value value =
    if value < 0x000080l (* 1 lsl  7 *) then 1 else
    if value < 0x000800l (* 1 lsl 11 *) then 2 else
    if value < 0x010000l (* 1 lsl 16 *) then 3 else 4

  (* === Decoding === *)

  let decode_header_byte byte =
    if byte land 0b10000000 = 0b00000000 then (byte               , 1) else
    if byte land 0b11100000 = 0b11000000 then (byte land 0b0011111, 2) else
    if byte land 0b11110000 = 0b11100000 then (byte land 0b0001111, 3) else
    if byte land 0b11111000 = 0b11110000 then (byte land 0b0000111, 4) else
      raise UTF8_header_byte_invalid

  let decode_continuation_byte byte =
    if byte land 0b11000000 = 0b10000000 then byte land 0b00111111 else
      raise UTF8_continuation_byte_invalid

  let decode_character string index =
    let value, width = decode_header_byte (Char.code string.[index]) in
    let value = if width = 1 then (Int32.of_int value)
      else begin
        let value = ref (Int32.of_int value) in
        for index = index + 1 to index + width - 1 do
          let chunk = decode_continuation_byte (Char.code string.[index]) in
          value := (!value <<< 6) ||| (Int32.of_int chunk)
        done;
        if width > (width_required_for_ucs_value !value)
        then raise UTF8_encoding_not_canonical;
        !value
      end in
    UCS_validator.validate value;
    (value, width)

  (* === Encoding === *)

  let encode_header_byte width value =
    match width with
    | 1 -> value
    | 2 -> value ||| 0b11000000l
    | 3 -> value ||| 0b11100000l
    | 4 -> value ||| 0b11110000l
    | _ -> raise UCS_value_out_of_range

  let encode_continuation_byte value =
    ((value &&& 0b00111111l) ||| 0b10000000l, value >>> 6)

  let encode_character value =
    UCS_validator.validate value;
    let width = width_required_for_ucs_value value in
    let b = Bytes.make width ' ' in
    (* Start by encoding the continuation bytes in reverse order. *)
    let rec encode_continuation_bytes remainder index =
      if index = 0 then remainder else
        let byte, remainder = encode_continuation_byte remainder in
        Bytes.set b index @@ Char.chr (Int32.to_int byte);
        encode_continuation_bytes remainder (index - 1) in
    let remainder = encode_continuation_bytes value (width - 1) in
    (* Finish by encoding the header byte. *)
    let byte = encode_header_byte width remainder in
    Bytes.set b 0 @@ Char.chr (Int32.to_int byte);
    Bytes.unsafe_to_string b

end

module     UTF8_codec = UTF8_CODEC (    UTF8_UCS_validator)
module XML_UTF8_codec = UTF8_CODEC (XML_UTF8_UCS_validator)

(* === String Validators === *)

module type STRING_VALIDATOR = sig

  val is_valid : string -> bool
  val validate : string -> unit
  val longest_valid_prefix : string -> string

end

exception Validation_error of int * exn

module String_validator (Decoder : CHARACTER_DECODER) : STRING_VALIDATOR = struct

  let validate string =
    let index = ref 0 and length = String.length string in
    begin try
        while !index < length do
          let _, width = Decoder.decode_character string !index in
          index := !index + width
        done;
      with
      | Invalid_argument _ -> raise String_incomplete
      | error -> raise (Validation_error (!index, error))
    end; assert (!index = length)

  let is_valid string =
    try validate string; true with _ -> false

  let longest_valid_prefix string =
    try validate string; string
    with Validation_error (index, _) -> String.sub string 0 index

end

module UTF8     = String_validator (    UTF8_codec)
module UTF8_XML = String_validator (XML_UTF8_codec)
