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

(* === Unicode Functions === *)

module UCS = struct

  let is_non_character value = false
                               || (0xfdd0 <= value && value <= 0xfdef) (* case 1 *)
                               || (Int.logand 0xfffe value = 0xfffe) (* case 2 *)
                               [@@inline]

end

module XML = struct

  let is_forbidden_control_character value = let value = Uchar.to_int value in
                                             value < 0x20
                                             && value <> 0x09
                                             && value <> 0x0a
                                             && value <> 0x0d
                                             [@@inline]

end

(* === UCS Validators === *)

module type UCS_VALIDATOR = sig

  val validate : Uchar.t -> unit [@@inline]

end

module UTF8_UCS_validator : UCS_VALIDATOR = struct

  let validate value =
    if (UCS.is_non_character[@inlined]) (Uchar.to_int value) then raise UCS_value_prohibited_in_UTF8

end

module XML_UTF8_UCS_validator : UCS_VALIDATOR = struct

  let validate value =
    UTF8_UCS_validator.validate value;
    if (XML.is_forbidden_control_character[@inlined]) value
    then raise UCS_value_prohibited_in_XML

end

(* ==== Character Codecs ==== *)

module UTF8_CODEC (UCS_validator : UCS_VALIDATOR) = struct
  (* === Decoding === *)

  let decode_continuation_byte byte =
    if byte land 0b11000000 = 0b10000000 then byte land 0b00111111 else
      raise UTF8_continuation_byte_invalid
    [@@inline]

  let rec decode_continuation_bytes string last value index =
    if index <= last then
      let chunk = (decode_continuation_byte[@inlined]) (Char.code string.[index]) in
      let value = (value lsl 6) lor chunk in
      decode_continuation_bytes string last value (index + 1)
    else value

  let validate_character string index =
    let value, width =
      let byte = Char.code string.[index] in
      if byte land 0b10000000 = 0b00000000 then (byte               , 1) else
      if byte land 0b11100000 = 0b11000000 then (byte land 0b0011111, 2) else
      if byte land 0b11110000 = 0b11100000 then (byte land 0b0001111, 3) else
      if byte land 0b11111000 = 0b11110000 then (byte land 0b0000111, 4) else
        raise UTF8_header_byte_invalid
    in
    let value = if width = 1 then value
      else decode_continuation_bytes string (index+width-1) value (index+1)
    in
    UCS_validator.validate (Uchar.unsafe_of_int value);
    width
    [@@inline]

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

module String_validator (Validator : UCS_VALIDATOR) : STRING_VALIDATOR = struct
  include UTF8_CODEC(Validator)

  let rec validate_aux string length index =
    if index = length then ()
    else
    let width =
      try (validate_character[@inlined]) string index
      with
      | Invalid_argument _ -> raise String_incomplete
      | error -> raise (Validation_error (index, error))
    in
    validate_aux string length (index + width)

  let validate string =
    validate_aux string (String.length string) 0

  let is_valid string =
    try validate string; true with _ -> false

  let longest_valid_prefix string =
    try validate string; string
    with Validation_error (index, _) -> String.sub string 0 index

end

module UTF8     = String_validator (UTF8_UCS_validator)
module UTF8_XML = String_validator (XML_UTF8_UCS_validator)