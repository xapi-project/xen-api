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

module Int = struct
  include Int
  let of_int (x:int) = x
end

type uchar = int

(* === Utility Functions === *)

let ( ||| ) = Int.logor
let ( <<< ) = Int.shift_left

(* === Unicode Functions === *)

module UCS = struct

  let min_value = 0x000000
  let max_value = 0x1fffff

  let is_non_character value = false
                               || (0xfdd0 <= value && value <= 0xfdef) (* case 1 *)
                               || (Int.logand 0xfffe value = 0xfffe) (* case 2 *)

  let is_out_of_range value =
    value < min_value || value > max_value

  let is_surrogate value =
    (0xd800 <= value && value <= 0xdfff)

end

module XML = struct

  let is_forbidden_control_character value = value < 0x20
                                             && value <> 0x09
                                             && value <> 0x0a
                                             && value <> 0x0d

end

(* === UCS Validators === *)

module type UCS_VALIDATOR = sig

  val validate : uchar -> unit

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

module UTF8_CODEC (UCS_validator : UCS_VALIDATOR) = struct
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

  let width_required_for_ucs_value value =
    if value < 0x000080 (* 1 lsl  7 *) then 1 else
    if value < 0x000800 (* 1 lsl 11 *) then 2 else
    if value < 0x010000 (* 1 lsl 16 *) then 3 else 4

  let decode_character string index =
    let value, width = decode_header_byte (Char.code string.[index]) in
    let value = if width = 1 then (Int.of_int value)
      else begin
        let value = ref (Int.of_int value) in
        for index = index + 1 to index + width - 1 do
          let chunk = decode_continuation_byte (Char.code string.[index]) in
          value := (!value <<< 6) ||| (Int.of_int chunk)
        done;
        if width > (width_required_for_ucs_value !value)
        then raise UTF8_encoding_not_canonical;
        !value
      end in
    UCS_validator.validate value;
    (value, width)

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

  let validate string =
    let index = ref 0 and length = String.length string in
    begin try
        while !index < length do
          let _, width = decode_character string !index in
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

module UTF8     = String_validator (UTF8_UCS_validator)
module UTF8_XML = String_validator (XML_UTF8_UCS_validator)
