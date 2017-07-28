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

(** Encoding helper modules *)

(** {2 Exceptions} *)

exception UCS_value_out_of_range
exception UCS_value_prohibited_in_UTF8
exception UCS_value_prohibited_in_XML
exception UTF8_character_incomplete
exception UTF8_header_byte_invalid
exception UTF8_continuation_byte_invalid
exception UTF8_encoding_not_canonical
exception String_incomplete

(** {2 UCS Validators} *)

(** Validates UCS character values. *)
module type UCS_VALIDATOR = sig
  val validate : int32 -> unit
end

(** Accepts all values within the UCS character value range
 *  except those which are invalid for all UTF-8 documents. *)
module UTF8_UCS_validator : UCS_VALIDATOR

(** Accepts all values within the UCS character value range except
 *  those which are invalid for all UTF-8-encoded XML documents. *)
module XML_UTF8_UCS_validator : UCS_VALIDATOR

module UCS : sig
  val min_value : int32
  val max_value : int32

  (** Returns true if and only if the given value corresponds to a UCS
      	 *  non-character. Such non-characters are forbidden for use in open
      	 *  interchange of Unicode text data, and include the following:
      	 *    1. values from 0xFDD0 to 0xFDEF; and
      	 *    2. values 0xnFFFE and 0xnFFFF, where (0x0 <= n <= 0x10).
      	 *  See the Unicode 5.0 Standard, section 16.7 for further details. *)
  val is_non_character : int32 -> bool

  (** Returns true if and only if the given value lies outside the
      	 *  entire UCS range. *)
  val is_out_of_range : int32 -> bool

  (** Returns true if and only if the given value corresponds to a UCS
      	 *  surrogate code point, only for use in UTF-16 encoded strings.
      	 *  See the Unicode 5.0 Standard, section 16.6 for further details. *)
  val is_surrogate : int32 -> bool
end

val (+++) : int32 -> int32 -> int32
val (---) : int32 -> int32 -> int32
val (&&&) : int32 -> int32 -> int32
val (|||) : int32 -> int32 -> int32
val (<<<) : int32 -> int -> int32
val (>>>) : int32 -> int -> int32

module XML : sig
  (** Returns true if and only if the given value corresponds to
      	 *  a forbidden control character as defined in section 2.2 of
      	 *  the XML specification, version 1.0. *)
  val is_forbidden_control_character : int32 -> bool
end

(** {2 Character Codecs} *)

module type CHARACTER_ENCODER = sig

  (** Encodes a single character value, returning a string containing
      	 *  the character. Raises an error if the character value is invalid. *)
  val encode_character : int32 -> string

end

module type CHARACTER_DECODER = sig
  (** Decodes a single character embedded within a string. Given a string
      	 *  and an index into that string, returns a tuple (value, width) where:
      	 *    value = the value of the character at the given index; and
      	 *    width = the width of the character at the given index, in bytes.
      	 *  Raises an appropriate error if the character is invalid. *)
  val decode_character : string -> int -> int32 * int
end

module UTF8_CODEC (UCS_validator : UCS_VALIDATOR) : sig
  (** Given a valid UCS value, returns the canonical
      	 *  number of bytes required to encode the value. *)
  val width_required_for_ucs_value : int32 -> int

  (** {3 Decoding} *)

  (** Decodes a header byte, returning a tuple (v, w) where:
      	 *  v = the (partial) value contained within the byte; and
      	 *  w = the total width of the encoded character, in bytes. *)
  val decode_header_byte : int -> int * int

  (** Decodes a continuation byte, returning the
      	 *  6-bit-wide value contained within the byte. *)
  val decode_continuation_byte : int -> int

  (** Decodes a single character embedded within a string. Given a string
      	 *  and an index into that string, returns a tuple (value, width) where:
      	 *    value = the value of the character at the given index; and
      	 *    width = the width of the character at the given index, in bytes.
      	 *  Raises an appropriate error if the character is invalid. *)
  val decode_character : string -> int -> int32 * int

  (** {3 Encoding} *)

  (** Encodes a header byte for the given parameters, where:
      	 *  width = the total width of the encoded character, in bytes;
      	 *  value = the most significant bits of the original UCS value. *)
  val encode_header_byte : int -> int32 -> int32	

  (** Encodes a continuation byte from the given UCS
      	 *  remainder value, returning a tuple (b, r), where:
      	 *  b = the continuation byte;
      	 *  r = a new UCS remainder value. *)
  val encode_continuation_byte : int32 -> int32 * int32

  (** Encodes a single character value, returning a string containing
      	 *  the character. Raises an error if the character value is invalid. *)
  val encode_character : int32 -> string
end

module UTF8_codec : sig
  val width_required_for_ucs_value : int32 -> int
  val decode_header_byte : int -> int * int
  val decode_continuation_byte : int -> int
  val decode_character : string -> int -> int32 * int

  val encode_header_byte : int -> int32 -> int32
  val encode_continuation_byte : int32 -> int32 * int32
  val encode_character : int32 -> string
end

module XML_UTF8_codec : sig
  val width_required_for_ucs_value : int32 -> int
  val decode_header_byte : int -> int * int
  val decode_continuation_byte : int -> int
  val decode_character : string -> int -> int32 * int

  val encode_header_byte : int -> int32 -> int32
  val encode_continuation_byte : int32 -> int32 * int32
  val encode_character : int32 -> string
end

(** {2 String Validators} *)

(** Provides functionality for validating and processing
 *  strings according to a particular character encoding. *)
module type STRING_VALIDATOR = sig

  (** Returns true if and only if the given string is validly-encoded. *)
  val is_valid : string -> bool

  (** Raises an encoding error if the given string is not validly-encoded. *)
  val validate: string -> unit

  (** Returns the longest validly-encoded prefix of the given string. *)
  val longest_valid_prefix : string -> string

end

module String_validator (Decoder : CHARACTER_DECODER) : STRING_VALIDATOR

(** Represents a validation error as a tuple [(i,e)], where:
 *    [i] = the index of the first non-compliant character;
 *    [e] = the reason for non-compliance. *)
exception Validation_error of int * exn

(** Provides functions for validating and processing
 *  strings according to the UTF-8 character encoding.
 *
 *  Validly-encoded strings must satisfy RFC 3629.
 *
 *  For further information, see:
 *  http://www.rfc.net/rfc3629.html *)
module UTF8 : STRING_VALIDATOR

(** Provides functions for validating and processing
 *  strings according to the UTF-8 character encoding,
 *  with certain additional restrictions on UCS values
 *  imposed by the XML specification.
 *
 *  Validly-encoded strings must satisfy both RFC 3629
 *  and section 2.2 of the XML specification.
 *
 *  For further information, see:
 *  http://www.rfc.net/rfc3629.html
 *  http://www.w3.org/TR/REC-xml/#charsets *)
module UTF8_XML : STRING_VALIDATOR
