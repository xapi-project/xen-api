(*
 * Copyright (C) 2007-2008 XenSource Ltd.
 *
 * Authors:
 * Vincent Hanquez <vincent@xensource.com>
 * Jonathan Knowles <jonathan.knowles@citrix.com>
 *
 * Provides functions relating to character and string encodings.
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

	(** Returns true if and only if the given value corresponds to a UCS *)
	(** non-character. Such non-characters are forbidden for use in open *)
	(** interchange of Unicode text data, and include the following:     *)
	(**   1. values from 0xFDD0 to 0xFDEF; and                           *)
	(**   2. values 0xnFFFE and 0xnFFFF, where (0x0 <= n <= 0x10).       *)
	(** See the Unicode 5.0 Standard, section 16.7 for further details.  *)
	let is_non_character value = false
		|| (0xfdd0l <= value && value <= 0xfdefl) (* case 1 *)
		|| (Int32.logand 0xfffel value = 0xfffel) (* case 2 *)

	(** Returns true if and only if the given value lies outside the *)
	(** entire UCS range.                                            *)
	let is_out_of_range value =
		value < min_value || value > max_value
	
	(** Returns true if and only if the given value corresponds to a UCS *)
	(** surrogate code point, only for use in UTF-16 encoded strings.    *)
	(** See the Unicode 5.0 Standard, section 16.6 for further details.  *)
	let is_surrogate value =
		(0xd800l <= value && value <= 0xdfffl)

end

module XML = struct

	(** Returns true if and only if the given value corresponds to *)
	(** a forbidden control character as defined in section 2.2 of *)
	(** the XML specification, version 1.0.                        *)
	let is_forbidden_control_character value = value < 0x20l
		&& value <> 0x09l
		&& value <> 0x0al
		&& value <> 0x0dl

end

(* === UCS Validators === *)

module type UCS_VALIDATOR = sig

	(** Validates UCS character values. *)
	val validate : int32 -> unit

end

module UTF8_UCS_validator : UCS_VALIDATOR = struct

	(** Accepts all values within the UCS character value range *)
	(** except those which are invalid for all UTF-8 documents. *)
	let validate value =
		if UCS.is_out_of_range  value then raise UCS_value_out_of_range;
		if UCS.is_non_character value then raise UCS_value_prohibited_in_UTF8;
		if UCS.is_surrogate     value then raise UCS_value_prohibited_in_UTF8

end

module XML_UTF8_UCS_validator : UCS_VALIDATOR = struct

	(** Accepts all values within the UCS character value range except *)
	(** those which are invalid for all UTF-8-encoded XML documents.   *)
	let validate value =
		UTF8_UCS_validator.validate value;
		if XML.is_forbidden_control_character value
		then raise UCS_value_prohibited_in_XML

end

(* ==== Character Codecs ==== *)

module type CHARACTER_DECODER = sig

	(** Decodes a single character embedded within a string. Given a string  *)
	(** and an index into that string, returns a tuple (value, width) where: *)
	(**   value = the value of the character at the given index; and         *)
	(**   width = the width of the character at the given index, in bytes.   *)
	(** Raises an appropriate error if the character is invalid.             *)
	val decode_character : string -> int -> int32 * int

end

module type CHARACTER_ENCODER = sig

	(** Encodes a single character value, returning a string containing   *)
	(** the character. Raises an error if the character value is invalid. *)
	val encode_character : int32 -> string

end

module UTF8_CODEC (UCS_validator : UCS_VALIDATOR) = struct

	(** Given a valid UCS value, returns the canonical *)
	(** number of bytes required to encode the value.  *)
	let width_required_for_ucs_value value =
		if value < 0x000080l (* 1 <<  7 *) then 1 else
		if value < 0x000800l (* 1 << 11 *) then 2 else
		if value < 0x010000l (* 1 << 16 *) then 3 else 4

	(* === Decoding === *)

	(** Decodes a header byte, returning a tuple (v, w) where:  *)
	(** v = the (partial) value contained within the byte; and  *)
	(** w = the total width of the encoded character, in bytes. *)
	let decode_header_byte byte =
		if byte land 0b10000000 = 0b00000000 then (byte               , 1) else
		if byte land 0b11100000 = 0b11000000 then (byte land 0b0011111, 2) else
		if byte land 0b11110000 = 0b11100000 then (byte land 0b0001111, 3) else
		if byte land 0b11111000 = 0b11110000 then (byte land 0b0000111, 4) else
		raise UTF8_header_byte_invalid

	(** Decodes a continuation byte, returning the  *)
	(** 6-bit-wide value contained within the byte. *)
	let decode_continuation_byte byte =
		if byte land 0b11000000 = 0b10000000 then byte land 0b00111111 else
		raise UTF8_continuation_byte_invalid

	(** @see CHARACTER_DECODER.decode_character *)
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

	(** Encodes a header byte for the given parameters, where:       *)
	(** width = the total width of the encoded character, in bytes;  *)
	(** value = the most significant bits of the original UCS value. *)
	let encode_header_byte width value =
		match width with
			| 1 -> value
			| 2 -> value ||| 0b11000000l
			| 3 -> value ||| 0b11100000l
			| 4 -> value ||| 0b11110000l
			| _ -> raise UCS_value_out_of_range

	(** Encodes a continuation byte from the given UCS    *)
	(** remainder value, returning a tuple (b, r), where: *)
	(** b = the continuation byte;                        *)
	(** r = a new UCS remainder value.                    *)
	let encode_continuation_byte value =
		((value &&& 0b00111111l) ||| 0b10000000l, value >>> 6)

	(** @see CHARACTER_ENCODER.encode_character *)
	let encode_character value =
		UCS_validator.validate value;
		let width = width_required_for_ucs_value value in
		let string = String.make width ' ' in
		(* Start by encoding the continuation bytes in reverse order. *)
		let rec encode_continuation_bytes remainder index =
			if index = 0 then remainder else
			let byte, remainder = encode_continuation_byte remainder in
			string.[index] <- Char.chr (Int32.to_int byte);
			encode_continuation_bytes remainder (index - 1) in
		let remainder = encode_continuation_bytes value (width - 1) in
		(* Finish by encoding the header byte. *)
		let byte = encode_header_byte width remainder in
		string.[0] <- Char.chr (Int32.to_int byte);
		string

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

	(** @see STRING_VALIDATOR.validate *)
	let validate string =
		let index = ref 0 and length = String.length string in
		begin try
			while !index < length do
				let value, width = Decoder.decode_character string !index in
				index := !index + width
			done;
		with
			| Invalid_argument _ -> raise String_incomplete
			| error -> raise (Validation_error (!index, error))
		end; assert (!index = length)

	(** @see STRING_VALIDATOR.is_valid *)
	let is_valid string =
		try validate string; true with _ -> false

	(** @see STRING_VALIDATOR.longest_valid_prefix *)
	let longest_valid_prefix string =
		try validate string; string
		with Validation_error (index, reason) -> String.sub string 0 index

end

module UTF8     = String_validator (    UTF8_codec)
module UTF8_XML = String_validator (XML_UTF8_codec)
