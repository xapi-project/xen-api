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
  val validate : Uchar.t -> unit
end

(** Accepts all values within the UCS character value range except
 *  those which are invalid for all UTF-8-encoded XML documents. *)
module XML_UTF8_UCS_validator : UCS_VALIDATOR

module XML : sig
  (** Returns true if and only if the given value corresponds to
      	 *  a forbidden control character as defined in section 2.2 of
      	 *  the XML specification, version 1.0. *)
  val is_forbidden_control_character : Uchar.t -> bool
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

module String_validator (Decoder : UCS_VALIDATOR) : STRING_VALIDATOR

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
