exception UCS_value_out_of_range
exception UCS_value_prohibited_in_UTF8
exception UCS_value_prohibited_in_XML
exception UTF8_header_byte_invalid
exception UTF8_continuation_byte_invalid
exception UTF8_encoding_not_canonical
exception String_incomplete

(** Provides functionality for validating and processing  *)
(** strings according to a particular character encoding. *)
module type STRING_VALIDATOR = sig

	(** Returns true if and only if the given string is validly-encoded. *)
	val is_valid : string -> bool

	(** Raises an encoding error if the given string is not validly-encoded. *)
	val validate: string -> unit

	(** Returns the longest validly-encoded prefix of the given string. *)
	val longest_valid_prefix : string -> string

end

(** Represents a validation error as a tuple (i,e), where: *)
(**   i = the index of the first non-compliant character;  *)
(**   e = the reason for non-compliance.                   *)
exception Validation_error of int * exn

(** Provides functions for validating and processing   *)
(** strings according to the UTF-8 character encoding. *)
(**                                                    *)
(** Validly-encoded strings must satisfy RFC 3629.     *)
(**                                                    *)
(** For further information, see:                      *)
(** http://www.rfc.net/rfc3629.html                    *)
module UTF8 : STRING_VALIDATOR

(** Provides functions for validating and processing   *)
(** strings according to the UTF-8 character encoding, *)
(** with certain additional restrictions on UCS values *)
(** imposed by the XML specification.                  *)
(**                                                    *)
(** Validly-encoded strings must satisfy both RFC 3629 *)
(** and section 2.2 of the XML specification.          *)
(**                                                    *)
(** For further information, see:                      *)
(** http://www.rfc.net/rfc3629.html                    *)
(** http://www.w3.org/TR/REC-xml/#charsets             *)
module UTF8_XML : STRING_VALIDATOR
