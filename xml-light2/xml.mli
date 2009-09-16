(** tree representation *)
type xml =
	| Element of (string * (string * string) list * xml list)
	| PCData of string

type error_pos
type error = string * error_pos

exception Error of error

val error : error -> string

(** input functions *)
val parse_file : string -> xml
val parse_in : in_channel -> xml
val parse_string : string -> xml
val parse_bigbuffer : Bigbuffer.t -> xml

(** output functions *)
val to_fct : xml -> (string -> unit) -> unit
val to_fct_fmt : xml -> (string -> unit) -> unit
val to_string : xml -> string
val to_string_fmt : xml -> string
val to_bigbuffer : xml -> Bigbuffer.t

(** helper functions *)
exception Not_pcdata of string
exception Not_element of string
val pcdata : xml -> string
val children : xml -> xml list
val tag : xml -> string
