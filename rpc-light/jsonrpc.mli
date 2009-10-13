	type error =
		| Unexpected_char of int * char * (* json type *) string
		| Invalid_value of int * (* value *) string * (* json type *) string
		| Invalid_leading_zero of int * string
		| Unterminated_value of int * string
		| Internal_error of int * string

exception Parse_error of error

val to_string : Rpc.t -> string
val of_string : string -> Rpc.t
