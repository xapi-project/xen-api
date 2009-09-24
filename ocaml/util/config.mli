exception Error of (string * string) list

type ty =
	| Set_bool of bool ref
	| Set_int of int ref
	| Set_string of string ref
	| Set_float of float ref
	| Unit of (unit -> unit)
	| Bool of (bool -> unit)
	| Int of (int -> unit)
	| String of (string -> unit)
	| Float of (float -> unit)

val read: string -> (string * ty) list -> (string -> string -> unit) -> unit
