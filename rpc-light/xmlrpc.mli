exception Parse_error of string * string

val to_string : Rpc.t -> string
val of_string : string -> Rpc.t
