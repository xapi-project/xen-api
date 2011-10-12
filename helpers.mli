val split : string -> int -> string * string
val break : (char -> bool) -> string -> string * string
val str_drop_while : (char -> bool) -> string -> string
val marshal_int : ?bigendian:bool -> int -> int64 -> string
val marshal_int8 : int -> string
val marshal_int16 : int -> string
val marshal_int32 : int32 -> string
val marshal_int64 : int64 -> string
val unmarshal_int : ?bigendian:bool -> int -> string -> int64
val unmarshal_int8 : string -> int
val unmarshal_int16 : string -> int
val unmarshal_int32 : string -> int32
val unmarshal_int64 : string -> int64
val unmask : string -> string -> string
