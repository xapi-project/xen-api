
(** decode a string encoded in base64. Will leave trailing NULLs on the string
    padding it out to a multiple of 3 characters *)
val decode: string -> string

(** encode a string into base64 *)
val encode: string -> string
