type t =
    Node of t list
  | Symbol of string
  | String of string
  | WeirdString of string * string

val mkstring : string -> t

val string_of : t -> string

val weird_of_string : string -> t

val output_fmt : Format.formatter -> t -> unit
