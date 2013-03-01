val code : string
val padding : char
val of_char : char -> int
val to_char : int -> char
val fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a
val explode : string -> char list
val implode : char list -> string
val strip_whitespace : string -> string
val decode : string -> string
val encode : string -> string
val test : string -> unit
val tests : string list

