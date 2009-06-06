
type 'a t

val make : unit -> 'a t
val null : 'a t
val string_of : 'a t -> string
val of_string : string -> 'a t

val make_dummy : string -> 'a t
val is_dummy : 'a t -> bool
val name_of_dummy : 'a t -> string
val pretty_string_of_dummy : 'a t -> string

val really_pretty_and_small : 'a t -> string
