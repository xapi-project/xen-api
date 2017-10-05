(** Ignore exceptions that can happen due to invalid references and just skip them *)

val exists : ('a -> bool) -> 'a list -> bool

val filter : ('a -> bool) -> 'a list -> 'a list

val for_all : ('a -> bool) -> 'a list -> bool

val map : ('a -> 'b) -> 'a list -> 'b list

val flat_map : ('a -> 'b list) -> 'a list -> 'b list
