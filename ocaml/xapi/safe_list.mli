(** Ignore exceptions that can happen due to invalid references and just skip them *)

val map : ('a -> 'b) -> 'a list -> 'b list

val filter : ('a -> bool) -> 'a list -> 'a list

val flat_map : ('a -> 'b list) -> 'a list -> 'b list
