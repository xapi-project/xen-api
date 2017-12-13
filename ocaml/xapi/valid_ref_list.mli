(** Ignore exceptions that can happen due to invalid references and just skip them *)

val exists : ('a -> bool) -> 'a list -> bool

val filter : ('a -> bool) -> 'a list -> 'a list

val for_all : ('a -> bool) -> 'a list -> bool

val map : ('a -> 'b) -> 'a list -> 'b list

val iter : ('a -> unit) -> 'a list -> unit

val flat_map : ('a -> 'b list) -> 'a list -> 'b list

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** [filter_map f l] applies [f] to each element of [l], and skips the elements
    of [l] for which [f] returns [None], and keeps the output [x] when [f]
    returns [Some x]. *)
