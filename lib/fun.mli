val const : 'a -> 'b -> 'a
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
val on : ('b -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'a -> 'c
val comp : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val comp2 : ('b -> 'c) -> ('a1 -> 'a2 -> 'b) -> ('a1 -> 'a2 -> 'c)
val (+++) : ('c -> 'd) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'd
val (++) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Forward pipe operator: facilitates left-to-right function composition. *)
val (|>) : 'a -> ('a -> 'b) -> 'b
val ($) : ('a -> 'b) -> 'a -> 'b
