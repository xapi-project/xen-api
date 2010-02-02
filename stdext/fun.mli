val const : 'a -> 'b -> 'a
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
val on : ('b -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'a -> 'c
val comp : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val (+++) : ('c -> 'd) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'd
val (++) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val ($) : ('a -> 'b) -> 'a -> 'b