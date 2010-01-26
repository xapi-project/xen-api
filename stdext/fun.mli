val const : 'a -> 'b -> 'a
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
val on : ('b -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'a -> 'c
val comp : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
