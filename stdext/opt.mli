val iter : ('a -> unit) -> 'a option -> unit
val map : ('a -> 'b) -> 'a option -> 'b option
val default : 'a -> 'a option -> 'a
val unbox : 'a option -> 'a
val is_boxed : 'a option -> bool
val to_list : 'a option -> 'a list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
