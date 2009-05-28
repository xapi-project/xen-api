type t = {
	sz: int;
	data: string;
	mutable prod: int;
	mutable cons: int;
	mutable pwrap: bool;
}

exception Data_limit
exception Full

val make : int -> t

val to_consume : t -> int
val to_fill : t -> int

val is_full : t -> bool
val is_empty : t -> bool

val consume : t -> int -> string
val consume_all : t -> string
val skip : t -> int -> unit

val feed_data : t -> string -> unit
val read_search : t -> (string -> int -> int -> int)
                    -> (string -> int -> int -> unit) -> int
                    -> int
val search : t -> char -> int
