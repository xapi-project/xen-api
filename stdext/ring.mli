type 'a t = { size : int; mutable current : int; data : 'a array; }
val make : int -> 'a -> 'a t
val length : 'a t -> int
val push : 'a t -> 'a -> unit
val peek : 'a t -> int -> 'a
val top : 'a t -> 'a
val iter_nb : 'a t -> ('a -> 'b) -> int -> unit
val raw_iter : 'a t -> ('a -> unit) -> unit
val iter : 'a t -> ('a -> 'b) -> unit
val get_nb : 'a t -> int -> 'a array
val get : 'a t -> 'a array
