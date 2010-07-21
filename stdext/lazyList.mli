(** A lazy-list *)

(** A forced lazy list element *)
type 'a elt = Empty | Cons of 'a * 'a t

(** A lazy list *)
and 'a t = 'a elt lazy_t

(** [map f xs] returns the list [f 1; f 2; ...; f n] *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [take n xs] returns the list truncated to the first [n] elements *)
val take : int -> 'a t -> 'a t

(** [iter f xs] applies every list element to [f] *)
val iter : ('a -> 'b) -> 'a t -> unit
