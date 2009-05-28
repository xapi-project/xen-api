type t = {
  size : int;
  mutable current : int;
  data : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
}
val make : int -> float -> t
val length : t -> int
val push : t -> float -> unit
val peek : t -> int -> float
val top : t -> float
val iter_nb : t -> (float -> 'a) -> int -> unit
val raw_iter : t -> (float -> 'a) -> unit
val iter : t -> (float -> 'a) -> unit
val get_nb : t -> int -> float array
val get : t -> float array
