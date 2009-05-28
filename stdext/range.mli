type t

(** Make a range. *)
val make : int -> int -> t

(** Extract the start and end of the given range. *)
val get : t -> int * int

(** Test the given int for membership in the given range. *)
val mem : int -> t -> bool

(** Fold over a range, starting at the smallest int. *)
val fold_left : ('a -> int -> 'a) -> 'a -> t -> 'a

(** Fold over a range, starting at the largest int. *)
val fold_right : (int -> 'a -> 'a) -> t -> 'a -> 'a

(** Convert a range to a list of ascending integers *)
val to_list : t -> int list

