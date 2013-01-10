(** A module to represent sets of elements as (start, length) pairs. *)

(** Elements must be 'Numbers': *)
module type Number = sig 
	type t 
	val zero: t
	val add : t -> t -> t 
	val sub : t -> t -> t 

end

(** Representation of a Set *)
module ExtentlistSet: functor (A : Number) -> sig
	type extent = A.t * A.t
	type t

	val empty : t

	val union : t -> t -> t
	val intersection : t -> t -> t
	val difference : t -> t -> t

	val of_list : extent list -> t
	val to_list : t -> extent list
	val fold_left : ('a -> extent -> 'a) -> 'a -> t -> 'a
end

