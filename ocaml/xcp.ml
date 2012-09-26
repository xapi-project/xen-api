type ('a, 'b) result =
	| Ok of 'a
	| Error of 'b

let error exn = Error exn

module type M = sig

  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

end
