module Result = struct
	type ('a, 'b) t =
		| Ok of 'a
		| Error of 'b

	let bind t f = match t with
		| Error e -> Error e
		| Ok x -> f x

	let (>>=) = bind

	let return x = Ok x

	let fail e = Error e

end

type ('a, 'b) t = ('a, 'b) Result.t

let error exn = Error exn

module type M = sig

  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

end

exception Unknown_method of string
