open Protocol

module IO : sig
	type ic
	type oc

	val connect : int -> (ic * oc)
	(** [connect port] connects to a switch listening on [port] *)
end

module Connection : sig
	val rpc: (IO.ic * IO.oc) -> In.t -> (string, exn) result
end

module Client : sig
	type t

	val connect: int -> string -> t

	val rpc: t -> string  -> string
end

module Server : sig

	val listen: (string -> string) -> (IO.ic * IO.oc) -> string -> string -> unit
end
