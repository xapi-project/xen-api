open Protocol

val whoami : unit -> string

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

	val rpc: t -> ?timeout:int -> string  -> string

	val list: t -> string -> string list
end

module Server : sig

	val listen: (string -> string) -> (IO.ic * IO.oc) -> string -> unit
end
