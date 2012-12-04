open Protocol

module IO : sig
	type ic
	type oc

	val connect : int -> (ic * oc) Lwt.t
	(** [connect port] connects to a switch listening on [port] *)
end

module Connection : sig
	val rpc: (IO.ic * IO.oc) -> In.t -> (string, exn) result Lwt.t
end

module Client : sig
	type t

	val connect: int -> string -> t Lwt.t

	val rpc: t -> string  -> string Lwt.t
end

module Server : sig

	val listen: (string -> string Lwt.t) -> (IO.ic * IO.oc) -> string -> string -> unit Lwt.t
end
