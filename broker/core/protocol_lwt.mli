open Protocol

module Connection : sig
	type t
	(** represents an open transport-level connection *)

	val make: int -> string -> t Lwt.t
	(** [make port token] connects to a switch listening on [port] and
		associates with a session identified by [token] *)

	exception Failed_to_read_response

	exception Unsuccessful_response

	val rpc: t -> In.t -> string Lwt.t
end

module Client : sig
	type t

	val connect: int -> t Lwt.t

	val rpc: t -> string -> string -> string Lwt.t
end

module Server : sig

	val listen: (string -> string Lwt.t) -> int -> string -> 'a Lwt.t
end
