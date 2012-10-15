
module Message : sig
	type t = {
		payload: string; (* switch to Rpc.t *)
		correlation_id: int;
		reply_to: string option;
	}
	val t_of_rpc: Rpc.t -> t
	val rpc_of_t: t -> Rpc.t

	val one_way: string -> t
end

open Cohttp_lwt_unix

module In : sig
	type t =
	| Login of string            (** Associate this transport-level channel with a session *)
	| Bind of string option      (** Listen on either an existing queue or a fresh one *)
	| Send of string * Message.t (** Send a message to a queue *)
	| Transfer of string * float (** ACK up to a message, blocking wait for new messages *)

	val of_request: Request.t -> t option
	(** parse a [t] from an HTTP request *)

	val to_request: t -> Request.t
	(** print a [t] to an HTTP request *)
end

module Out : sig
	type transfer = {
		dropped: int;
		messages: (int64 * Message.t) list;
	}
	val transfer_of_rpc: Rpc.t -> transfer
	val rpc_of_transfer: transfer -> Rpc.t

	type t =
	| Login
	| Bind of string
	| Send
	| Transfer of transfer

	val to_response : t ->  (Response.t * Body.t) Lwt.t
end

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
