
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

module In : sig
	type t =
	| Login of string            (** Associate this transport-level channel with a session *)
	| Create of string option    (** Create a queue with a well-known or fresh name *)
	| Subscribe of string        (** Subscribe to messages from a queue *)
	| Send of string * Message.t (** Send a message to a queue *)
	| Transfer of int64 * float  (** blocking wait for new messages *)
	| Ack of int64               (** ACK this particular message *)
	| Diagnostics                (** return a diagnostic dump *)

	val rpc_of_t : t -> Rpc.t
	val t_of_rpc : Rpc.t -> t

	val headers: string -> Cohttp.Header.t

	val of_request: (string option) -> Cohttp.Code.meth -> string -> t option
	(** parse a [t] from an HTTP request and body  *)

	val to_request: t -> (string option) * Cohttp.Code.meth * Uri.t
	(** print a [t] to an HTTP request and body *)
end

module Out : sig
	type transfer = {
		messages: (int64 * Message.t) list;
	}
	val transfer_of_rpc: Rpc.t -> transfer
	val rpc_of_transfer: transfer -> Rpc.t

	type t =
	| Login
	| Create of string
	| Subscribe
	| Send
	| Transfer of transfer
	| Ack
	| Diagnostics of string
	| Not_logged_in

	val to_response : t -> Cohttp.Code.status_code * string
end

