(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms, 
with or without modification, are permitted provided 
that the following conditions are met:

*   Redistributions of source code must retain the above 
    copyright notice, this list of conditions and the 
    following disclaimer.
*   Redistributions in binary form must reproduce the above 
    copyright notice, this list of conditions and the 
    following disclaimer in the documentation and/or other 
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.
*)

exception Queue_deleted of string

type message_id = string * int64
(** uniquely identifier for this message *)

val rpc_of_message_id: message_id -> Rpc.t
val message_id_of_rpc: Rpc.t -> message_id

val rpc_of_message_id_opt: message_id option -> Rpc.t
val message_id_opt_of_rpc: Rpc.t -> message_id option

module Message : sig
	type kind =
	| Request of string
	| Response of message_id 
	type t = {
		payload: string; (* switch to Rpc.t *)
		kind: kind;
	}
	val t_of_rpc: Rpc.t -> t
	val rpc_of_t: t -> Rpc.t
end

module Event : sig
	type message =
		| Message of message_id * Message.t
		| Ack of message_id

	type t = {
		time: float;
		input: string option;
		queue: string;
		output: string option;
		message: message;
		processing_time: int64 option;
	}
	val t_of_rpc: Rpc.t -> t
	val rpc_of_t: t -> Rpc.t
end

module In : sig
	type transfer = {
		from: string option;
		timeout: float;
		queues: string list;
	}

	type t =
	| Login of string            (** Associate this transport-level channel with a session *)
	| CreatePersistent of string (** Create a persistent named queue *)
	| CreateTransient of string  (** Create a transient named queue which will be deleted when the client disconnects *)
	| Destroy of string          (** Destroy a named queue *)
	| Send of string * Message.t (** Send a message to a queue *)
	| Transfer of transfer       (** blocking wait for new messages *)
	| Trace of int64 * float     (** blocking wait for trace data *)
	| Ack of message_id          (** ACK this particular message *)
	| List of string             (** return a list of queue names with a prefix *)
	| Diagnostics                (** return a diagnostic dump *)
	| Get of string list         (** return a web interface resource *)

	val rpc_of_t : t -> Rpc.t
	val t_of_rpc : Rpc.t -> t

	val headers: string -> Cohttp.Header.t

	val of_request: (string option) -> Cohttp.Code.meth -> string -> t option
	(** parse a [t] from an HTTP request and body  *)

	val to_request: t -> (string option) * Cohttp.Code.meth * Uri.t
	(** print a [t] to an HTTP request and body *)
end

type origin =
	| Anonymous of int (** An un-named connection, probably a temporary client connection *)
	| Name of string   (** A service with a well-known name *)
(** identifies where a message came from *)

module Entry : sig
	type t = {
		origin: origin;
		time: int64; (** ns *)
		message: Message.t;
	}
	(** an enqueued message *)

	val make: int64 -> origin -> Message.t -> t
end

module Diagnostics : sig
	type queue_contents = (message_id * Entry.t) list

	type queue = {
		next_transfer_expected: int64 option;
		queue_contents: queue_contents;
	}

	type t = {
		start_time: int64;
		current_time: int64;
		permanent_queues: (string * queue) list;
		transient_queues: (string * queue) list;
	}
	val rpc_of_t: t -> Rpc.t
	val t_of_rpc: Rpc.t -> t
end


module Out : sig
	type transfer = {
		messages: (message_id * Message.t) list;
		next: string;
	}
	val transfer_of_rpc: Rpc.t -> transfer
	val rpc_of_transfer: transfer -> Rpc.t

	type trace = {
		events: (int64 * Event.t) list;
	}
	val trace_of_rpc: Rpc.t -> trace
	val rpc_of_trace: trace -> Rpc.t

	val string_list_of_rpc: Rpc.t -> string list
	val rpc_of_string_list: string list -> Rpc.t

	type t =
	| Login
	| Create of string
	| Destroy
	| Send of message_id option
	| Transfer of transfer
	| Trace of trace
	| Ack
	| List of string list
	| Diagnostics of Diagnostics.t
	| Not_logged_in
	| Get of string

	val to_response : t -> Cohttp.Code.status_code * string
end

type ('a, 'b) result =
| Ok of 'a
| Error of 'b

exception Failed_to_read_response

exception Unsuccessful_response

module Connection(IO: Cohttp.IO.S) : sig
	val rpc: (IO.ic * IO.oc) -> In.t -> (string, exn) result IO.t
end

module Server(IO: Cohttp.IO.S) : sig
	val listen: (string -> string IO.t) -> (IO.ic * IO.oc) -> string -> unit IO.t
end

