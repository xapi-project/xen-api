(*
 * Copyright (c) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t
(** a persistent message queue with a well-known name.
    XXX these aren't really queues as messages are removed
    from the middle *)

val contents: t -> (Protocol.message_id * Protocol.Entry.t) list
(** [contents t] returns the elements within a queue *)

module Directory : sig

	val add: string -> unit
	(** [add name] adds an empty queue with name [name] *)

	val remove: string -> unit
	(** [remove name] removes the queue with name [name] *)

	val find: string -> t
	(** [find name] returns the queue with name [name].
	    XXX should we switch to an option type? *)

	val list: string -> string list
	(** [list prefix] returns the names of non-empty queues whose
	    names have prefix [prefix] *)
end

val queue_of_id: Protocol.message_id -> string
(** [queue_of_id id] returns the name of the queue containing
    message id [id] *)

val ack: Protocol.message_id -> unit
(** [ack id] removes message [id] from whichever queue it is in *)

val transfer: int64 -> string list -> (Protocol.message_id * Protocol.Message.t) list
(** [transfer from names] returns all messages which are newer
    than [from] from all queues in [names] *)

val wait: int64 -> string -> unit Lwt.t
(** [wait from name] returns a thread which blocks until a message
    newer than [from] is added to queue with name [name] *)

val entry: Protocol.message_id -> Protocol.Entry.t option
(** [entry id] returns the entry containing message id [id] *)

val send: Protocol.origin -> string -> Protocol.Message.t -> Protocol.message_id option Lwt.t
(** [send origin name payload] sends a message with contents
    [payload] to the queue with name [name] *)
