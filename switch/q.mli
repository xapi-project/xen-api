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

type t with sexp
(** a persistent message queue with a well-known name.
    XXX these aren't really queues as messages are removed
    from the middle *)

val contents: t -> (Protocol.message_id * Protocol.Entry.t) list
(** [contents t] returns the elements within a queue *)

val get_owner: t -> string option
(** [get_owner t] returns the owner of [t] where the owner is the entity
    which, when it is destroyed, the queue is also cleaned up. *)

type queues with sexp
(** A set of message queues *)

val empty: queues

module StringSet : Set.S with type elt = string

val owned_queues: queues -> string -> StringSet.t
(** [owned_queues owner] returns a list of queue names owned by [owner] *)

module Directory : sig

  val add: queues -> ?owner:string -> string -> queues
  (** [add name] adds an empty queue with name [name].
      If we want to tie the queue lifetime to an "owner" then
      supply the owner's name. *)

  val remove: queues -> string -> queues
  (** [remove name] removes the queue with name [name] *)

  val find: queues -> string -> t
  (** [find name] returns the queue with name [name].
      	    XXX should we switch to an option type? *)

  val list: queues -> string -> string list
  (** [list prefix] returns the names of non-empty queues whose
      	    names have prefix [prefix] *)
end

val queue_of_id: Protocol.message_id -> string
(** [queue_of_id id] returns the name of the queue containing
    message id [id] *)

val ack: queues -> Protocol.message_id -> queues
(** [ack id] removes message [id] from whichever queue it is in *)

val transfer: queues -> int64 -> string list -> (Protocol.message_id * Protocol.Message.t) list
(** [transfer from names] returns all messages which are newer
    than [from] from all queues in [names] *)

val wait: queues -> int64 -> float -> string list -> unit Lwt.t
(** [wait queues from timeout names] returns a thread which blocks until a message
    newer than [from] is added to any of the queues with names [names]. *)

val entry: queues -> Protocol.message_id -> Protocol.Entry.t option
(** [entry id] returns the entry containing message id [id] *)

val send: queues -> Protocol.origin -> string -> Protocol.Message.t -> (queues * Protocol.message_id) option Lwt.t
(** [send origin name payload] sends a message with contents
    [payload] to the queue with name [name] *)
