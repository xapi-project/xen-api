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

open Message_switch_core

type t [@@deriving sexp]
(** a persistent message queue with a well-known name.
    XXX these aren't really queues as messages are removed
    from the middle *)

val contents: t -> (Protocol.message_id * Protocol.Entry.t) list
(** [contents t] returns the elements within a queue *)

val get_owner: t -> string option
(** [get_owner t] returns the owner of [t] where the owner is the entity
    which, when it is destroyed, the queue is also cleaned up. *)

type queues [@@deriving sexp]
(** A set of message queues *)

val empty: queues

module StringSet : Set.S with type elt = string

val owned_queues: queues -> string -> StringSet.t
(** [owned_queues owner] returns a list of queue names owned by [owner] *)

module Op : sig
  type t

  val of_cstruct: Cstruct.t -> t option
  val to_cstruct: t -> Cstruct.t
end

val do_op: queues -> Op.t -> queues

module Directory : sig

  val add: queues -> ?owner:string -> string -> Op.t

  val remove: queues -> string -> Op.t

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

val ack: queues -> Protocol.message_id -> Op.t

val transfer: queues -> int64 -> string list -> (Protocol.message_id * Protocol.Message.t) list
(** [transfer from names] returns all messages which are newer
    than [from] from all queues in [names] *)

val entry: queues -> Protocol.message_id -> Protocol.Entry.t option
(** [entry id] returns the entry containing message id [id] *)

val send: queues -> Protocol.origin -> string -> Protocol.Message.t -> (Protocol.message_id * Op.t) option
