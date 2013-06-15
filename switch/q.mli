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

type t
(** a persistent message queue with a well-known name.
    XXX these aren't really queues as messages are removed
    from the middle *)

val contents: t -> (int64 * Protocol.Entry.t) list
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

val queue_of_id: int64 -> string option
(** [queue_of_id id] returns the name of the queue containing
    message id [id] *)

val ack: int64 -> unit
(** [ack id] removes message [id] from whichever queue it is in *)

val transfer: int64 -> string list -> (int64 * Protocol.Message.t) list
(** [transfer from names] returns all messages which are newer
    than [from] from all queues in [names] *)

val wait: int64 -> string -> unit Lwt.t
(** [wait from name] returns a thread which blocks until a message
    newer than [from] is added to queue with name [name] *)

val entry: int64 -> Protocol.Entry.t option
(** [entry id] returns the entry containing message id [id] *)

val send: Protocol.origin -> string -> Protocol.Message.t -> int64 option Lwt.t
(** [send origin name payload] sends a message with contents
    [payload] to the queue with name [name] *)
