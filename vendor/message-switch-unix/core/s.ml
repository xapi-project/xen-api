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

module type BACKEND = sig
  (** This is the input type for the Client/Server functor: implement this
      and you can create a Client and a Server module *)

  val whoami: unit -> string
  (** An identifier for this process: my temporary queue names will be
      based on this identifier *)

  module IO: sig
    include Cohttp.S.IO

    val map: ('a -> 'b) -> 'a t -> 'b t

    val iter: ('a -> unit t) -> 'a list -> unit t

    val any: 'a t list -> 'a t

    val is_determined: 'a t -> bool
  end

  val connect: string -> (IO.ic * IO.oc) IO.t

  val disconnect: (IO.ic * IO.oc) -> unit IO.t

  module Ivar : sig
    type 'a t

    val create: unit -> 'a t

    val fill: 'a t -> 'a -> unit

    val read: 'a t -> 'a IO.t
  end

  module Mutex : sig
    type t

    val create: unit -> t

    val with_lock: t -> (unit -> 'a IO.t) -> 'a IO.t
  end

  module Clock : sig
    type timer

    val run_after: int -> (unit-> unit) -> timer

    val cancel: timer -> unit
  end
end

module type SERVER = sig
  type +'a io

  type error = [
    | `Failed_to_read_response
    | `Unsuccessful_response
  ]

  type 'a result = ('a, [ `Message_switch of error ]) Mresult.result

  val pp_error : Format.formatter -> [ `Message_switch of error ] -> unit
  val error_to_msg : 'a result -> ('a, [ `Msg of string ]) Mresult.result

  type t
  (** A listening server *)

  val listen: process:(string -> string io) -> switch:string -> queue:string -> unit -> t result io
  (** Connect to [switch] and start processing messages on [queue] via function
      [process] *)

  val shutdown: t:t -> unit -> unit io
  (** [shutdown t] shutdown a server *)
end

module type CLIENT = sig
  type +'a io

  type t

  type error = [
    | `Failed_to_read_response
    | `Unsuccessful_response
    | `Timeout
    | `Queue_deleted of string
  ]

  type 'a result = ('a, [ `Message_switch of error ]) Mresult.result

  val pp_error : Format.formatter -> [ `Message_switch of error ] -> unit
  val error_to_msg : 'a result -> ('a, [ `Msg of string ]) Mresult.result

  val connect: switch:string -> unit -> t result io
  (** [connect switch] connects to a named switch *)

  val disconnect: t:t -> unit -> unit io
  (** [disconnect] closes the connection *)

  val rpc: t:t -> queue:string -> ?timeout: int -> body:string -> unit -> string result io
  (** Performs a remote-procedure call via destinatino queue [queue] with
      payload [body]. If a [timeout] is provided then the call will return before
      the timeout expires *)

  val list: t:t -> prefix:string -> ?filter:[`All | `Alive] -> unit -> string list result io
  (** List queues on the switch starting with string [prefix] *)

  val diagnostics: t:t -> unit -> Protocol.Diagnostics.t result io
  (** Download a diagnostics dump from the switch *)

  val trace: t:t -> ?from:int64 -> ?timeout:float -> unit -> Protocol.Out.trace result io
  (** Download event trace records from the server. The trace starts from
      the earliest recorded, unless overridden with a newer message id using
      the [from] parameter. If a [timeout] is given then the call will return
      before the timeout expires. *)

  val ack: t:t -> message:Protocol.message_id -> unit -> unit result io
  (** Acknowledge the processing has completed of a particular message. The
      switch will purge the message from the queue. Note it is not necessary
      to acknowledge messages in any particular order. *)

  val destroy: t:t -> queue:string -> unit -> unit result io
  (** [destroy t queue_name] destroys the named queue, and all associated
      messages. *)

  val shutdown: t:t -> unit -> unit result io
  (** [shutdown t] request that the switch shuts down *)
end
