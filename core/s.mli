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
  val whoami: unit -> string

  module IO: sig
    include Cohttp.S.IO

    val map: ('a -> 'b) -> 'a t -> 'b t

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

  type t
  (** A listening server *)

  val listen: process:(string -> string io) -> switch:string -> queue:string -> unit -> [ `Ok of t | `Error of exn ] io

  val shutdown: t:t -> unit -> unit io
  (** [shutdown t] shutdown a server *)
end

module type CLIENT = sig
  type +'a io

  type t

  val connect: switch:string -> unit -> [ `Ok of t | `Error of exn ] io

  val disconnect: t:t -> unit -> unit io
  (** [disconnect] closes the connection *)

  val rpc: t:t -> queue:string -> ?timeout: int -> body:string -> unit -> [ `Ok of string | `Error of exn ] io

  val list: t:t -> prefix:string -> unit -> [ `Ok of string list | `Error of exn ] io

  val diagnostics: t:t -> unit -> [ `Ok of Protocol.Diagnostics.t | `Error of exn ] io

  val trace: t:t -> from:int64 -> ?timeout:float -> unit -> [ `Ok of Protocol.Out.trace | `Error of exn ] io

  val ack: t:t -> message:(string * int64) -> unit -> [ `Ok of unit | `Error of exn ] io

  val destroy: t:t -> queue:string -> unit -> [ `Ok of unit | `Error of exn ] io
  (** [destroy t queue_name] destroys the named queue, and all associated
      messages. *)

  val shutdown: t:t -> unit -> [ `Ok of unit | `Error of exn ] io
  (** [shutdown t] request that the switch shuts down *)
end
