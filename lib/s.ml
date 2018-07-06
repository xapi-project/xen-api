(*
 * Copyright (C) 2013 Citrix Systems Inc
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
open Sexplib.Std

type msg = [ `Msg of string ]

module type BLOCK = Mirage_block_lwt.S

module type COMPARABLE = sig
  type t
  (** An item with a total ordering *)

  val compare: t -> t -> [ `LessThan | `Equal | `GreaterThan ]
  (** Compare two items *)
end

module type CSTRUCTABLE = sig
  type t
  (** Something that can be read from and written to a Cstruct.t *)

  val to_cstruct: t -> Cstruct.t
  val of_cstruct: Cstruct.t -> t option
end

type traced_operation = [
  | `Set of string * string * [ `Producer | `Consumer | `Suspend | `Suspend_ack ] * [ `Int64 of int64 | `Bool of bool ]
  | `Get of string * string * [ `Producer | `Consumer | `Suspend | `Suspend_ack ] * [ `Int64 of int64 | `Bool of bool ]
] [@@deriving sexp]
type traced_operation_list = traced_operation list [@@deriving sexp]

module type LOG = sig
  val debug : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val info : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val error : ('a, unit, string, unit Lwt.t) format4 -> 'a

  val trace: traced_operation list -> unit Lwt.t
end

module type RING = sig
  type t
  (* A ring containing variable-sized messages *)

  type disk
  (* A block device *)

  type item
  (* A message on the ring *)

  type error = [ `Retry | `Suspended | `Msg of string ]

  type 'a result = ('a, error) Result.result
  val pp_error : Format.formatter -> error -> unit
  val open_error : 'a result -> ('a, [> error]) Result.result
  val error_to_msg : 'a result -> ('a, msg) Result.result

  val attach: ?queue:string -> ?client:string -> disk:disk -> unit -> t result Lwt.t
  (** [attach queue client blockdevice] attaches to a previously-created shared ring on top
      of [blockdevice]. *)

  val detach: t -> unit Lwt.t
  (** [detach t] frees all resources associated with [t]. Attempts to use [t]
      after a detach will result in an [`Error _] *)

  val state: t -> [`Running | `Suspended] result Lwt.t
  (** [state t ()] queries the current state of the ring. If the result is
      `Suspended then the producer has acknowledged and will nolonger produce
      items. Clients which support suspend/resume should arrange to call this
      function periodically. *)

  val debug_info: t -> (string * string) list result Lwt.t
  (** [debug_info t] returns a list of key=value pairs which may be useful
      for debugging. Nothing should be assumed about the keys or the values;
      they should only be printed or logged. *)

  type position [@@deriving sexp_of]
  (** The position within a stream *)

  include COMPARABLE with type t := position

  val advance: t:t -> position:position -> unit -> unit result Lwt.t
  (** [advance t position] exposes the item associated with [position] to
      the Consumer so it can be [pop]ped. *)
end

module type PRODUCER = sig
  include RING

  val create: disk:disk -> unit -> unit result Lwt.t
  (** [create blockdevice] initialises a shared ring on top of [blockdevice]
      where we will be able to [push] variable-sized items. *)

  val push: t:t -> item:item -> unit -> position result Lwt.t
  (** [push t item] pushes [item] onto the ring [t] but doesn't expose it to
      the Consumer.
      [`Ok position] means the update has been safely written to the block device
      and can be exposed to the Consumer by calling [advance position].
      [`Error message] indicates some fatal software bug: the message should
      be logged and the process shutdown.
      [`Suspend] means that the consumer has requested that no more items
      be pushed onto the queue temporarily
      [`Retry] means that the item should fit but there is temporarily not
      enough space in the ring. The client should retry later. *)
end

module type CONSUMER = sig
  include RING

  val suspend: t -> unit result Lwt.t
  (** [suspend t] signals that the producer should stop pushing items.
      Note this function returns before the producer has acknowledged.
      The result `Retry means that a previous call to [resume] has not
      been acknowledged; the client should retry. *)

  val resume: t -> unit result Lwt.t
  (** [resume t] signals that a producer may again start pushing items.
      This call does not wait for an acknowledgement from the producer.
      Note it is not an error to resume an already-resumed queue.
      The result `Retry means that a previous call to [suspend] has not
      been acknowledged; the client should retry. *)

  val pop: t:t -> ?from:position -> unit -> (position * item) result Lwt.t
  (** [peek t ?position ()] returns a pair [(position, item)] where [item] is the
      next item on the ring after [from]. Repeated calls to [pop] will return the
      same [item].
      To indicate that the item has been processed, call [advance position].
      [`Retry] means there is no item available at the moment and the client should
      try again later. *)

  val fold: f:(item -> 'a -> 'a) -> t:t -> ?from:position -> init:'a -> unit -> (position * 'a) result Lwt.t
  (** [peek_all f t ?position init ()] folds [f] across all the values that can be
      immediately [peek]ed from the ring. If any of the [fold] operations fail
      then the whole operation fails. The successful result includes the final
      [position] which can be used to consume all the items at once. *)
end

module type CLOCK = sig
  include Mirage_types.MCLOCK
  val connect : unit -> t Lwt.t
end
module type TIME = Mirage_types_lwt.TIME

module type JOURNAL = sig
  type t
  (** A journal kept on disk of work we need to perform *)

  type disk
  (** The disk where we will keep our records. This should persist over a crash. *)

  type operation
  (** An idempotent operation which we will perform at-least-once *)

  type error = [ `Msg of string ]
  type 'a result = ('a, error) Result.result
  val pp_error : Format.formatter -> error -> unit
  val open_error : 'a result -> ('a, [> error]) Result.result
  val error_to_msg : 'a result -> ('a, msg) Result.result

  val start: ?name:string -> ?client:string -> ?flush_interval:int64 -> ?retry_interval:int64 -> disk -> (operation list -> unit result Lwt.t) -> t result Lwt.t
  (** Start a journal replay thread on a given disk, with the given processing
      function which will be applied at-least-once to every item in the journal.
      If a [flush_interval] is provided then every push will start a timer
      and the items will not be processed until the timer expires (or the journal
      becomes full) to encourage batching. The [retry_interval] gives the delay
      between re'perform'ing journalled items that fail. Default is 5 seconds. *)

  val shutdown: t -> unit Lwt.t
  (** Shut down a journal replay thread *)

  type waiter = {
    flush: unit -> unit;
    sync: unit -> unit Lwt.t
  }

  val push: t -> operation -> waiter result Lwt.t
  (** Append an operation to the journal. When this returns, the operation will
      be performed at-least-once before any later items are performed.
      If a client needs to wait for the operation to be completed then call
      the returned thunk and wait for the resulting thread. *)
end
