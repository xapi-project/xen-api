(** The Delay module here implements simple cancellable delays. *)
module Delay : sig
  type t

  val make : unit -> t
  (** Makes a Delay.t *)

  val wait : t -> float -> bool
  (** Wait for the specified amount of time. Returns true if we waited the full
      length of time, false if we were woken *)

  val signal : t -> unit
  (** Signal anyone currently waiting with the Delay.t *)
end

(** The type of a scheduler *)
type t

(** The handle for referring to an item that has been scheduled *)
type handle

val rpc_of_handle : handle -> Rpc.t

val handle_of_rpc : Rpc.t -> handle

val make : unit -> t
(** Creates a scheduler *)

(** Items can be scheduled as a delta measured in seconds from now. *)
type time = Delta of int

(** This module is for dumping the state of a scheduler *)
module Dump : sig
  type u = {time: int64; thing: string}

  type dump = u list

  val rpc_of_dump : dump -> Rpc.t

  val dump_of_rpc : Rpc.t -> dump

  val make : t -> dump
end

val one_shot : t -> time -> string -> (unit -> unit) -> handle
(** Insert a one-shot item into the scheduler. *)

val cancel : t -> handle -> unit
(** Cancel an item *)

(** The PipeDelay module here implements simple cancellable delays. *)
module PipeDelay : sig
  type t

  val make : unit -> t
  (** Makes a PipeDelay.t *)

  val wait : t -> float -> bool
  (** Wait for the specified amount of time. Returns true if we waited the full
      length of time, false if we were woken *)

  val signal : t -> unit
  (** Signal anyone currently waiting with the PipeDelay.t *)
end
