
(** The Delay module here implements simple cancellable delays. *)
module Delay :
  sig
    type t

    (** Makes a Delay.t *)
    val make : unit -> t

    (** Wait for the specified amount of time. Returns true if we waited
        the full length of time, false if we were woken *)
    val wait : t -> float -> bool

    (** Signal anyone currently waiting with the Delay.t *)
    val signal : t -> unit
  end

(** The type of a scheduler *)
type t

(** The handle for referring to an item that has been scheduled *)
type handle
val rpc_of_handle : handle -> Rpc.t
val handle_of_rpc : Rpc.t -> handle

(** Creates a scheduler *)
val make : unit -> t

(** Items can be scheduled at an absolute time (measured in seconds since
    unix epoch) or as a delta measured in for seconds from now. *)
type time = Absolute of int64 | Delta of int

(** Useful for Absolutely scheduled items *)
val now : unit -> int64

(** This module is for dumping the state of a scheduler *)
module Dump :
  sig
    type u = { time : int64; thing : string; }
    type dump = u list
    val rpc_of_dump : dump -> Rpc.t
    val dump_of_rpc : Rpc.t -> dump
    val make : t -> dump
  end

(** Insert a one-shot item into the scheduler. *)
val one_shot : t -> time -> string -> (unit -> unit) -> handle

(** Cancel an item *)
val cancel : t -> handle -> unit

(** shutdown a scheduler. Any item currently scheduled will not
    be executed. The scheduler cannot be restarted. *)
val shutdown : t -> unit
