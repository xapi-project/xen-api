
module Delay :
  sig
    type t
    val make : unit -> t
    exception Pre_signalled
    val wait : t -> float -> bool
    val signal : t -> unit
  end

type t

val global_scheduler : t

type time = Absolute of int64 | Delta of int

val now : unit -> int64

module Dump :
  sig
    type u = { time : int64; thing : string; }
    type dump = u list
    val rpc_of_dump : dump -> Rpc.t
    val dump_of_rpc : Rpc.t -> dump
    val make : t -> dump
  end

val one_shot : t -> time -> string -> (unit -> unit) -> int64 * int

val cancel : t -> int64 * int -> unit

val process_expired : t -> bool

val main_loop : t -> unit

val start : unit -> unit
