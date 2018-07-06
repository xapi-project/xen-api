(** Represents an active tapdisk instance *)
type tapdev

module Stats :
sig
  module Tap :
  sig
    type t = {
      minor : int;
      reqs : int64 * int64;
      kicks : int64 * int64;
    }
  end
  module Driver :
  sig
    type t = { ty : int; name : string; }
  end
  module Image :
  sig
    type t = {
      name : string;
      hits : int64 * int64;
      fail : int64 * int64;
      driver : Driver.t;
    }
  end
  type t = {
    name : string;
    secs : int64 * int64;
    images : Image.t list;
    tap : Tap.t;
    nbd_mirror_failed : int;
    reqs_outstanding : int;
  }
end

val tapdev_of_rpc : Rpc.t -> tapdev
val rpc_of_tapdev : tapdev -> Rpc.t

val get_minor : tapdev -> int
val get_tapdisk_pid : tapdev -> int

type t = tapdev * string * (string * string) option

type context
val create : unit -> context
val create_dummy : string -> context

type driver = Vhd | Aio
val string_of_driver : driver -> string

val get_devnode_dir : context -> string
val get_tapdevstem : context -> string

val allocate : context -> int
val devnode : context -> int -> string
val spawn : context -> int
val attach : context -> int -> int -> tapdev
val args : tapdev -> string list
val _open : context -> tapdev -> string -> driver -> unit
val close : context -> tapdev -> unit
val pause : context -> tapdev -> unit
val unpause : context -> tapdev -> string -> driver -> unit
val detach : context -> tapdev -> unit
val free : context -> int -> unit
val list : ?t:tapdev -> context -> t list
val is_paused : context -> tapdev -> bool
val is_active : context -> tapdev -> bool
val stats : context -> tapdev -> Stats.t

(** Thrown by [of_device x] when [x] is a device not owned by blktap *)
exception Not_blktap

(** Thrown by [of_device x] when [x] is not a device *)
exception Not_a_device

(** Given a path to a device, return the corresponding tap information *)
val of_device : context -> string -> t
