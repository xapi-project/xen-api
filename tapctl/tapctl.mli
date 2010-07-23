(** Represents an active tapdisk instance *)
type tapdev
val tapdev_of_rpc : Rpc.t -> tapdev
val rpc_of_tapdev : tapdev -> Rpc.t

type t = tapdev * string * (string * string) option

type context
val create : unit -> context

type driver = Vhd | Aio
val string_of_driver : driver -> string

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

(** Given a path to a device, return the corresponding tap information *)
val of_device : context -> string -> t
