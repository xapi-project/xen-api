val run_after : seconds:int -> (unit -> unit) -> int64 * int

val cancel : int64 * int -> unit

val start : unit -> unit
