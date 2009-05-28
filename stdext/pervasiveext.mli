val exnhook : (exn -> unit) option ref
val finally : (unit -> 'a) -> (unit -> 'b) -> 'a
val maybe_with_default : 'b -> ('a -> 'b) -> 'a option -> 'b
val may : ('a -> 'b) -> 'a option -> 'b option
val default : 'a -> 'a option -> 'a
val maybe : ('a -> unit) -> 'a option -> unit
val reraise_if : bool -> (unit -> unit) -> unit
val ignore_exn : (unit -> unit) -> unit
val ignore_int : int -> unit
val ignore_int32 : int32 -> unit
val ignore_int64 : int64 -> unit
val ignore_string : string -> unit
val ignore_float : float -> unit
val ignore_bool : bool -> unit
