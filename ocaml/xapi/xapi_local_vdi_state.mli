val activate : string -> bool -> unit
val deactivate : string -> unit
val clear : unit -> unit
val iter : (bool -> string -> unit) -> unit
val fold : (bool -> string -> 'a -> 'a) -> 'a -> 'a
