type t
val make : unit -> t
val length : t -> int64
val append_substring : t -> string -> int -> int -> unit
val to_fct : t -> (string -> unit) -> unit
val to_string : t -> string
val to_stream : t -> out_channel -> unit
