type t

val create : caller_uuid:string -> t
(** [create ~caller_uuid] creates a fresh statistics record with zero counts
    and a [last_called] of [Mtime.Span.zero]. *)

val register_call : token_amount:float -> t -> unit
(** Track that a client has made a call *)

val get_uuid : t -> string

val get_call_count : t -> int

val get_token_count : t -> float
