type t

val rpc_of_t : t -> Rpc.t
val t_of_rpc : Rpc.t -> t
(** Checks whether a string is a valid tag string.
    Tag character set: A-Za-z0-9_+.-
    Can't start with hyphen. Max length is 128.
    Empty tags are currently not allowed. *)
val is_valid : string -> bool
(** Creates a tag from a string. Fails on non-conforming strings. *)
val of_string : string -> t
(** Converts a tag to a string. *)
val string_of : t -> string
