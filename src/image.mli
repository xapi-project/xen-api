
type t = [
	| `Vhd of string
    | `Raw of string
    | `Nbd of (string * string)
]
(** An image may either be backed by a vhd-format file or a
    raw-format file. *)

val to_string: t -> string
(** Pretty-print the image *)

val of_device: string -> t option
(** Examine the provided path and return the backing image, or
    None if one doesn't exist. *)
