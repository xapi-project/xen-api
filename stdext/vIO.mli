exception End_of_file
exception Timeout

type t = {
	read : string -> int -> int -> int;
	write : string -> int -> int -> int;
	input_line : (?timeout: float option -> unit -> string) option;
	flush : unit -> unit;
	close : unit -> unit;
	is_raw : bool;
	selectable : Unix.file_descr option;
}

val read : ?timeout: float option -> t -> string -> int -> int -> int
val write : ?timeout: float option -> t -> string -> int -> int -> int
val read_string : ?timeout: float option -> t -> int -> string
val write_string : ?timeout: float option -> t -> string -> unit
val input_line : ?timeout: float option -> t -> string
val flush : t -> unit
val close : t -> unit
