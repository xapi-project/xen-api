type t = { name: string;
	   parts: part list }

and part = 
  | Hole of string
  | String of string

val string_of: (string * string) list -> t -> string

val output_fmt : Format.formatter -> t -> unit
