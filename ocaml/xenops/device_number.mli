type spec = 
	| Xen of int * int
	| Scsi of int * int
	| Ide of int * int

type interface

val make: spec -> interface

(** Given an interface, return an integer suitable for xenstore *)
val int_of_interface: interface -> int

(** Given an integer, return an interface *)
val interface_of_int: int -> interface

(** Given an interface, return a possible string representation *)
val string_of_interface: interface -> string

(** Given a string, return an interface *)
val interface_of_string: string -> interface

