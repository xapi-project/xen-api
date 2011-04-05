type spec = 
	| Xen of int * int
	| Scsi of int * int
	| Ide of int * int

type interface

val make: spec -> interface

(** [debug_string_of_interface i] returns a pretty-printed interface *)
val debug_string_of_interface: interface -> string

(** [string_of_interface i] returns a possible string representation of interface [i] *)
val string_of_interface: interface -> string

(** [interface_of_string x] returns the interface corresponding to string [x] *)
val interface_of_string: string -> interface

type xenstore_key = string

(** [xenstore_key_of_interface i] returns the xenstore key from interface [i] *)
val xenstore_key_of_interface: interface -> xenstore_key

(** [interface_of_xenstore_key key] returns an interface from a xenstore key *)
val interface_of_xenstore_key: xenstore_key -> interface

type disk_number = int

(** [disk_number_of_interface i] returns the corresponding non-negative disk number *)
val disk_number_of_interface: interface -> disk_number

(** [interface_of_disk_number hvm n] returns the interface corresponding to disk 
	number [n] which depends on whether the guest is [hvm] or not. *)
val interface_of_disk_number: bool -> disk_number -> interface

