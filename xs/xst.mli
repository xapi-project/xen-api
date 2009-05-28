type ops = {
	directory : string -> string list;
	read : string -> string;
	readv : string -> string list -> string list;
	write : string -> string -> unit;
	writev : string -> (string * string) list -> unit;
	mkdir : string -> unit;
	rm : string -> unit;
	getperms : string -> Xsraw.perms;
	setperms : string -> Xsraw.perms -> unit;
	setpermsv : string -> string list -> Xsraw.perms -> unit;
}

val get_operations : int -> Xsraw.con -> ops
val transaction : Xsraw.con -> (ops -> 'a) -> 'a
