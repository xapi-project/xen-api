(* From http://www.xmlrpc.com/spec *)
type t = 
	| Int of int
	| Bool of bool
	| String of string
	| Double of float
	| Struct of (string * t) list
	| Array of t list
