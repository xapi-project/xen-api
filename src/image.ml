module Opt = struct
	let default d = function
		| None -> d
		| Some x -> x
end

let startswith prefix x =
	let prefix' = String.length prefix
	and x' = String.length x in
	prefix' <= x' && (String.sub x 0 prefix' = prefix)

type t = [
	| `Vhd of string
	| `Raw of string
]

let to_string = function
	| `Vhd x -> "vhd:" ^ x
	| `Raw x -> "raw:" ^ x

let of_device path =
	try 
		match Tapctl.of_device (Tapctl.create ()) path with
		| _, _, (Some ("vhd", vhd)) -> Some (`Vhd vhd)
		| _, _, (Some ("aio", vhd)) -> Some (`Raw vhd)
		| _, _, _ -> raise Not_found
	with Tapctl.Not_blktap ->
		None
	| Tapctl.Not_a_device ->
		None
	| _ -> 
		None
