open Ctypes
open Foreign

let bytes = foreign "bytes" (ocaml_bytes @-> uint64_t @-> returning uint64_t)
let file = foreign "file" (string @-> returning uint64_t)

let to_hex hash =
	Printf.sprintf "%016LX" hash;;

let bytes buff = 
	let size = Unsigned.UInt64.of_int (Bytes.length buff) in
	Unsigned.UInt64.to_int64(bytes (ocaml_bytes_start buff) size);;

let file name = 
	close_in (open_in_bin name); (* To emulate the behaviour of the Digest class, also throws an error if the file does not exist *)
	Unsigned.UInt64.to_int64(file name);; (* We know the file exists at this point *)
