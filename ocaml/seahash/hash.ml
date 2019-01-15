open Ctypes
open Foreign

let bytes = foreign "hash_bytes" (ocaml_bytes @-> uint64_t @-> returning uint64_t)
let file = foreign "hash_filedesc" (int @-> returning uint64_t)

let fd_of_int (x: int) : Unix.file_descr = Obj.magic x;;
let int_of_fd (x: Unix.file_descr) : int = Obj.magic x;;

let to_hex hash =
	Printf.sprintf "%016LX" hash;;

let bytes buff = 
	let size = Unsigned.UInt64.of_int (Bytes.length buff) in
	Unsigned.UInt64.to_int64(bytes (ocaml_bytes_start buff) size);;

let file file_name = 
	if Sys.is_directory file_name || not(Sys.file_exists file_name)
	then raise Not_found
	else begin
		let fd = Unix.openfile file_name [O_RDONLY] 0o666 in
		let csum = Unsigned.UInt64.to_int64(file (int_of_fd fd)) in
		
		if csum = 0L
		then raise End_of_file (* Unexpected EOF *)
		else begin
			Unix.close fd;
			csum
		end
	end
