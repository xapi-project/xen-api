(* read size bytes and return the completed buffer *)
let read fd size =
	let buf = String.create size in
	let i = ref size in
	while !i <> 0
	do
		let rd = Unix.read fd buf (size - !i) !i in
		if rd <= 0 then raise End_of_file;
		i := !i - rd
	done;
	buf

(** write a buf to fd *)
let write fd buf =
	let len = String.length buf in
	let i = ref len in
	while !i <> 0
	do
		let wd = Unix.write fd buf (len - !i) !i in
		i := !i - wd
	done

(** connect to the host and port, and give the fd *)
let connect host port =
	let sockaddr = Unix.ADDR_INET (host, port) in
	let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	Unix.connect fd sockaddr;
	fd

(** Write an integer to an fd as 4 bytes, most significant first *)
let write_int fd x = 
	let buffer = "\000\000\000\000" in
	buffer.[0] <- char_of_int ((x lsr 24) land 0xff);
	buffer.[1] <- char_of_int ((x lsr 16) land 0xff);
	buffer.[2] <- char_of_int ((x lsr 8) land 0xff);
	buffer.[3] <- char_of_int ((x lsr 0) land 0xff);
	write fd buffer

(** Read a 4-byte most significant first integer from an fd *)
let read_int fd = 
	let buffer = read fd 4 in
	let a = int_of_char buffer.[0]
	and b = int_of_char buffer.[1] 
	and c = int_of_char buffer.[2] 
	and d = int_of_char buffer.[3] in
	(a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d
