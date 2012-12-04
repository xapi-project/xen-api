open Lwt

exception Short_write of int * int
exception End_of_file

let proxy a b =
	let copy id src dst =
		let buffer = String.make 16384 '\000' in
		try_lwt
			while_lwt true do
				lwt n = Lwt_unix.read src buffer 0 (String.length buffer) in
				if n = 0
				then raise_lwt End_of_file
				else
					lwt m = Lwt_unix.write dst buffer 0 n in
					if n <> m then raise_lwt (Short_write(m, n))
					else return ()
			done
		with e ->
			(try Lwt_unix.shutdown src Lwt_unix.SHUTDOWN_RECEIVE with _ -> ());
			(try Lwt_unix.shutdown dst Lwt_unix.SHUTDOWN_SEND with _ -> ());
			return () in
	let ts = [ copy "ab" a b; copy "ba" b a ] in
	lwt () = Lwt.join ts in
	return ()

let file_descr_of_int (x: int) : Unix.file_descr =
	Obj.magic x (* Keep this in sync with ocaml's file_descr type *)

let proxy_socket = ref None
let ip = ref "127.0.0.1"

let main () =
	Arg.parse [
		"-ip", Arg.Set_string ip, (Printf.sprintf "IP address to bind to (default %s)" !ip);
		"-proxy", Arg.Int (fun x -> proxy_socket := Some x), "file-descriptor to proxy I/O to";
	] (fun x -> Printf.fprintf stderr "Unknown argument: %s" x)
		"accept one connection and proxy I/O to a second file-desriptor";

	let proxy_socket = match !proxy_socket with
		| Some x ->
			let fd = file_descr_of_int x in
			Lwt_unix.of_unix_file_descr fd
		| None ->
			Printf.fprintf stderr "You must provide a -proxy argument\n%!";
			exit 1 in

	let s = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
	Lwt_unix.bind s (Lwt_unix.ADDR_INET(Unix.inet_addr_of_string !ip, 0));
	Lwt_unix.listen s 5;
	let port = match Lwt_unix.getsockname s with
		| Unix.ADDR_INET(_, port) -> port
		| _ -> assert false in

	let protocols =
		let open Xcp.Channel in
		[
			TCP_proxy(!ip, port);
		] in
	Printf.fprintf stdout "%s\n%!" (Jsonrpc.to_string (Xcp.Channel.rpc_of_protocols protocols));

	lwt fd, peer = Lwt_unix.accept s in
	lwt () = Lwt_unix.close s in
	proxy fd proxy_socket

let _ =
	Lwt_main.run (main ())
