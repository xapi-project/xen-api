open Nbd

let global_id = ref 0L
let global_mutex = Lwt_mutex.create ()
let get_id () = Lwt_mutex.with_lock global_mutex
	(fun () ->
		let x = !global_id in
		global_id := Int64.add 1L !global_id;
		Lwt.return x)

let really_read_string sock len =
	let str = String.make len '\000' in
	let rec inner left =
		lwt n = Lwt_unix.read sock str (len - left) left in
	    if n=left then Lwt.return str else inner (left - n)
	in
	inner len

let bitstring_of_file_descr_max sock max =
	lwt s = really_read_string sock max in 
    Lwt.return (Bitstring.bitstring_of_string s)

module NbdRpc = struct
	type transport = Lwt_unix.file_descr
	type id = int64
	type request_hdr = Request.t
	type request_body = string option
	type response_hdr = Reply.t
	type response_body = string option

	let recv_hdr sock =
		lwt str = really_read_string sock 16 in
		let reply = parse_reply (Bitstring.bitstring_of_string str) in
		Lwt.return (Some reply.Reply.handle, reply)

	let recv_body sock req_hdr res_hdr =
		if res_hdr.Reply.error <> 0l then Lwt.fail (Failure "Error returned") else
			match req_hdr.Request.ty with
				| NBD_cmd_read -> 
					lwt s = really_read_string sock (Int32.to_int req_hdr.Request.len) in
	                Lwt.return (Some s)
				| _ -> 
                    Lwt.return None

	let send_one sock req_hdr req_body =
		let msg = construct_request req_hdr in
		lwt n = Lwt_unix.write sock msg 0 (String.length msg) in
		if n<>String.length msg then Lwt.fail (Failure "Short write!") else
			match req_body with
				| None -> Lwt.return ()
				| Some b -> begin 
					lwt n = Lwt_unix.write sock b 0 (String.length b) in
					if n <> String.length b then Lwt.fail (Failure "Short write!") else
						Lwt.return ()
					end

	let id_of_request req = req.Request.handle

	let handle_unrequested_packet t reply =
		Lwt.return ()
end

module Mux = Lwt_mux.Mux(NbdRpc)

type t = Mux.client

let negotiate sock =
	lwt str = really_read_string sock 8 in
	Printf.printf "Read init_passwd\n%!";
	if str <> init_passwd then Lwt.fail (Failure "Bad magic in negotiate") else
		lwt bs = bitstring_of_file_descr_max sock 8 in
        Printf.printf "Read magic\n%!";
        let magic = get_int64 bs in
		if magic=opts_magic then Lwt.fail (Failure "Unhandled opts_magic")
		else if magic <> cliserv_magic then Lwt.fail (Failure "Bad magic") else
			lwt bs = bitstring_of_file_descr_max sock 8 in
            Printf.printf "Read size\n%!";
            let sz = get_int64 bs in
			lwt bs = bitstring_of_file_descr_max sock 4 in
            let flags = get_int32 bs in
			Printf.printf "Read flags\n%!";
			lwt bs = bitstring_of_file_descr_max sock 124 in
            lwt t = Mux.create sock in
			Lwt.return (t, sz, flags_of_flags (Int32.to_int flags))

let connect hostname port =
	let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
	Printf.printf "Created socket\n%!";
	Printf.printf "Looking up host: %s\n%!" hostname;
	lwt host_info = Lwt_unix.gethostbyname hostname in
    Printf.printf "Gothostbyname\n%!";
    let server_address = host_info.Lwt_unix.h_addr_list.(0) in
	lwt () = Lwt_unix.connect socket (Lwt_unix.ADDR_INET (server_address, port)) in
    Printf.printf "Connected\n%!";
    negotiate socket

let write t str from =
	lwt id = get_id () in
    let req_hdr = {
		Request.ty = NBD_cmd_write;
		handle=id;
		from=from;
		len=Int32.of_int (String.length str);
	} in
	let req_body = Some str in
	lwt _ = Mux.rpc req_hdr req_body t in
    Lwt.return ()

let read t from len =
	lwt id = get_id () in
    let req_hdr = {
		Request.ty = NBD_cmd_read;
		handle=id;
		from=from;
		len=len
	} in
	let req_body = None in
	lwt res = Mux.rpc req_hdr req_body t in
    match res with
		| (_,Some res) -> Lwt.return res
		| _ -> Lwt.fail (Failure "No response!?")
