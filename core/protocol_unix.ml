(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms, 
with or without modification, are permitted provided 
that the following conditions are met:

*   Redistributions of source code must retain the above 
    copyright notice, this list of conditions and the 
    following disclaimer.
*   Redistributions in binary form must reproduce the above 
    copyright notice, this list of conditions and the 
    following disclaimer in the documentation and/or other 
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.
*)

open Protocol
open Cohttp

let whoami () = Printf.sprintf "%s:%d"
	(Filename.basename Sys.argv.(0)) (Unix.getpid ())

module IO = struct
	type 'a t = 'a
	let ( >>= ) a f = f a
	let (>>) m n = m >>= fun _ -> n

	let return a = a

	let iter = List.iter

	type ic = in_channel
	type oc = out_channel

	let read_line ic =
		try
			let line = input_line ic in
			let last = String.length line - 1 in
			let line = if line.[last] = '\r' then String.sub line 0 last else line in
			Some line
		with _ -> None

	let read_into_exactly ic buf ofs len =
		try
			really_input ic buf ofs len; true
		with _ -> false
	let read_exactly ic len =
		let buf = String.create len in
		read_into_exactly ic buf 0 len >>= function
		| true -> return (Some buf)
		| false -> return None

	let read ic n =
		let buf = String.make n '\000' in
		let actually_read = input ic buf 0 n in
		if actually_read = n
		then buf
		else String.sub buf 0 actually_read

	let write oc x = 
		output_string oc x; flush oc

	let connect port =
		let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port) in
		let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		let () = Unix.connect fd sockaddr in
		Unix.setsockopt fd Unix.TCP_NODELAY true;
		let ic = Unix.in_channel_of_descr fd in
		let oc = Unix.out_channel_of_descr fd in
		(ic, oc)
end

module Connection = Protocol.Connection(IO)

exception Timeout

module Opt = struct
	let iter f = function
	| None -> ()
	| Some x -> f x
	let map f = function
	| None -> None
	| Some x -> Some (f x)
end

let with_lock m f =
	Mutex.lock m;
	try
		let r = f () in
		Mutex.unlock m;
		r
	with e ->
		Mutex.unlock m;
		raise e

let rpc_exn c frame = match Connection.rpc c frame with
	| Error e -> raise e
	| Ok raw -> raw

module Client = struct
	type 'a response = {
		mutable v: 'a option;
		mutable timed_out: bool;
		m: Mutex.t;
		c: Condition.t;
	}
	let task () = {
		v = None;
		timed_out = false;
		m = Mutex.create ();
		c = Condition.create ();
	}
	let wakeup_later r x =
		with_lock r.m
			(fun () ->
				r.v <- Some x;
				Condition.signal r.c
			)
	let timeout_later r =
		with_lock r.m
			(fun () ->
				r.timed_out <- true;
				Condition.signal r.c
			)
	let wait r =
		with_lock r.m
			(fun () ->
				while r.v = None && not r.timed_out do
					Condition.wait r.c r.m
				done;
				match r.v, r.timed_out with
				| _, true -> raise Timeout
				| Some x, _ -> x
				| None, false -> assert false
			)

	type t = {
		requests_conn: (IO.ic * IO.oc);
		events_conn: (IO.ic * IO.oc);
		requests_m: Mutex.t;
		wakener: (int64, Protocol.Message.t response) Hashtbl.t;
		reply_queue_name: string; 
	}

	let connect port =
		let token = whoami () in
		let requests_conn = IO.connect port in
		let (_: string) = rpc_exn requests_conn (In.Login token) in
		let events_conn = IO.connect port in
		let (_: string) = rpc_exn events_conn (In.Login token) in

		let wakener = Hashtbl.create 10 in

		Protocol_unix_scheduler.start ();

		let requests_m = Mutex.create () in
		let (_ : Thread.t) =
			let rec loop from =
				let timeout = 5. in
				let frame = In.Transfer(from, timeout) in
				let raw = rpc_exn events_conn frame in
				let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
				match transfer.Out.messages with
				| [] -> loop from
				| m :: ms ->
					List.iter
						(fun (i, m) ->
							(* If the Ack doesn't belong to us then assume it's another thread *)
							with_lock requests_m (fun () ->
								match m.Message.kind with
								| Message.Response j ->
									if Hashtbl.mem wakener j then begin
										let (_: string) = rpc_exn events_conn (In.Ack i) in
										wakeup_later (Hashtbl.find wakener j) m;
									end else Printf.printf "no wakener for id %Ld\n%!" i
								| Message.Request _ -> ()
							)
						) transfer.Out.messages;
					let from = List.fold_left max (fst m) (List.map fst ms) in
					loop from in
			Thread.create loop (-1L) in
		let reply_queue_name = rpc_exn requests_conn (In.CreateTransient token) in
		let (_: string) = rpc_exn requests_conn (In.Subscribe reply_queue_name) in
		{
			requests_conn = requests_conn;
			events_conn = events_conn;
			requests_m;
			wakener = wakener;
			reply_queue_name = reply_queue_name;
		}

	(* Maintain at most one connection per process *)
	let connect =
		let c = ref None in
		let m = Mutex.create () in
		fun port ->
			with_lock m (fun () ->
				match !c with
				| Some x -> x
				| None ->
					let c' = connect port in
					c := Some c';
					c'
			)

	let rpc c ?timeout ~dest:dest_queue_name x =
		let t = task () in
		let timer = Opt.map (fun timeout ->
			Protocol_unix_scheduler.(one_shot (Delta timeout) "rpc" (fun () -> timeout_later t))
		) timeout in

		let id = with_lock c.requests_m
		(fun () ->
			let (_: string) = rpc_exn c.requests_conn (In.CreatePersistent dest_queue_name) in
			let msg = In.Send(dest_queue_name, {
				Message.payload = x;
				kind = Message.Request c.reply_queue_name
			}) in
			let (id: string) = rpc_exn c.requests_conn msg in
			if id = "" then raise (Protocol.Queue_deleted dest_queue_name);
			let id = Int64.of_string id in
			Hashtbl.add c.wakener id t;
			id
		) in
		(* now block waiting for our response *)
		let response = wait t in
		(* release resources *)
		Opt.iter Protocol_unix_scheduler.cancel timer;
		with_lock c.requests_m (fun () -> Hashtbl.remove c.wakener id);

		response.Message.payload

	let list c prefix =
		with_lock c.requests_m
		(fun () ->
			let (result: string) = rpc_exn c.requests_conn (In.List prefix) in
			Out.string_list_of_rpc (Jsonrpc.of_string result)
		)
end

module Server = struct

	let listen process port name =
		let open IO in

		let token = whoami () in
		let request_conn = IO.connect port in
		let (_: string) = rpc_exn request_conn (In.Login token) in
		let reply_conn = IO.connect port in
		let (_: string) = rpc_exn reply_conn (In.Login token) in

		(* Only allow one reply RPC at a time (no pipelining) *)
		let m = Mutex.create () in
		let reply req = with_lock m (fun () -> Connection.rpc reply_conn req) in

		Connection.rpc request_conn (In.Login token) >>= fun _ ->
		Connection.rpc request_conn (In.CreatePersistent name) >>= fun _ ->
		Connection.rpc request_conn (In.Subscribe name) >>= fun _ ->
		Printf.fprintf stdout "Serving requests forever\n%!";

		let rec loop from =
			let timeout = 5. in
			let frame = In.Transfer(from, timeout) in
			Connection.rpc request_conn frame >>= function
			| Error e ->
				Printf.fprintf stderr "Server.listen.loop: %s\n%!" (Printexc.to_string e);
				return ()
			| Ok raw ->
				let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
				begin match transfer.Out.messages with
				| [] -> loop from
				| m :: ms ->
					List.iter
						(fun (i, m) ->
							let (_: Thread.t) = Thread.create
							(fun () ->
								let response =
									try
										process m.Message.payload
									with e ->
										Printexc.to_string e in
								response >>= fun response ->
								begin
									match m.Message.kind with
									| Message.Response _ ->
										(* response where a request should be: configuration error? *)
										return ()
									| Message.Request reply_to ->
										let request = In.Send(reply_to, { Message.kind = Message.Response i; payload = response }) in
										reply request >>= fun _ ->
										return ()
								end >>= fun () ->
								let request = In.Ack i in
								reply request >>= fun _ ->
								()
							) () in ()
						) transfer.Out.messages;
					let from = List.fold_left max (fst m) (List.map fst ms) in
					loop from
				end in
		loop (-1L)
end

