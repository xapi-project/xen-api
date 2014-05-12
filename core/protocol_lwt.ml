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
open Lwt
open Cohttp
open Cohttp_lwt_unix

let whoami () = Printf.sprintf "%s:%d"
	(Filename.basename Sys.argv.(0)) (Unix.getpid ())

module IO = struct
	type 'a t = 'a Lwt.t
	let ( >>= ) = Lwt.bind
	let (>>) m n = m >>= fun _ -> n
	let return = Lwt.return

	type ic = Lwt_io.input Lwt_io.channel
	type oc = Lwt_io.output Lwt_io.channel

	let iter = Lwt_list.iter_s

	let read_line = Lwt_io.read_line_opt
	let read ic count =
		try_lwt Lwt_io.read ~count ic
		with End_of_file -> return ""
	let read_into_exactly ic buf off len =
		try_lwt
			lwt () = Lwt_io.read_into_exactly ic buf off len in
			return true
		with End_of_file -> return false
	let read_exactly ic len =
		let buf = String.create len in
		read_into_exactly ic buf 0 len >>= function
		| true -> return (Some buf)
		| false -> return None

	let write = Lwt_io.write
	let write_line = Lwt_io.write_line
	let flush = Lwt_io.flush
	let connect port =
		let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port) in
		let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
		lwt () = Lwt_unix.connect fd sockaddr in
		let ic = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.input fd in
		let oc = Lwt_io.of_fd ~close:(fun () -> return ()) ~mode:Lwt_io.output fd in
		return (ic, oc)
end

module Connection = Protocol.Connection(IO)

module Client = struct
	type t = {
		requests_conn: (IO.ic * IO.oc);
		events_conn: (IO.ic * IO.oc);
		requests_m: Lwt_mutex.t;
		wakener: (Protocol.message_id, Message.t Lwt.u) Hashtbl.t;
		dest_queue_name: string;
		reply_queue_name: string;
	}

	let lwt_rpc c frame = match_lwt Connection.rpc c frame with
		| Error e -> fail e
		| Ok raw -> return raw

	let connect port dest_queue_name =
		let token = whoami () in
		lwt requests_conn = IO.connect port in
		lwt (_: string) = lwt_rpc requests_conn (In.Login token) in
		lwt events_conn = IO.connect port in
		lwt (_: string) = lwt_rpc events_conn (In.Login token) in

		let wakener = Hashtbl.create 10 in
		let requests_m = Lwt_mutex.create () in

		lwt reply_queue_name = lwt_rpc requests_conn (In.CreateTransient token) in

		let (_ : unit Lwt.t) =
			let rec loop from =
				let timeout = 5. in
				let transfer = {
					In.from = from;
					timeout = timeout;
					queues = [ reply_queue_name ]
				} in
				let frame = In.Transfer transfer in
				lwt raw = lwt_rpc events_conn frame in
				let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
				match transfer.Out.messages with
				| [] -> loop from
				| m :: ms ->
					lwt () = Lwt_list.iter_s
						(fun (i, m) ->
							Lwt_mutex.with_lock requests_m (fun () ->
								match m.Message.kind with
								| Message.Response j ->
									if Hashtbl.mem wakener j then begin
										lwt (_: string) = lwt_rpc events_conn (In.Ack i) in
										wakeup_later (Hashtbl.find wakener j) m;
										return ()
									end else begin
										Printf.printf "no wakener for id %s, %Ld\n%!" (fst i) (snd i);
										return ()
									end
								| Message.Request _ -> return ()
							)
						) transfer.Out.messages in
					loop (Some transfer.Out.next) in
			loop None in
		lwt (_: string) = lwt_rpc requests_conn (In.CreatePersistent dest_queue_name) in
		return {
			requests_conn = requests_conn;
			events_conn = events_conn;
			requests_m;
			wakener = wakener;
			dest_queue_name = dest_queue_name;
			reply_queue_name = reply_queue_name;
		}

	let rpc c x =
		let t, u = Lwt.task () in
		let msg = In.Send(c.dest_queue_name, {
			Message.payload = x;
			kind = Message.Request c.reply_queue_name
		}) in
		lwt () = Lwt_mutex.with_lock c.requests_m
		(fun () ->
			lwt (id: string) = lwt_rpc c.requests_conn msg in
			match message_id_opt_of_rpc (Jsonrpc.of_string id) with
			| None ->
				fail (Queue_deleted c.dest_queue_name)
			| Some mid ->
				Hashtbl.add c.wakener mid u;
				return ()
		) in
		lwt response = t in
		return response.Message.payload

	let list c prefix =
		lwt (result: string) = lwt_rpc c.requests_conn (In.List prefix) in
		return (Out.string_list_of_rpc (Jsonrpc.of_string result))
end

module Server = Protocol.Server(IO)
