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

open Cohttp

exception Queue_deleted of string

type message_id = (string * int64) with rpc
(** uniquely identifier for this message *)

type message_id_opt = message_id option with rpc

module Message = struct
	type kind =
	| Request of string
	| Response of message_id
	with rpc
	type t = {
		payload: string; (* switch to Rpc.t *)
		kind: kind;
	} with rpc

end

module Event = struct
	type message =
		| Message of message_id * Message.t
		| Ack of message_id
	with rpc

	type t = {
		time: float;
		input: string option;
		queue: string;
		output: string option;
		message: message;
		processing_time: int64 option;
	} with rpc

end

module In = struct
	type transfer = {
		from: string option;
		timeout: float;
		queues: string list;
	} with rpc

	type t =
	| Login of string            (** Associate this transport-level channel with a session *)
	| CreatePersistent of string
	| CreateTransient of string
	| Destroy of string          (** Explicitly remove a named queue *)
	| Send of string * Message.t (** Send a message to a queue *)
	| Transfer of transfer       (** blocking wait for new messages *)
	| Trace of int64 * float     (** blocking wait for trace data *)
	| Ack of message_id          (** ACK this particular message *)
	| List of string             (** return a list of queue names with a prefix *)
	| Diagnostics                (** return a diagnostic dump *)
	| Get of string list         (** return a web interface resource *)
	with rpc

	let slash = Re_str.regexp_string "/"
	let split = Re_str.split_delim slash

	let of_request body meth path =
		match body, meth, split path with
		| "", `GET, "" :: "admin" :: path     -> Some (Get path)
		| "", `GET, "" :: ((("js" | "css" | "images") :: _) as path) -> Some (Get path)
		| "", `GET, [ ""; "" ]                -> Some Diagnostics
		| "", `GET, [ ""; "login"; token ]    -> Some (Login token)
		| "", `GET, [ ""; "persistent"; name ] -> Some (CreatePersistent name)
		| "", `GET, [ ""; "transient"; name ] -> Some (CreateTransient name)
		| "", `GET, [ ""; "destroy"; name ]   -> Some (Destroy name)
		| "", `GET, [ ""; "ack"; name; id ]   -> Some (Ack (name, Int64.of_string id))
		| "", `GET, [ ""; "list"; prefix ]    -> Some (List prefix)
		| "", `GET, [ ""; "trace"; ack_to; timeout ] ->
			Some (Trace(Int64.of_string ack_to, float_of_string timeout))
		| "", `GET, [ ""; "trace" ] ->
			Some (Trace(-1L, 5.))
		| body, `POST, [ ""; "transfer" ] ->
			Some (Transfer(transfer_of_rpc (Jsonrpc.of_string body)))
		| body, `POST, [ ""; "request"; name; reply_to ] ->
			Some (Send (name, { Message.kind = Message.Request reply_to; payload = body }))
		| body, `POST, [ ""; "response"; name; from_q; from_n ] ->
			Some (Send (name, { Message.kind = Message.Response (from_q, Int64.of_string from_n); payload = body }))
		| _, _, _ -> None

	let headers payload =
		Header.of_list [
            "user-agent", "cohttp";
            "content-length", string_of_int (String.length payload);
            "connection", "keep-alive";
        ]


	let to_request = function
		| Login token ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/login/%s" token) ())
		| CreatePersistent name ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/persistent/%s" name) ())
		| CreateTransient name ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/transient/%s" name) ())
		| Destroy name ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/destroy/%s" name) ())
		| Ack (name, x) ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/ack/%s/%Ld" name x) ())
		| List x ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/list/%s" x) ())
		| Transfer t ->
			let body = Jsonrpc.to_string (rpc_of_transfer t) in
			Some body, `POST, (Uri.make ~path:"/transfer" ())
		| Trace(ack_to, timeout) ->
			None, `GET, (Uri.make ~path:(Printf.sprintf "/trace/%Ld/%.16g" ack_to timeout) ())
		| Send (name, { Message.kind = Message.Request r; payload = p }) ->
			Some p, `POST, (Uri.make ~path:(Printf.sprintf "/request/%s/%s" name r) ())
		| Send (name, { Message.kind = Message.Response (q, i); payload = p }) ->
			Some p, `POST, (Uri.make ~path:(Printf.sprintf "/response/%s/%s/%Ld" name q i) ())
		| Diagnostics ->
			None, `GET, (Uri.make ~path:"/" ())
		| Get path ->
			None, `GET, (Uri.make ~path:(String.concat "/" ("" :: "admin" :: path)) ())
end

type origin =
	| Anonymous of string (** An un-named connection, probably a temporary client connection *)
	| Name of string   (** A service with a well-known name *)
with rpc
(** identifies where a message came from *)

module Entry = struct
	type t = {
		origin: origin;
		time: int64;
		message: Message.t;
	} with rpc
	(** an enqueued message *)

	let make time origin message =
		{ origin; time; message }
end

module Diagnostics = struct
	type queue_contents = (message_id * Entry.t) list with rpc

	type queue = {
		next_transfer_expected: int64 option;
		queue_contents: queue_contents;
	} with rpc

	type t = {
		start_time: int64;
		current_time: int64;
		permanent_queues: (string * queue) list;
		transient_queues: (string * queue) list;
	}
	with rpc
end

module Out = struct
	type transfer = {
		messages: (message_id * Message.t) list;
		next: string;
	} with rpc

	type trace = {
		events: (int64 * Event.t) list;
	} with rpc

	type queue_list = string list with rpc
	let rpc_of_string_list = rpc_of_queue_list
	let string_list_of_rpc = queue_list_of_rpc

	type t =
	| Login
	| Create of string
	| Destroy
	| Send of message_id option
	| Transfer of transfer
	| Trace of trace
	| Ack
	| List of string list
	| Diagnostics of Diagnostics.t
	| Not_logged_in
	| Get of string

	let to_response = function
		| Login
		| Ack
		| Destroy ->
			`OK, ""
		| Send x ->
			`OK, (Jsonrpc.to_string (rpc_of_message_id_opt x))
		| Create name ->
			`OK, name
		| Transfer transfer ->
			`OK, (Jsonrpc.to_string (rpc_of_transfer transfer))
		| Trace trace ->
			`OK, (Jsonrpc.to_string (rpc_of_trace trace))
		| List l ->
			`OK, (Jsonrpc.to_string (rpc_of_queue_list l))
		| Diagnostics x ->
			`OK, (Jsonrpc.to_string (Diagnostics.rpc_of_t x))
		| Not_logged_in ->
			`Not_found, "Please log in."
		| Get x ->
			`OK, x
end

exception Failed_to_read_response

exception Unsuccessful_response

exception Timeout

type ('a, 'b) result =
| Ok of 'a
| Error of 'b

module type S = sig
  val whoami: unit -> string

  module IO: Cohttp.IO.S

  val connect: int -> (IO.ic * IO.oc) IO.t

  module Ivar : sig
    type 'a t

    val create: unit -> 'a t

    val fill: 'a t -> 'a -> unit

    val read: 'a t -> 'a IO.t
  end

  module Mutex : sig
    type t

    val create: unit -> t

    val with_lock: t -> (unit -> 'a IO.t) -> 'a IO.t
  end

  module Clock : sig
    type timer

    val run_after: int -> (unit-> unit) -> timer

    val cancel: timer -> unit
  end
end

module Connection = functor(IO: Cohttp.IO.S) -> struct
	open IO
	module Request = Cohttp.Request.Make(IO)
	module Response = Cohttp.Response.Make(IO)

	let rpc (ic, oc) frame =
		let b, meth, uri = In.to_request frame in
		let body = match b with None -> "" | Some x -> x in
		let headers = In.headers body in
		let req = Cohttp.Request.make ~meth ~headers uri in
		Request.write (fun req oc -> match b with
		| Some body ->
			Request.write_body req oc body
		| None -> return ()
		) req oc >>= fun () ->

		Response.read ic >>= function
		| `Ok response ->
			if Cohttp.Response.status response <> `OK then begin
				Printf.fprintf stderr "Server sent: %s\n%!" (Cohttp.Code.string_of_status (Cohttp.Response.status response));
				(* Response.write (fun _ _ -> return ()) response Lwt_io.stderr >>= fun () -> *)
				return (Error Unsuccessful_response)
			end else begin
				Response.read_body_chunk response ic >>= function
				| Transfer.Final_chunk x -> return (Ok x)
				| Transfer.Chunk x -> return (Ok x)
				| Transfer.Done -> return (Ok "")
			end
		| `Invalid s ->
			Printf.fprintf stderr "Invalid response: '%s'\n%!" s;
			return (Error Failed_to_read_response)
		| `Eof ->
			Printf.fprintf stderr "Empty response\n%!";
			return (Error Failed_to_read_response)
end

module Opt = struct
  let iter f = function
  | None -> ()
  | Some x -> f x
  let map f = function
  | None -> None
  | Some x -> Some (f x)
end

module Client = functor(M: S) -> struct

  module Connection = Connection(M.IO)

  open M.IO

  type t = {
    requests_conn: (ic * oc);
    events_conn: (ic * oc);
    requests_m: M.Mutex.t;
    wakener: (message_id, (Message.t, exn) result M.Ivar.t) Hashtbl.t;
    dest_queue_name: string;
    reply_queue_name: string;
  }

  let ( >>|= ) m f = m >>= function
    | Ok x -> f x
    | Error y -> return (Error y)

  let rec iter_s f = function
    | [] -> return (Ok ())
    | x :: xs ->
      f x >>|= fun () ->
      iter_s f xs

  let connect port dest_queue_name =
    let token = M.whoami () in
    M.connect port >>= fun requests_conn ->
    Connection.rpc requests_conn (In.Login token) >>|= fun (_: string) ->
    M.connect port >>= fun events_conn ->
    Connection.rpc events_conn (In.Login token) >>|= fun (_: string) ->

    let wakener = Hashtbl.create 10 in
    let requests_m = M.Mutex.create () in

    Connection.rpc requests_conn (In.CreateTransient token) >>|= fun reply_queue_name ->
    let (_ : (unit, exn) result M.IO.t) =
      let rec loop from =
        let timeout = 5. in
        let transfer = {
          In.from = from;
          timeout = timeout;
          queues = [ reply_queue_name ]
        } in
        let frame = In.Transfer transfer in
        Connection.rpc events_conn frame >>|= fun raw ->
        let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
        match transfer.Out.messages with
        | [] -> loop from
        | m :: ms ->
          iter_s
            (fun (i, m) ->
              M.Mutex.with_lock requests_m (fun () ->
                match m.Message.kind with
                | Message.Response j ->
                  if Hashtbl.mem wakener j then begin
                    Connection.rpc events_conn (In.Ack i) >>|= fun (_: string) ->
                    M.Ivar.fill (Hashtbl.find wakener j) (Ok m);
                    return (Ok ())
                  end else begin
                    Printf.printf "no wakener for id %s, %Ld\n%!" (fst i) (snd i);
                    return (Ok ())
                  end
                | Message.Request _ -> return (Ok ())
              )
            ) transfer.Out.messages >>|= fun () ->
          loop (Some transfer.Out.next) in
      loop None in
    Connection.rpc requests_conn (In.CreatePersistent dest_queue_name) >>|= fun (_: string) ->
    return (Ok {
      requests_conn;
      events_conn;
      requests_m;
      wakener = wakener;
      dest_queue_name;
      reply_queue_name;
    })

  let rpc c ?timeout x =
    let ivar = M.Ivar.create () in

    let timer = Opt.map (fun timeout ->
      M.Clock.run_after timeout (fun () -> M.Ivar.fill ivar (Error Timeout))
    ) timeout in

    let msg = In.Send(c.dest_queue_name, {
      Message.payload = x;
      kind = Message.Request c.reply_queue_name
    }) in
    M.Mutex.with_lock c.requests_m
    (fun () ->
      Connection.rpc c.requests_conn msg >>|= fun (id: string) ->
      match message_id_opt_of_rpc (Jsonrpc.of_string id) with
      | None ->
        return (Error (Queue_deleted c.dest_queue_name))
      | Some mid ->
        Hashtbl.add c.wakener mid ivar;
        return (Ok mid)
    ) >>|= fun mid ->
    M.Ivar.read ivar >>|= fun x ->
    Hashtbl.remove c.wakener mid;
    Opt.iter M.Clock.cancel timer;
    return (Ok x.Message.payload)

  let list c prefix =
    Connection.rpc c.requests_conn (In.List prefix) >>|= fun result ->
    return (Ok (Out.string_list_of_rpc (Jsonrpc.of_string result)))
end


module Server = functor(IO: Cohttp.IO.S) -> struct

	module Connection = Connection(IO)

  open IO

	let listen process c name =
		let token = Printf.sprintf "%d" (Unix.getpid ()) in
		Connection.rpc c (In.Login token) >>= fun _ ->
		Connection.rpc c (In.CreatePersistent name) >>= fun _ ->
		Printf.fprintf stdout "Serving requests forever\n%!";

		let rec loop from =
			let timeout = 5. in
			let transfer = {
				In.from = from;
				timeout = timeout;
				queues = [ name ];
			} in
			let frame = In.Transfer transfer in
			Connection.rpc c frame >>= function
			| Error e ->
				Printf.fprintf stderr "Server.listen.loop: %s\n%!" (Printexc.to_string e);
				return ()
			| Ok raw ->
				let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
				begin match transfer.Out.messages with
				| [] -> loop from
				| m :: ms ->
					iter
						(fun (i, m) ->
							process m.Message.payload >>= fun response ->
							begin
								match m.Message.kind with
								| Message.Response _ ->
									return () (* configuration error *)
								| Message.Request reply_to ->
									let request = In.Send(reply_to, { Message.kind = Message.Response i; payload = response }) in
									Connection.rpc c request >>= fun _ ->
									return ()
					 		end >>= fun () ->
							let request = In.Ack i in
							Connection.rpc c request >>= fun _ ->
							return ()
						) transfer.Out.messages >>= fun () ->
					loop (Some transfer.Out.next)
				end in
		loop None
end
