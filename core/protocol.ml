(*
 * Copyright (c) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib.Std
open Cohttp

exception Queue_deleted of string

type message_id = (string * int64) with rpc, sexp
(** uniquely identifier for this message *)

type message_id_opt = message_id option with rpc, sexp

let timeout = 30.

module Message = struct
  type kind =
    | Request of string
    | Response of message_id
  with rpc, sexp
  type t = {
    payload: string; (* switch to Rpc.t *)
    kind: kind;
  } with rpc, sexp

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
    | Shutdown                   (** shutdown the switch *)
    | Get of string list         (** return a web interface resource *)
  with rpc

  let slash = Re_str.regexp_string "/"
  let split = Re_str.split_delim slash

  let of_request body meth path =
    match body, meth, split path with
    | "", `GET, "" :: "admin" :: path     -> Some (Get path)
    | "", `GET, "" :: ((("js" | "css" | "images") :: _) as path) -> Some (Get path)
    | "", `GET, [ ""; "" ]                -> Some Diagnostics
    | "", `GET, [ ""; "login"; token ]    -> Some (Login (Uri.pct_decode token))
    | "", `GET, [ ""; "persistent"; name ] -> Some (CreatePersistent (Uri.pct_decode name))
    | "", `GET, [ ""; "transient"; name ] -> Some (CreateTransient (Uri.pct_decode name))
    | "", `GET, [ ""; "destroy"; name ]   -> Some (Destroy (Uri.pct_decode name))
    | "", `GET, [ ""; "ack"; name; id ]   -> Some (Ack (Uri.pct_decode name, Int64.of_string id))
    | "", `GET, [ ""; "list"; prefix ]    -> Some (List (Uri.pct_decode prefix))
    | "", `GET, [ ""; "trace"; ack_to; timeout ] ->
      Some (Trace(Int64.of_string ack_to, float_of_string timeout))
    | "", `GET, [ ""; "trace" ] ->
      Some (Trace(-1L, 5.))
    | body, `POST, [ ""; "transfer" ] ->
      Some (Transfer(transfer_of_rpc (Jsonrpc.of_string body)))
    | body, `POST, [ ""; "request"; name; reply_to ] ->
      Some (Send (Uri.pct_decode name, { Message.kind = Message.Request (Uri.pct_decode reply_to); payload = body }))
    | body, `POST, [ ""; "response"; name; from_q; from_n ] ->
      Some (Send (Uri.pct_decode name, { Message.kind = Message.Response (from_q, Int64.of_string from_n); payload = body }))
    | "", `POST, [ ""; "shutdown" ]       -> Some Shutdown
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
    | Shutdown ->
      None, `POST, (Uri.make ~path:"/shutdown" ())
    | Get path ->
      None, `GET, (Uri.make ~path:(String.concat "/" ("" :: "admin" :: path)) ())
end

type origin =
  | Anonymous of string (** An un-named connection, probably a temporary client connection *)
  | Name of string   (** A service with a well-known name *)
with rpc, sexp
(** identifies where a message came from *)

module Entry = struct
  type t = {
    origin: origin;
    time: int64;
    message: Message.t;
  } with rpc, sexp
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
    | Shutdown
    | Not_logged_in
    | Get of string

  let to_response = function
    | Login
    | Ack
    | Destroy ->
      `OK, ""
    | Shutdown ->
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

module type S = sig
  val whoami: unit -> string

  module IO: sig
    include Cohttp.S.IO

    val map: ('a -> 'b) -> 'a t -> 'b t

    val any: 'a t list -> 'a t

    val is_determined: 'a t -> bool
  end

  val connect: int -> (IO.ic * IO.oc) IO.t

  val disconnect: (IO.ic * IO.oc) -> unit IO.t

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

module Connection = functor(IO: Cohttp.S.IO) -> struct
  open IO
  module Request = Cohttp.Request.Make(IO)
  module Response = Cohttp.Response.Make(IO)

  let rpc (ic, oc) frame =
    let b, meth, uri = In.to_request frame in
    let body = match b with None -> "" | Some x -> x in
    let headers = In.headers body in
    let req = Cohttp.Request.make ~meth ~headers uri in
    Request.write (fun writer -> match b with
        | Some body ->
          Request.write_body writer body
        | None -> return ()
      ) req oc >>= fun () ->

    Response.read ic >>= function
    | `Ok response ->
      if Cohttp.Response.status response <> `OK then begin
        Printf.fprintf stderr "Server sent: %s\n%!" (Cohttp.Code.string_of_status (Cohttp.Response.status response));
        (* Response.write (fun _ _ -> return ()) response Lwt_io.stderr >>= fun () -> *)
        return (`Error Unsuccessful_response)
      end else begin
        let reader = Response.make_body_reader response ic in
        let results = Buffer.create 128 in
        let rec read () =
          Response.read_body_chunk reader >>= function
          | Transfer.Final_chunk x ->
            Buffer.add_string results x;
            return (`Ok (Buffer.contents results))
          | Transfer.Chunk x ->
            Buffer.add_string results x;
            read ()
          | Transfer.Done ->
            return (`Ok (Buffer.contents results)) in
        read ()
      end
    | `Invalid s ->
      Printf.fprintf stderr "Invalid response: '%s'\n%!" s;
      return (`Error Failed_to_read_response)
    | `Eof ->
      Printf.fprintf stderr "Empty response\n%!";
      return (`Error Failed_to_read_response)
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
    mutable requests_conn: (ic * oc);
    mutable events_conn: (ic * oc);
    requests_m: M.Mutex.t;
    wakener: (message_id, [ `Ok of Message.t | `Error of exn ] M.Ivar.t) Hashtbl.t;
    dest_queue_name: string;
    reply_queue_name: string;
  }

  let ( >>|= ) m f = m >>= function
    | `Ok x -> f x
    | `Error y -> return (`Error y)

  let rec iter_s f = function
    | [] -> return (`Ok ())
    | x :: xs ->
      f x >>|= fun () ->
      iter_s f xs

  let connect port dest_queue_name =
    let token = M.whoami () in
    let rec reconnect () =
      M.connect port >>= fun requests_conn ->
      Connection.rpc requests_conn (In.Login token) >>|= fun (_: string) ->
      M.connect port >>= fun events_conn ->
      Connection.rpc events_conn (In.Login token) >>|= fun (_: string) ->
      return (`Ok (requests_conn, events_conn)) in

    reconnect () >>|= fun (requests_conn, events_conn) ->

    let wakener = Hashtbl.create 10 in
    let requests_m = M.Mutex.create () in

    Connection.rpc requests_conn (In.CreateTransient token) >>|= fun reply_queue_name ->
    Connection.rpc requests_conn (In.CreatePersistent dest_queue_name) >>|= fun (_: string) ->

    let t = { requests_conn; events_conn; requests_m; wakener; dest_queue_name; reply_queue_name } in

    let (_ : [ `Ok of unit | `Error of exn ] M.IO.t) =
      let rec loop from =
        let transfer = {
          In.from = from;
          timeout = timeout;
          queues = [ reply_queue_name ]
        } in
        let frame = In.Transfer transfer in
        Connection.rpc events_conn frame >>= function
        | `Error _ ->
          reconnect ()
          >>|= fun (requests_conn, events_conn) ->
          t.requests_conn <- requests_conn;
          t.events_conn <- events_conn;
          loop from
        | `Ok raw ->
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
                       M.Ivar.fill (Hashtbl.find wakener j) (`Ok m);
                       return (`Ok ())
                     end else begin
                       Printf.printf "no wakener for id %s, %Ld\n%!" (fst i) (snd i);
                       Hashtbl.iter (fun k v ->
                         Printf.printf "  have wakener id %s, %Ld\n%!" (fst k) (snd k)
                       ) wakener;
                       return (`Ok ())
                     end
                   | Message.Request _ -> return (`Ok ())
                 )
            ) transfer.Out.messages >>|= fun () ->
          loop (Some transfer.Out.next) in
      loop None in
    return (`Ok t)

  let disconnect c =
    M.disconnect c.requests_conn >>= fun () ->
    M.disconnect c.events_conn

  let rpc c ?timeout x =
    let ivar = M.Ivar.create () in

    let timer = Opt.map (fun timeout ->
        M.Clock.run_after timeout (fun () -> M.Ivar.fill ivar (`Error Timeout))
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
           return (`Error (Queue_deleted c.dest_queue_name))
         | Some mid ->
           Hashtbl.add c.wakener mid ivar;
           return (`Ok mid)
      ) >>|= fun mid ->
    M.Ivar.read ivar >>|= fun x ->
    Hashtbl.remove c.wakener mid;
    Opt.iter M.Clock.cancel timer;
    return (`Ok x.Message.payload)

  let list c prefix =
    Connection.rpc c.requests_conn (In.List prefix) >>|= fun result ->
    return (`Ok (Out.string_list_of_rpc (Jsonrpc.of_string result)))

  let destroy c queue_name =
    Connection.rpc c.requests_conn (In.Destroy queue_name) >>|= fun result ->
    return (`Ok ())

  let shutdown c =
    Connection.rpc c.requests_conn In.Shutdown >>|= fun result ->
    return (`Ok ())
end


module Server = functor(M: S) -> struct

  module Connection = Connection(M.IO)

  open M.IO

  type t = {
    request_shutdown: unit M.Ivar.t;
    on_shutdown: unit M.Ivar.t;
  }

  let shutdown t =
    M.Ivar.fill t.request_shutdown ();
    M.Ivar.read t.on_shutdown

  let listen process c name =
    let token = Printf.sprintf "%d" (Unix.getpid ()) in
    Connection.rpc c (In.Login token) >>= fun _ ->
    Connection.rpc c (In.CreatePersistent name) >>= fun _ ->

    let request_shutdown = M.Ivar.create () in
    let on_shutdown = M.Ivar.create () in
    let t = { request_shutdown; on_shutdown } in

    let rec loop from =
      let transfer = {
        In.from = from;
        timeout = timeout;
        queues = [ name ];
      } in
      let frame = In.Transfer transfer in
      let message = Connection.rpc c frame in
      any [ map (fun _ -> ()) message; M.Ivar.read request_shutdown ] >>= fun () ->
      if is_determined (M.Ivar.read request_shutdown) then begin
        M.Ivar.fill on_shutdown ();
        return ()
      end else begin
        message >>= function
        | `Error e ->
          Printf.fprintf stderr "Server.listen.loop: %s\n%!" (Printexc.to_string e);
          return ()
        | `Ok raw ->
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
          end
      end in
    let (_: unit M.IO.t) = loop None in
    return t
end

(* The following type is deprecated, used only by legacy Unix clients *)
type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b
