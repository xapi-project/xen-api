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

exception Queue_deleted of string

type message_id = (string * int64) [@@deriving rpc, sexp]
(** uniquely identifier for this message *)

type message_id_opt = message_id option [@@deriving rpc, sexp]

let timeout = 30.

module Message = struct
  type kind =
    | Request of string
    | Response of message_id
  [@@deriving rpc, sexp]
  type t = {
    payload: string; (* switch to Rpc.t *)
    kind: kind;
  } [@@deriving rpc, sexp]

end

module Event = struct
  type message =
    | Message of message_id * Message.t
    | Ack of message_id
  [@@deriving rpc]

  type t = {
    time: float;
    input: string option;
    queue: string;
    output: string option;
    message: message;
    processing_time: int64 option;
  } [@@deriving rpc]

end

module In = struct
  type transfer = {
    from: string option;
    timeout: float;
    queues: string list;
  } [@@deriving rpc]

  type t =
    | Login of string            (** Associate this transport-level channel with a session *)
    | CreatePersistent of string
    | CreateTransient of string
    | Destroy of string          (** Explicitly remove a named queue *)
    | Send of string * Message.t (** Send a message to a queue *)
    | Transfer of transfer       (** blocking wait for new messages *)
    | Trace of int64 * float     (** blocking wait for trace data *)
    | Ack of message_id          (** ACK this particular message *)
    | List of string * [ `All | `Alive ]
                                (** return a list of queue names with a prefix *)
    | Diagnostics                (** return a diagnostic dump *)
    | Shutdown                   (** shutdown the switch *)
    | Get of string list         (** return a web interface resource *)
  [@@deriving rpc]

  let slash = Re.Str.regexp_string "/"
  let split = Re.Str.split_delim slash

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
    | "", `GET, [ ""; "list"; prefix ]    -> Some (List (Uri.pct_decode prefix, `All))
    | "", `GET, [ ""; "list"; "alive"; prefix ]    -> Some (List (Uri.pct_decode prefix, `Alive))
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
    Cohttp.Header.of_list [
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
    | List (x, `All) ->
      None, `GET, (Uri.make ~path:(Printf.sprintf "/list/%s" x) ())
    | List (x, `Alive) ->
      None, `GET, (Uri.make ~path:(Printf.sprintf "/list/alive/%s" x) ())
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
[@@deriving sexp, rpc]
(** identifies where a message came from *)

module Entry = struct
  type t = {
    origin: origin;
    time: int64;
    message: Message.t;
  } [@@deriving rpc, sexp]
  (** an enqueued message *)

  let make time origin message =
    { origin; time; message }
end

module Diagnostics = struct
  type queue_contents = (message_id * Entry.t) list [@@deriving rpc]

  type queue = {
    next_transfer_expected: int64 option;
    queue_contents: queue_contents;
  } [@@deriving rpc]

  type t = {
    start_time: int64;
    current_time: int64;
    permanent_queues: (string * queue) list;
    transient_queues: (string * queue) list;
  }
  [@@deriving rpc]
end

module Out = struct
  type transfer = {
    messages: (message_id * Message.t) list;
    next: string;
  } [@@deriving rpc]

  type trace = {
    events: (int64 * Event.t) list;
  } [@@deriving rpc]

  type queue_list = string list [@@deriving rpc]
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




