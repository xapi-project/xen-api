(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** A very very simple HTTP server *)

(** A URI path used to index handlers *)
type uri_path = string

(** A handler is a function which takes a request and produces a response *)
type 'a handler =
  | BufIO of (Http.Request.t -> Buf_io.t -> 'a -> unit)
  | FdIO of (Http.Request.t -> Unix.file_descr -> 'a -> unit)

module Stats : sig
  (** Statistics recorded per-handler *)
  type t = {
    mutable n_requests: int;    (** Total number of requests processed *)
    mutable n_connections: int; (** Total number of connections accepted *)
    mutable n_framed: int;      (** using the more efficient framed protocol *)
  }
end

module Server : sig

  (** Represents an HTTP server with a set of handlers and set of listening sockets *)
  type 'a t

  (** An HTTP server which sends back a default error response to every request *)
  val empty: 'a -> 'a t

  (** [add_handler x m uri h] adds handler [h] to server [x] to serve all requests with
      		method [m] for URI prefix [uri] *)
  val add_handler : 'a t -> Http.method_t -> uri_path -> 'a handler -> unit

  (** [find_stats x m uri] returns stats associated with method [m] and uri [uri]
      		in server [x], or None if none exist *)
  val find_stats: 'a t -> Http.method_t -> uri_path -> Stats.t option

  (** [all_stats x] returns a list of (method, uri, stats) triples *)
  val all_stats: 'a t -> (Http.method_t * uri_path * Stats.t) list

  (** [enable_fastpath x] switches on experimental performance optimisations *)
  val enable_fastpath: 'a t -> unit

end

exception Too_many_headers
exception Generic_error of string

type socket

val bind : ?listen_backlog:int -> Unix.sockaddr -> string -> socket

(* [bind_retry]: like [bind] but will catch (possibly transient exceptions) and retry *)
val bind_retry : ?listen_backlog:int -> Unix.sockaddr -> socket

val start : 'a Server.t -> socket -> unit

val handle_one : 'a Server.t -> Unix.file_descr -> 'a -> Http.Request.t -> bool

exception Socket_not_found

val stop : socket -> unit

exception Client_requested_size_over_limit

module Chunked :
sig
  type t
  val of_bufio : Buf_io.t -> t
  val read : t -> int -> string
end

val read_chunked_encoding : Http.Request.t -> Buf_io.t -> bytes Http.ll

(* The rest of this interface needs to be deleted and replaced with Http.Response.* *)

val response_fct :
  Http.Request.t ->
  ?hdrs:(string * string) list ->
  Unix.file_descr -> int64 -> (Unix.file_descr -> unit) -> unit

val response_str :
  Http.Request.t -> ?hdrs:(string * string) list -> Unix.file_descr -> string -> unit
val response_missing : ?hdrs:(string * string) list -> Unix.file_descr -> string -> unit
val response_unauthorised : ?req:Http.Request.t -> string -> Unix.file_descr -> unit
val response_forbidden : ?req:Http.Request.t -> Unix.file_descr -> unit
val response_badrequest : ?req:Http.Request.t -> Unix.file_descr -> unit
val response_internal_error: ?req:Http.Request.t -> ?extra:uri_path -> Unix.file_descr -> unit
val response_method_not_implemented : ?req:Http.Request.t -> Unix.file_descr -> unit
val response_file : ?mime_content_type:string -> Unix.file_descr -> string -> unit
val respond_to_options : Http.Request.t -> Unix.file_descr -> unit

val headers : Unix.file_descr -> string list -> unit
val read_body : ?limit:int -> Http.Request.t -> Buf_io.t -> string
val request_of_bio: ?use_fastpath:bool -> Buf_io.t -> Http.Request.t option

