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

(** A very very simple HTTP server! *)

(**
   Notes:
  
   HTTP CONNECT requests are not handled in the standard way! Normally, one
   would issue a connect request like this:
   
      CONNECT host.domain:port HTTP/1.0
   
   But we've got different proxies for different things, so we use the syntax
  
      CONNECT /console?session_id=... HTTP/1.0
   
   So we're not exactly standards compliant :)
  
  *)

type uri_path = string
val make_uri_path : string -> uri_path

type bufio_req_handler = Http.Request.t -> Buf_io.t -> unit
type fdio_req_handler = Http.Request.t -> Unix.file_descr -> unit
type req_handler = BufIO of bufio_req_handler | FdIO of fdio_req_handler

val connect_handler_table : (uri_path, req_handler) Hashtbl.t
val get_handler_table : (uri_path, req_handler) Hashtbl.t
val post_handler_table : (uri_path, req_handler) Hashtbl.t
val put_handler_table : (uri_path, req_handler) Hashtbl.t
val handler_table : Http.method_t -> (uri_path, req_handler) Hashtbl.t

val add_handler : Http.method_t -> uri_path -> req_handler -> unit

val best_effort : (unit -> unit) -> unit

val headers : Unix.file_descr -> string list -> unit
(*val get_return_version : Http.Request.t -> string*)

val response_fct :
  Http.Request.t ->
  ?hdrs:string list ->
  Unix.file_descr -> int64 -> (Unix.file_descr -> unit) -> unit

val response_str :
  Http.Request.t -> ?hdrs:string list -> Unix.file_descr -> string -> unit
val response_missing : ?hdrs:string list -> Unix.file_descr -> string -> unit
(*
val response_redirect : Unix.file_descr -> string -> unit
*)
val response_unauthorised : ?req:Http.Request.t -> string -> Unix.file_descr -> unit
val response_forbidden : ?req:Http.Request.t -> Unix.file_descr -> unit
val response_file :
  ?hdrs:'a list ->
  ?mime_content_type:string -> Unix.file_descr -> string -> unit

val request_of_bio: Buf_io.t -> Http.Request.t option

val default_callback : Http.Request.t -> Buf_io.t -> unit

exception Too_many_headers
exception Generic_error of string

val handle_connection : 'a -> Unix.file_descr -> unit

type server = Unix.file_descr

val bind : ?listen_backlog:int -> Unix.sockaddr -> server
val http_svr : (Unix.file_descr * string) list -> Thread.t list
val server_table : (Unix.file_descr, Server_io.server) Hashtbl.t

exception Server_not_found
val start : server * string -> Unix.file_descr
val stop : server -> unit

exception Client_requested_size_over_limit
val read_body : ?limit:int -> Http.Request.t -> Buf_io.t -> string

module Chunked :
  sig
    type t
    val of_bufio : Buf_io.t -> t
    val read : t -> int -> string
  end

val read_chunked_encoding : Http.Request.t -> Buf_io.t -> string Http.ll

