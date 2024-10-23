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

(** Recognised HTTP methods *)
type method_t = Get | Post | Put | Connect | Options | Unknown of string

val string_of_method_t : method_t -> string

val method_t_of_string : string -> method_t

(** Exception raised when parsing start line of request *)
exception Http_parse_failure

exception Unauthorised of string

exception Method_not_implemented

exception Forbidden

exception Timeout

exception Too_large

exception Client_requested_size_over_limit

type authorization = Basic of string * string | UnknownAuth of string

val make_frame_header : string -> string

val read_http_request_header :
     read_timeout:float option
  -> total_timeout:float option
  -> max_length:int option
  -> Unix.file_descr
  -> bool * string * string option

val read_http_response_header : bytes -> Unix.file_descr -> int

module Accept : sig
  type t = {ty: (string * string option) option  (** None means '*' *); q: int}

  exception Parse_failure of string

  val equal : t -> t -> bool

  val of_string : string -> t list

  val to_string : t -> string

  val matches : string -> t -> bool

  val preferred : from:string list -> t list -> string list
  (** [preferred ~from accepted] returns the content types in [~from]
      that are accepted by elements of [accepted] in priority order *)
end

(** Parsed form of the HTTP request line plus cookie info *)
module Request : sig
  type t = {
      m: method_t
    ; uri: string
    ; query: (string * string) list
    ; version: string
    ; frame: bool
    ; transfer_encoding: string option
    ; accept: string option
    ; content_length: int64 option
    ; auth: authorization option
    ; cookie: (string * string) list
    ; task: string option
    ; subtask_of: string option
    ; content_type: string option
    ; host: string option
    ; user_agent: string option
    ; mutable close: bool
    ; additional_headers: (string * string) list
    ; body: string option
    ; traceparent: string option
  }

  val rpc_of_t : t -> Rpc.t

  val t_of_rpc : Rpc.t -> t

  val empty : t

  val make :
       ?frame:bool
    -> ?version:string
    -> ?keep_alive:bool
    -> ?accept:string
    -> ?cookie:(string * string) list
    -> ?length:int64
    -> ?auth:authorization
    -> ?subtask_of:string
    -> ?body:string
    -> ?headers:(string * string) list
    -> ?content_type:string
    -> ?host:string
    -> ?query:(string * string) list
    -> ?traceparent:string
    -> user_agent:string
    -> method_t
    -> string
    -> t
  (** [make] is the standard constructor for [t] *)

  val get_version : t -> string
  (** [get_version t] returns the HTTP protocol version *)

  val to_string : t -> string
  (** [to_string t] returns a short string summarising [t] *)

  val to_header_list : t -> string list
  (** [to_header_list t] returns the list of HTTP headers associated
      		with [t] *)

  val to_wire_string : t -> string
  (** [to_wire_string t] returns a string which could be sent to a server *)

  val with_originator_of : t option -> (string option -> unit) -> unit

  val traceparent_of : t -> Tracing.Span.t option

  val with_tracing :
    ?attributes:(string * string) list -> name:string -> t -> (t -> 'a) -> 'a
end

(** Parsed form of the HTTP response *)
module Response : sig
  type t = {
      version: string
    ; frame: bool
    ; code: string
    ; message: string
    ; content_length: int64 option
    ; task: string option
    ; additional_headers: (string * string) list
    ; body: string option
  }

  val empty : t

  val make :
       ?frame:bool
    -> ?version:string
    -> ?length:int64
    -> ?task:string
    -> ?headers:(string * string) list
    -> ?body:string
    -> string
    -> string
    -> t
  (** Returns an instance of type t *)

  val internal_error : t

  val to_string : t -> string
  (** [to_string t] returns a short string summarising [t] *)

  val to_header_list : t -> string list
  (** [to_header_list t] returns the list of HTTP headers associated
      		with [t] *)

  val to_wire_string : t -> string
  (** [to_wire_string t] returns a string which could be sent to a client *)
end

val authorization_of_string : string -> authorization

val http_403_forbidden : ?version:string -> unit -> string list

val http_200_ok : ?version:string -> ?keep_alive:bool -> unit -> string list

val http_200_ok_with_content :
  int64 -> ?version:string -> ?keep_alive:bool -> unit -> string list

val http_302_redirect : ?version:string -> string -> string list

val http_404_missing : ?version:string -> unit -> string list

val http_400_badrequest : ?version:string -> unit -> string list

val http_500_internal_server_error : ?version:string -> unit -> string list

val http_501_method_not_implemented : ?version:string -> unit -> string list

val http_503_service_unavailable : ?version:string -> unit -> string list

module Hdr : sig
  val task_id : string
  (** Header used for task id *)

  val subtask_of : string

  val host : string

  val user_agent : string
  (** Header used for User-Agent string *)

  val content_type : string

  val content_length : string

  val cookie : string

  val transfer_encoding : string

  val authorization : string

  val connection : string

  val acrh : string

  val cache_control : string

  val content_disposition : string

  val accept : string

  val location : string

  val traceparent : string

  val hsts : string
  (** Header used for HTTP Strict Transport Security *)
end

val output_http : Unix.file_descr -> string list -> unit

val parse_cookies : string -> (string * string) list

val urlencode : string -> string
(** Encode parameter suitably for appearing in a query parameter in a URL. *)

type 'a ll = End | Item of 'a * (unit -> 'a ll)

val ll_iter : ('a -> unit) -> 'a ll -> unit

module Url : sig
  type http = {
      host: string
    ; auth: authorization option
    ; port: int option
    ; ssl: bool
  }

  type file = {path: string}

  type scheme = Http of http | File of file

  type data = {uri: string; query_params: (string * string) list}

  type t = scheme * data

  val equal : t -> t -> bool

  val of_string : string -> t

  val to_string : t -> string

  val get_uri : t -> string

  val set_uri : t -> string -> t

  val get_query_params : t -> (string * string) list

  val get_query : t -> string

  val auth_of : t -> authorization option

  val set_ssl : bool -> t -> t
end
