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

type authorization = 
    | Basic of string * string
    | UnknownAuth of string

val read_http_header: string -> Unix.file_descr -> int

val read_http_request_header: string -> Unix.file_descr -> int
val read_http_response_header: string -> Unix.file_descr -> int

(** Parsed form of the HTTP request line plus cookie info *)
module Request : sig
	type t = {
		m: method_t;
		uri: string;
		query: (string*string) list;
		version: string;
		transfer_encoding: string option;
		content_length: int64 option;
		auth: authorization option;
		cookie: (string * string) list;
		task: string option;
		subtask_of: string option;
		content_type: string option;
		user_agent: string option;
		mutable close: bool;
		additional_headers: (string*string) list;
		body: string option;
	}

	val rpc_of_t : t -> Rpc.t
	val t_of_rpc : Rpc.t -> t
	  
	val empty: t

	(** [make] is the standard constructor for [t] *)
	val make: ?version:string -> ?keep_alive:bool -> ?cookie:(string*string) list -> ?length:int64 -> ?subtask_of:string -> ?body:string -> ?headers:(string*string) list -> ?content_type:string -> user_agent:string -> method_t -> string -> t

	(** [get_version t] returns the HTTP protocol version *)
	val get_version: t -> string

	(** [of_request_line l] parses [l] of the form "METHOD HTTP/VERSION" and
		returns the corresponding [t] *)
	val of_request_line: string -> t

	(** [to_string t] returns a short string summarising [t] *)
	val to_string: t -> string

	(** [to_header_list t] returns the list of HTTP headers associated
		with [t] *)
	val to_header_list: t -> string list

	(** [to_wire_string t] returns a string which could be sent to a server *)
	val to_wire_string: t -> string
end

(** Parsed form of the HTTP response *)
module Response : sig
	type t = {
		version: string;
		code: string;
		message: string;
		content_length: int64 option;
		task: string option;
		additional_headers: (string*string) list;
		body: string option
	}

	val empty: t

	(** Returns an instance of type t *)
	val make: ?version:string -> ?length:int64 -> ?task:string -> ?headers:(string*string) list -> ?body:string -> string -> string -> t

	val internal_error: t

	(** [to_string t] returns a short string summarising [t] *)
	val to_string: t -> string

	(** [to_header_list t] returns the list of HTTP headers associated
		with [t] *)
	val to_header_list: t -> string list

	(** [to_wire_string t] returns a string which could be sent to a client *)
	val to_wire_string: t -> string
end

val authorization_of_string : string -> authorization

val parse_uri : string -> string * ((string * string) list)

val http_403_forbidden : ?version:string -> unit -> string list
val http_200_ok : ?version:string -> ?keep_alive:bool -> unit -> string list

val http_200_ok_with_content : int64 -> ?version:string -> ?keep_alive:bool -> unit -> string list
val http_302_redirect : ?version:string -> string -> string list
val http_404_missing : ?version:string -> unit -> string list
val http_400_badrequest : ?version:string -> unit -> string list
val http_401_unauthorised : ?version:string -> ?realm:string -> unit -> string list
val http_406_notacceptable : ?version:string -> unit -> string list
val http_500_internal_error : ?version:string -> unit -> string list
val http_501_method_not_implemented : ?version:string -> unit -> string list

module Hdr : sig
	(** Header used for task id *)
	val task_id: string
	val subtask_of: string
	(** Header used for User-Agent string *)
	val user_agent: string
	val content_type: string
	val content_length: string
	val cookie: string
	val transfer_encoding: string
	val authorization: string
	val connection: string
end

val output_http : Unix.file_descr -> string list -> unit

val parse_keyvalpairs : string -> (string * string) list

val urlencode : string -> string

type 'a ll = End | Item of 'a * (unit -> 'a ll)
val ll_iter : ('a -> unit) -> 'a ll -> unit
