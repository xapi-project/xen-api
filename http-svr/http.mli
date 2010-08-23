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
type method_t = Get | Post | Put | Connect | Unknown of string
val string_of_method_t : method_t -> string

(** Exception raised when parsing start line of request *)
exception Http_parse_failure
exception Unauthorised of string
exception Forbidden

type authorization = 
    | Basic of string * string
    | UnknownAuth of string

(** Parsed form of the HTTP request line plus cookie info *)
type request = {
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
    headers: string list;
}

(** Parsed form of the HTTP response *)
module Response : sig
	type t = {
		content_length: int64 option;
		task: string option;
	}
end

val rpc_of_request : request -> Rpc.t 
val request_of_rpc : Rpc.t -> request
 
val nullreq : request
val authorization_of_string : string -> authorization

val parse_uri : string -> string * ((string * string) list)

val request_of_string : string -> request
val pretty_string_of_request : request -> string

(** Marshal a request back into wire-format *)
val string_list_of_request : request -> string list

val http_request : ?version:string -> ?keep_alive:bool -> ?cookie:((string*string) list) -> ?length:(int64) -> user_agent:(string) -> method_t -> string -> string -> string list

val http_403_forbidden : string list
val http_200_ok : ?version:string -> ?keep_alive:bool -> unit -> string list

val http_200_ok_with_content : int64 -> ?version:string -> ?keep_alive:bool -> unit -> string list
val http_302_redirect : string -> string list
val http_404_missing : string list
val http_400_badrequest : string list
val http_401_unauthorised : ?realm:string -> unit -> string list
val http_406_notacceptable : string list
val http_500_internal_error : string list

(** Header used for task id *)
val task_id_hdr : string
val subtask_of_hdr : string

(** Header used for User-Agent string *)
val user_agent_hdr : string

val content_type_hdr : string

val output_http : Unix.file_descr -> string list -> unit

val strip_cr : string -> string

(* debugging function: *)
val myprint : ('a, unit, string, unit) format4 -> 'a

val end_of_string : string -> int -> string
val parse_keyvalpairs : string -> (string * string) list

val escape : string -> string
val urlencode : string -> string

type 'a ll = End | Item of 'a * (unit -> 'a ll)
val ll_iter : ('a -> unit) -> 'a ll -> unit
