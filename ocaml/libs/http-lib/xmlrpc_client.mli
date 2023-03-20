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

(** Thrown when ECONNRESET is caught which suggests the remote crashed
    	or restarted *)
exception Connection_reset

(** Thrown when repeated attempts to connect an stunnel to a remote host
    	and check the connection works fail. *)
exception Stunnel_connection_failed

module SSL : sig
  (** A desired SSL configuration *)
  type t

  val make :
       ?use_fork_exec_helper:bool
    -> ?use_stunnel_cache:bool
    -> verify_cert:Stunnel.verification_config option
    -> ?task_id:string
    -> unit
    -> t
  (** [make] is used to create a type [t] *)
end

(** Understood low-level transport types *)
type transport =
  | Unix of string  (** Unix domain socket *)
  | TCP of string * int  (** Plain TCP/IP with a host, port *)
  | SSL of SSL.t * string * int  (** SSL over TCP/IP with a host, port *)

val transport_of_url :
  verify_cert:Stunnel.verification_config option -> Http.Url.t -> transport
(** [transport_of_url url] returns the transport associated with [url] *)

val string_of_transport : transport -> string
(** [string_of_transport t] returns a debug-friendly version of [t] *)

val with_transport :
  ?stunnel_wait_disconnect:bool -> transport -> (Unix.file_descr -> 'a) -> 'a
(** [with_transport transport f] calls [f fd] with [fd] connected via
    	the requested [transport] *)

val with_http :
     Http.Request.t
  -> (Http.Response.t * Unix.file_descr -> 'a)
  -> Unix.file_descr
  -> 'a
(** [with_http request f] calls [f (r, fd)] where [r] is the HTTP response
    received after sending HTTP [request] and [fd] is still connected to
    	the client. *)

val xmlrpc :
     ?frame:bool
  -> ?version:string
  -> ?keep_alive:bool
  -> ?task_id:string
  -> ?cookie:(string * string) list
  -> ?length:int64
  -> ?auth:Http.authorization
  -> ?subtask_of:string
  -> ?query:(string * string) list
  -> ?body:string
  -> string
  -> Http.Request.t
(** Returns an HTTP.Request.t representing an XMLRPC request *)

val connect :
     ?session_id:string
  -> ?task_id:string
  -> ?subtask_of:string
  -> string
  -> Http.Request.t
(** Returns an HTTP.Request.t representing an HTTP CONNECT *)

module XML_protocol : sig
  (** Functions for handling HTTP/XML (not necessarily XMLRPC) *)

  val read_response : Http.Response.t -> Unix.file_descr -> Xml.xml
  (** [read_response r fd] returns the XML from [fd] given
      		HTTP request [r] *)

  val rpc :
       ?srcstr:string
    -> ?dststr:string
    -> transport:transport
    -> http:Http.Request.t
    -> Xml.xml
    -> Xml.xml
  (** [rpc transport http xml] sends [xml] and returns the result *)
end

module XMLRPC_protocol : sig
  (** Functions for handling HTTP/XML via rpc-light *)

  val read_response : Http.Response.t -> Unix.file_descr -> Rpc.response
  (** [read_response r fd] returns the response from [fd] given
      		HTTP request [r] *)

  val rpc :
       ?srcstr:string
    -> ?dststr:string
    -> transport:transport
    -> http:Http.Request.t
    -> Rpc.call
    -> Rpc.response
  (** [rpc transport http call] sends [call] and returns the result *)
end

module JSONRPC_protocol : sig
  (** Functions for handling HTTP/JSONRPC *)

  val read_response : Http.Response.t -> Unix.file_descr -> Rpc.response
  (** [read_response r fd] returns the response from [fd] given HTTP request [r] *)

  val rpc :
       ?srcstr:string
    -> ?dststr:string
    -> transport:transport
    -> http:Http.Request.t
    -> Rpc.call
    -> Rpc.response
  (** [rpc transport http call] sends [call] and returns the result *)
end

module Internal : sig
  (** Internal functions should not be used by clients directly *)

  val set_stunnelpid_callback : (string option -> int -> unit) option ref
  (** When invoking an XMLRPC call over HTTPS via stunnel, this callback
      		is called to allow us to store the association between a task and an
      		stunnel pid *)

  val unset_stunnelpid_callback : (string option -> int -> unit) option ref
  (** After invoking an XMLRPC call over HTTPS via stunnel, this callback
      		is called to allow us to forget the association between a task and an
      		stunnel pid *)

  val destination_is_ok : (string -> bool) option ref
  (** Callback to check whether a destination address is still OK. Only called after
      a failed attempt to talk to the destination *)
end
