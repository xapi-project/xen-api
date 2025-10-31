(*
 * Copyright (C) 2012 Citrix Systems Inc.
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

val make : ?timeout:float -> Uri.t -> Rpc.call -> Rpc.response Lwt.t
(** [make ?timeout uri] returns an 'rpc' function which can be
    passed to Client.* functions *)

val make_json : ?timeout:float -> Uri.t -> Rpc.call -> Rpc.response Lwt.t
(** [make ?timeout uri] returns an 'rpc' function which can be
    passed to Client.* functions *)

val uri_local_json : Uri.t
(** [uri_local_json] is the Uri of the local Unix domain socket using the JSON protocol.
    Should be passed to [make_json].

    There is no standard for Unix sockets with HTTP protocol, so the Uri uses the non-standard 'http+unix' scheme
    with the Unix domain socket encoded into the hostname part of the Uri.
    make_json knows how to handle that, if you intend to use this Uri with other HTTP libraries you'll likely need to write a custom resolver.
*)

val uri_ip_json : string -> Uri.t
(** [uri_ip_json ip] is an Uri to connect to [ip] using the preferred protocols. Currently this is 'https://' using JSONRPC.
    Should be passed to [make_json]. *)

include module type of Client.ClientF (Lwt)

module Lwt_unix_IO : sig
  type ic = (unit -> unit Lwt.t) * Lwt_io.input_channel

  type oc = (unit -> unit Lwt.t) * Lwt_io.output_channel

  val open_connection : Uri.t -> (ic * oc, exn) Xen_api.result Lwt.t
end

module SessionCache : sig
  (** a session cache for a specific user *)
  type t

  val create_uri :
       ?timeout:float
    -> switch:Lwt_switch.t
    -> target:Uri.t
    -> uname:string
    -> pwd:string
    -> version:string
    -> originator:string
    -> unit
    -> t
  (** [create_uri ?timeout ?target ~uname ~pwd ~version ~originator ()]

    An RPC function is constructed for [target], and a session is retrieved.

    @param timeout timeout between XAPI calls
    @param switch registers {!val:destroy} to be called when the {!val:Lwt_switch.t} is turned off
    @param target XAPI host Uri for JSONRPC
    @param uname XAPI user name
    @param pwd password for [uname]
    @param version client application version
    @param originator client application user agent
  *)

  val create_rpc :
       ?timeout:float
    -> (Rpc.call -> Rpc.response Lwt.t)
    -> uname:string
    -> pwd:string
    -> version:string
    -> originator:string
    -> unit
    -> t
  (** [create_rpc ?timeout rpc ~uname ~pwd ~version ~originator ()] is a session cache for the specified [uname] user.

    A session is retrieved using the [rpc] function.

    @param timeout timeout between XAPI calls
    @param rpc an rpc function
    @param uname XAPI user name
    @param pwd password for [uname]
    @param version client application version
    @param originator client application user agent

    @see {!val:create_uri}
  *)

  val with_session :
       t
    -> (   rpc:(Rpc.call -> Rpc.response Lwt.t)
        -> session_id:API.ref_session
        -> 'a Lwt.t
       )
    -> 'a Lwt.t
  (** [with_session t f] calls [f ~rpc ~session_id] with a valid logged in session and an [rpc] function to use. *)

  val destroy : t -> unit Lwt.t
  (** [destroy t] logs out all sessions from the cache. *)
end
