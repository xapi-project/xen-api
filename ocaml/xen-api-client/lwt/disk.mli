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

type authentication =
  | Session_id of string
  (** use an existing session_id *)

  | UserPassword of string * string
  (** login with this username and password, then log out afterwards *)

val uri: pool:Uri.t -> authentication:authentication -> vdi:API.ref_VDI -> Uri.t
(** [uri pool auth vdi] constructs a URI which references the remote disk [vdi]
    in the pool referenced by URI [uri] via authentication method
    [authentication]. *)

val start_upload: chunked:bool -> uri:Uri.t -> Data_channel.t Lwt.t
(** [start_upload chunked uri] connects to the remote [uri] and offers
    either a raw encoding or a chunked encoding (depending on [chunked]).
    The disk contents should be written using the Data_channel.t. Closing
    the connection finishes the upload. *)
