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

val make: ?timeout:float -> string -> Rpc.call -> Rpc.response Lwt.t
(** [make ?timeout uri] returns an 'rpc' function which can be
	passed to Client.* functions *)

val make_json: ?timeout:float -> string -> Rpc.call -> Rpc.response Lwt.t
(** [make ?timeout uri] returns an 'rpc' function which can be
	passed to Client.* functions *)

include (module type of (Client.ClientF(Lwt)))

module Lwt_unix_IO : sig

  type ic = (unit -> unit Lwt.t) * Lwt_io.input_channel
  type oc = (unit -> unit Lwt.t) * Lwt_io.output_channel

  val open_connection: Uri.t -> ((ic * oc), exn) Xen_api.result Lwt.t
end

