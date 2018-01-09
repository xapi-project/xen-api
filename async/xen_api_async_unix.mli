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

val make: ?timeout:float -> string -> Rpc.call -> Rpc.response Async.Deferred.t
(** [make ?timeout uri] returns an 'rpc' function which can be
    passed to Client.* functions *)

val make_json: ?timeout:float -> string -> Rpc.call -> Rpc.response Async.Deferred.t
(** [make_json ?timeout uri] returns an 'rpc' function which can be
    passed to Client.* functions *)

include (module type of (Client.ClientF(struct include Async.Deferred let bind a f = bind a ~f end)))
