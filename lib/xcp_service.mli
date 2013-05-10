(*
 * Copyright (C) Citrix Systems Inc.
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

module type BRAND = sig val name: string end

module Debug : sig
	module Make(Brand: BRAND): sig
		val debug : ('a, unit, string, unit) format4 -> 'a
		val info  : ('a, unit, string, unit) format4 -> 'a
		val warn  : ('a, unit, string, unit) format4 -> 'a
		val error : ('a, unit, string, unit) format4 -> 'a

	end
end

val common_prefix: string

val sockets_group: string ref

type opt = string * Arg.spec * (unit -> string) * string

type res = {
	name: string;
	description: string;
	essential: bool;
	path: string ref;
	perms: Unix.access_permission list;
}

val configure: ?options:opt list -> ?resources:res list -> unit -> unit

type server

val make_socket_server: string -> (Unix.file_descr -> unit) -> server

val make: path:string ->
	queue_name:string ->
	?raw_fn: (Unix.file_descr -> unit) ->
	rpc_fn: (Rpc.call -> Rpc.response) ->
	unit ->
	server

val serve_forever: server -> unit

val daemon: bool ref

val daemonize: unit -> unit

val maybe_daemonize: unit -> unit

