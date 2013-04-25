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

type opt = string * Arg.spec * (unit -> string) * string

type res = {
	name: string;
	description: string;
	essential: bool;
	path: string ref;
	perms: Unix.access_permission list;
}

val configure: ?options:opt list -> ?resources:res list -> unit -> unit

val listen: string -> Unix.file_descr

val daemon: bool ref

val daemonize: unit -> unit

val maybe_daemonize: unit -> unit

val accept_forever: Unix.file_descr -> (Unix.file_descr -> unit) -> unit

type 'a handler =
	(string -> Rpc.call) ->
	(Rpc.response -> string) ->
	('a -> Rpc.call -> Rpc.response) ->
	Unix.file_descr ->
	'a->
	unit

val binary_handler: 'a handler
val http_handler: 'a handler

val wait_forever: unit -> unit


