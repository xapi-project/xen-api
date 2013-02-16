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

type spec = string * Arg.spec * (unit -> string) * string

val configure: spec list -> unit

val listen: string -> Unix.file_descr

val daemonize: unit -> unit

val accept_forever: Unix.file_descr -> (Unix.file_descr -> unit) -> unit

val binary_handler:
	(string -> Rpc.call) ->
	(Rpc.response -> string) ->
	('a -> Rpc.call -> Rpc.response) ->
	Unix.file_descr ->
	'a->
	unit

val wait_forever: unit -> unit


