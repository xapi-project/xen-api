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

val common_prefix: string

type opt = string * Arg.spec * (unit -> string) * string

module Config_file : sig
  val parse_line : string -> (string * string) option
end

type res = {
	name: string;
	description: string;
	essential: bool;
	path: string ref;
	perms: Unix.access_permission list;
}

val configure: ?options:opt list -> ?resources:res list -> unit -> unit

type ('a, 'b) error = [
  | `Ok of 'a
  | `Error of 'b
]

val configure2:
  name:string ->
  version:string ->
  doc:string ->
  ?options:opt list -> ?resources:res list -> unit ->
  (unit, string) error
(** More advanced service configuration with manpage generation *)

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
val loglevel: unit -> Syslog.level

val daemonize: ?start_fn:(unit -> unit) -> unit -> unit

val maybe_daemonize: ?start_fn:(unit -> unit) -> unit -> unit
