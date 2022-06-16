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

val common_prefix : string

type opt = string * Arg.spec * (unit -> string) * string

module Config_file : sig
  val parse_line : string -> (string * string) option
end

type res = {
    name: string
  ; description: string
  ; essential: bool
  ; path: string ref
  ; perms: Unix.access_permission list
}

val configure : ?options:opt list -> ?resources:res list -> unit -> unit

val configure2 :
     name:string
  -> version:string
  -> doc:string
  -> ?options:opt list
  -> ?resources:res list
  -> unit
  -> unit
(** More advanced service configuration with manpage generation *)

type server

val make_socket_server : string -> (Unix.file_descr -> unit) -> server

val make :
     path:string
  -> queue_name:string
  -> ?raw_fn:(Unix.file_descr -> unit)
  -> rpc_fn:(Rpc.call -> Rpc.response)
  -> unit
  -> server

val serve_forever : server -> unit

val daemon : bool ref

val loglevel : unit -> Syslog.level

val daemonize : ?start_fn:(unit -> unit) -> unit -> unit

val maybe_daemonize : ?start_fn:(unit -> unit) -> unit -> unit

val cli :
     name:string
  -> doc:string
  -> version:Rpc.Version.t
  -> cmdline_gen:(unit -> ('a Cmdliner.Term.t * Cmdliner.Cmd.info) list)
  -> 'a Cmdliner.Cmd.t
(** [cli ~name ~doc ~version ~cmdline_gen] creates a [Cmdliner] cli parser with
    the subcommands defined by [cmdline_gen] which by default prints the
    manpage. The resulting parser needs to be evaluated for the current args *)

val eval_cmdline : (unit -> unit) Cmdliner.Cmd.t -> unit
(** [eval_cmdline cli] evaluates the cli parser [cli] for the parsers usually
    generated using [cli] with the [rpclib] cli generator *)
