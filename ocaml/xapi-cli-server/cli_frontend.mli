(*
 * Copyright (C) 2026 Vates.
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

(** Interface of the Cli_frontend module *)

exception ParseError of string

exception ParamNotFound of string

type commandline

val get_params : commandline -> (string * string) list

val get_cmdname : commandline -> string

val get_reqd_param : commandline -> string -> string

val cmdtable : (string, Cli_cmdtable.cmd_spec) Hashtbl.t

val populate_cmdtable : Cli_operations.rpc -> API.ref_session -> unit

val cmd_help : (Cli_printer.printval -> unit) -> bool -> commandline -> unit

val parse_commandline : string list -> commandline
