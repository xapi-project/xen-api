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

(** Allow settings to be stored in a config file as key = value pairs,
	as well as parsed directly from the commandline. *)

val parse_line: string -> (string * Arg.spec * string) list -> unit
(** [parse_string data args] parses the string [data], looking for 
    "key = value\n" and, if found, applies the rules from [args] as
	if the values had been passed directly on the command line. *)

val parse_file: string -> (string * Arg.spec * string) list -> unit
(** [parse_file filename args] parses [filename] as a sequence of "key = value\n"
	and applies the rules from [args] as if the values had been passed
	directly on the command line. *)
