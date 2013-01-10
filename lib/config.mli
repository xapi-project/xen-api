(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
exception Error of (string * string) list

type ty =
	| Set_bool of bool ref
	| Set_int of int ref
	| Set_string of string ref
	| Set_float of float ref
	| Unit of (unit -> unit)
	| Bool of (bool -> unit)
	| Int of (int -> unit)
	| String of (string -> unit)
	| Float of (float -> unit)

val read: string -> (string * ty) list -> (string -> string -> unit) -> unit
