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
exception End_of_file
exception Timeout

type t = {
	read : string -> int -> int -> int;
	write : string -> int -> int -> int;
	input_line : (?timeout: float option -> unit -> string) option;
	flush : unit -> unit;
	close : unit -> unit;
	is_raw : bool;
	selectable : Unix.file_descr option;
}

val read : ?timeout: float option -> t -> string -> int -> int -> int
val write : ?timeout: float option -> t -> string -> int -> int -> int
val read_string : ?timeout: float option -> t -> int -> string
val write_string : ?timeout: float option -> t -> string -> unit
val input_line : ?timeout: float option -> t -> string
val flush : t -> unit
val close : t -> unit
