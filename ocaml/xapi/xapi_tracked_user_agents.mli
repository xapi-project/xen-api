(*
 * Copyright (C) Cloud Software Group
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

val track : __context:Context.t -> unit
(** [track ~__context] parses and records the user agent from the context.
	Only the name/version part is kept, and only if the string is not too long.
	Oldest entries are evicted if the table is full. *)

val get : unit -> (string * string) list
(** [get ()] returns the list of (name, version) pairs currently tracked. *)

val reset : unit -> unit
(** [reset ()] clears all tracked user agents and the queue. *)
