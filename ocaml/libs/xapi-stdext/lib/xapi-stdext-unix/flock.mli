(*
 * Copyright (C) 2026 Cloud Software Group
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

(** Locks on files with flock(2), a file per key in a directory, checked before
    they are opened. A one-line mark that starts with [magic] attests a file;
    only this module's fields of it are checked. *)

include Lock_rw.S

val create : boot_id:(unit -> string) -> magic:string -> string -> t
(** [create ~boot_id ~magic dir] does no input or output. [boot_id ()] names
    the current boot in the mark; it is read at each check of a mark. A key
    names the file [<key>.lock] in [dir], and its mark: printable ASCII
    without '/', '=' or a space, not starting with '.' or '#'. *)

val field : string -> string -> field option
(** [None] for one of this module's own names, a name with '=', or a name or
    value with a space or another byte that is not printable *)
