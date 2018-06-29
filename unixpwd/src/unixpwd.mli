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

exception Error of string

val getpwd :   string -> string
(* [getpwd user] returns the (encrypted) password for [user] from the
 * /etc/passwd database. It raised [Error] on error. *)


val getspw :   string -> string
(* [getspw user] returns the (encrypted) password for [user] from the
 * /etc/shadow database. It raises [Error] on error. *)

val get    :   string -> string
(* [get user] returns the (encrypted) password for [user] from the
 * /etc/shadow database if an entry exists, otherwise it tries to
 * obtain the password from the /etc/passwd database. It raises [Error]
 * if that fails.
 **)

val setpwd :   string -> string -> unit
val setspw :   string -> string -> unit
(* [setpwd user pw] and [setspw user pw] set the (encrypted) password
 * for [user] in /etc/passwd and /etc/shadow, respectively. They raise
 * [Error] on error.
 *)

val unshadow : unit   -> string
(* [unshadow] returns the contents of /etc/passwd as a string with
 * passwords from /etc/shadow for users that have a corresponding entry.
 * Raises [Error] on error.
 *)
