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
(** Client module to handle v6 licenses with the v6 licensing daemon.
 *  V6 licenses enable the features of Citrix Essentials for XenServer.
 * @group Licensing
 *)

val apply_edition : __context:Context.t -> string -> (string * string) list ->
	string * Features.feature list * (string * string) list
val get_editions : unit -> (string * string * string * int) list
val get_version : unit -> string

