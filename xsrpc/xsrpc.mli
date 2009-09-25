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

type t

exception Stop_listen
exception Timeout
exception Protocol_error of string

type status = Error | Success

val bind : int -> string -> t
val query_with_id : t -> string -> string -> string -> (status * string)
val query : t -> string -> string -> (status * string)
val listen : string -> (int -> string -> string -> string -> (status * string) option) -> unit
