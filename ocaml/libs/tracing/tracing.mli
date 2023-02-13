(*
 * Copyright (C) 2023 Cloud Software Group
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

(* Basic types *)

type blob

type t = blob option

val empty : t

val null : t -> bool

val t_of_string : string -> t

val string_of_t : t -> string

val json_of_t : t -> string option

(* Create spans *)

val start : name:string -> parent:t -> (t, exn) result

val finish : t -> (unit, exn) result
