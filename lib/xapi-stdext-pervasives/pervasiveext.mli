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

val finally : (unit -> 'a) -> (unit -> unit) -> 'a
(** [finally f g] returns [f ()] guaranteeing to run clean-up actions
    [g ()] even if [f ()] throws an exception. *)

val maybe_with_default : 'b -> ('a -> 'b) -> 'a option -> 'b
val may : ('a -> 'b) -> 'a option -> 'b option
val default : 'a -> 'a option -> 'a
val maybe : ('a -> unit) -> 'a option -> unit
val reraise_if : bool -> (unit -> unit) -> unit
val ignore_exn : (unit -> unit) -> unit
val ignore_int : int -> unit
val ignore_int32 : int32 -> unit
val ignore_int64 : int64 -> unit
val ignore_string : string -> unit
val ignore_float : float -> unit
val ignore_bool : bool -> unit

val (++) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
val ($) : ('a -> 'b) -> 'a -> 'b
