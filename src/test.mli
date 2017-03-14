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


module NoOpMonad :
sig
  type 'a t = 'a
  val return : 'a -> 'a
  val bind : 'a -> ('a -> 'b) -> 'b
end

module StringMonad :
sig
  type 'a t = { data : 'a; str : string; }
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val strwr : string -> unit t
  val getstr : 'a t -> string
  val getdata : 'a t -> 'a
end
