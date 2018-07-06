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

module Monad : sig include Monad.M1.MONAD with type 'a m = 'a option end
val iter : ('a -> unit) -> 'a option -> unit
val map : ('a -> 'b) -> 'a option -> 'b option
val default : 'a -> 'a option -> 'a
val unbox : 'a option -> 'a
val is_boxed : 'a option -> bool
val is_some : 'a option -> bool
val is_none : 'a option -> bool
val to_list : 'a option -> 'a list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
val join : ('a option) option -> 'a option
val of_exception : (unit -> 'a) -> 'a option
