(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

type ('a, 'b) result = [
| `Ok of 'a 
| `Error of 'b
]

include Monad.S2 with type ('a, 'b) t := ('a, 'b) result

val ok: 'a -> ('a, 'b) result

val all: ('a, 'b) result list -> ('a list, 'b) result

val get_ok: ('a, 'b) result -> 'a
val get_error: ('a, 'b) result -> 'b
