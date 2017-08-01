(*
 * Copyright (C) 2010-2011 Citrix Systems Inc.
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

(** 1-parameter monads. *)
module M1 = struct

  module type BASE =
  sig
    type 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m
    val return : 'a -> 'a m
  end

  module type MONAD =
  sig
    type 'a m
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
    val bind : 'a m -> ('a -> 'b m) -> 'b m
    val return : 'a -> 'a m
  end

  module Make (B : BASE) : MONAD with type 'a m = 'a B.m =
  struct
    type 'a m = 'a B.m
    let (>>=) = B.bind
    let bind = B.bind
    let return = B.return
  end

end

(** 2-parameter monads. *)
module M2 = struct

  module type BASE =
  sig
    type ('a, 'x) m
    val bind : ('a, 'x) m -> ('a -> ('b, 'x) m) -> ('b, 'x) m
    val return : 'a -> ('a, 'x) m
  end

  module type MONAD =
  sig
    type ('a, 'x) m
    val (>>=) : ('a, 'x) m -> ('a -> ('b, 'x) m) -> ('b, 'x) m
    val bind : ('a, 'x) m -> ('a -> ('b, 'x) m) -> ('b, 'x) m
    val return : 'a -> ('a, 'x) m
  end

  module Make (B : BASE) : MONAD with type ('a, 'x) m = ('a, 'x) B.m =
  struct
    type ('a, 'x) m = ('a, 'x) B.m
    let (>>=) = B.bind
    let bind = B.bind
    let return = B.return
  end

end

