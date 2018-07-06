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

(* Perhaps it's better to use `option' from the ocaml-extlib extension
 * to the standard library instead?  (Although it would not suffice,
 * since it's not a super-set of our `opt'.)
 * (http://code.google.com/p/ocaml-extlib/)
*)

module Monad = Monad.M1.Make (struct

    type 'a m = 'a option

    let bind option f =
      match option with
      | None -> None
      | Some result -> f result

    let return x = Some x

  end)

let iter f = function
  | Some x -> f x
  | None -> ()

let map f = function
  | Some x -> Some(f x)
  | None -> None

let default d = function
  | Some x -> x
  | None -> d

let unbox = function
  | Some x -> x
  | None -> raise Not_found

let is_boxed = function
  | Some _ -> true
  | None -> false

let is_some = is_boxed

let is_none = function
  | Some _ -> false
  | None -> true

let to_list = function
  | Some x -> [x]
  | None -> []

let fold_left f accu = function
  | Some x -> f accu x
  | None -> accu

let fold_right f opt accu =
  match opt with
  | Some x -> f x accu
  | None -> accu

let join = function
  | Some (Some a) -> Some a
  | _ -> None

let of_exception f =
  try Some (f ())
  with _ -> None

