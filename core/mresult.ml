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

let bind v f = match v with `Ok v -> f v | `Error _ as e -> e
let map v f = match v with `Ok v -> `Ok (f v) | `Error _ as e -> e
let join r = match r with `Ok v -> v | `Error _ as e -> e
let ( >>= ) = bind
let ( >>| ) = map

let return x = `Ok x
let ok = return
let fail x = `Error (`Msg x)

let all xs =
  let rec loop acc = function
  | [] -> return (List.rev acc)
  | `Ok x :: xs -> loop (x :: acc) xs
  | `Error x :: _ -> `Error x in
  loop [] xs

let get_ok = function
| `Ok x -> x
| `Error _ -> raise (Invalid_argument "get_ok encountered an `Error")

let get_error = function
| `Error x -> x
| `Ok _ -> raise (Invalid_argument "get_error encountered an `Ok")
