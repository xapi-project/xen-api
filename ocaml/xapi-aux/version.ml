(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

(* Simple abstraction for version information that enforces a simple
   format and predicatable semantics *)

exception Format of string

(** in decreasing oder of sginificance *)
type t = int list

let of_string str =
  let int str = Scanf.sscanf str "%u%!" Fun.id in
  try String.split_on_char '.' str |> List.map int with _ -> raise (Format str)

let to_string t =
  let str int = Printf.sprintf "%d" int in
  t |> List.map str |> String.concat "."

(** Total order over versions; 1.2.3 is equal to 1.2.3.0 *)
let rec compare v1 v2 =
  match (v1, v2) with
  | [], [] ->
      0
  | 0 :: xs, [] ->
      compare xs []
  | _, [] ->
      1
  | [], 0 :: ys ->
      compare [] ys
  | [], _ ->
      -1
  | x :: xs, y :: ys when x = y ->
      compare xs ys
  | x :: _, y :: _ when x < y ->
      -1
  | _ ->
      1

let ne x y = compare x y <> 0

let eq x y = compare x y = 0

let le x y = compare x y <= 0

let ge x y = compare x y >= 0

let gt x y = compare x y > 0

let lt x y = compare x y < 0

let is_valid str =
  try
    ignore (of_string str) ;
    true
  with Format _ -> false

module String = struct
  let wrap f v1 v2 = f (of_string v1) (of_string v2)

  let compare = wrap compare

  let ne = wrap ne

  let eq = wrap eq

  let le = wrap le

  let ge = wrap ge

  let gt = wrap gt

  let lt = wrap lt
end
