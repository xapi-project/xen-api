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

(* Utils, some from stdext originally *)

(**
 * @group Performance Monitoring
*)

let isnan x = match classify_float x with | FP_nan -> true | _ -> false

let array_index e a =
  let len = Array.length a in
  let rec check i =
    if len <= i then -1
    else if Array.get a i = e then i
    else check (i + 1)
  in check 0   

let array_remove n a =
  Array.append (Array.sub a 0 n) (Array.sub a (n+1) (Array.length a - n - 1))

let filter_map f list =
  let rec inner acc l =
    match l with
    | [] -> List.rev acc
    | x::xs -> 
      let acc = match f x with | Some res -> res::acc | None -> acc in
      inner acc xs
  in
  inner [] list

let rec setify = function
  | [] -> []
  | (x::xs) -> if List.mem x xs then setify xs else x::(setify xs)
