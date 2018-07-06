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

exception Parse_error

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

(** C# and JS representation of special floats are 'NaN' and 'Infinity' which
    are different from ocaml's native representation. Caml is fortunately more
    forgiving when doing a float_of_string, and can cope with these forms, so
    we make a generic float_to_string function here *)
let f_to_s f = 
  match classify_float f with 
  | FP_normal | FP_subnormal -> Printf.sprintf "%0.4f" f
  | FP_nan -> "NaN"
  | FP_infinite -> if f>0.0 then "Infinity" else "-Infinity"
  | FP_zero -> "0.0"

module Xmlm_utils = struct
  let tag n = ("", n), []
  let start_tag n = (`El_start (tag n))
  let accept s i = if Xmlm.input i = s then () else raise Parse_error

  let rec iter_seq el acc i = match Xmlm.peek i with
    | `El_start _ -> iter_seq el ((el i) :: acc) i
    | `El_end -> List.rev acc
    | _ -> raise Parse_error

  let get_el n i =
    if Xmlm.input i = (start_tag n) then
      let d = match Xmlm.peek i with
        | `Data d -> ignore (Xmlm.input i); d
        | `El_end -> ""
        | _ -> raise Parse_error
      in
      accept (`El_end) i;
      d
    else raise Parse_error

  let rec read_all t read_f i acc =
    if (Xmlm.peek i) = (start_tag t) 
    then read_all t read_f i ((read_f i) :: acc)
    else List.rev acc

  let read_block t f i =
    accept (start_tag t) i;
    let res = f i in
    accept (`El_end) i;
    res
end
