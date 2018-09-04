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
(** Some string handling functions to help drawing text tables.
    Modified from Richard's code in the CLI *)

let pad n s before =
  if String.length s>n then
    (if String.length s > 2 then
       (String.sub s 0 (n-2))^".."
     else
       String.sub s 0 n)
  else
    let padding = String.make (n-(String.length s)) ' ' in
    if before then padding^s else s^padding

let left n s = pad n s false
let right n s = pad n s true

let compute_col_widths rows =
  let mkints n = let rec f x = if x = n then [] else x :: (f (x+1)) in f 0 in
  let numcols = List.length (List.hd rows) in
  let column x = List.map (fun row -> List.nth row x) rows in
  let cols = List.map column (mkints numcols) in

  let max n str = max n (String.length str) in
  List.map (List.fold_left max 0) cols

let print (rows: string list list) = match rows with
  | [] -> ()
  | _ ->
    let widths = compute_col_widths rows in
    let sll = List.map (List.map2 right widths) rows in
    List.iter (fun line -> print_endline (String.concat " | " line)) sll
