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

let to_list tbl =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

(* this is not a fold ... *)
let fold_keys tbl =
  Hashtbl.fold (fun k _ acc -> k :: acc) tbl []

(* ... neither is this *)
let fold_values tbl =
  Hashtbl.fold (fun _ v acc -> v :: acc) tbl []

let add_empty tbl k v =
  if not (Hashtbl.mem tbl k) then
    Hashtbl.add tbl k v

let add_list tbl l =
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) l

let remove_other_keys tbl valid_keys =
  let keys = fold_keys tbl in
  let maybe_remove k =
    if not (List.mem k valid_keys) then Hashtbl.remove tbl k in
  List.iter maybe_remove keys

let of_list l =
  let tbl = Hashtbl.create (List.length l) in
  add_list tbl l;
  tbl
