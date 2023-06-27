(*
 * Copyright (C) Cloud Software Group.
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

let string_of_features features =
  Array.map (Printf.sprintf "%08Lx") features
  |> Array.to_list
  |> String.concat "-"

exception InvalidFeatureString of string

let features_of_string str =
  let scanf fmt s = Scanf.sscanf s fmt (fun x -> x) in
  try
    String.split_on_char '-' str
    |> (fun lst -> if lst = [""] then [] else lst)
    |> Array.of_list
    |> Array.map (scanf "%08Lx%!")
  with _ -> raise (InvalidFeatureString str)

(** If arr0 is shorter than arr1, extend arr0 with elements from arr1 up to the
 *  length of arr1. Otherwise, truncate arr0 to the length of arr1. *)
let extend arr0 arr1 =
  let new_arr = Array.copy arr1 in
  let len = min (Array.length arr0) (Array.length arr1) in
  Array.blit arr0 0 new_arr 0 len ;
  new_arr

(** If arr is shorter than len elements, extend with zeros up to len elements.
 *  Otherwise, truncate arr to len elements. *)
let zero_extend arr len =
  let zero_arr = Array.make len 0L in
  extend arr zero_arr

let features_op2 f a b =
  let n = max (Array.length a) (Array.length b) in
  Array.map2 f (zero_extend a n) (zero_extend b n)

(** Calculate the intersection of two feature sets.
 *  Intersection with the empty set is treated as identity, so that intersection
 *  can be folded easily starting with an accumulator of [||].
 *  If both sets are non-empty and of differing lengths, and one set is longer
 *  than the other, the shorter one is zero-extended to match it.
 *  The returned set is the same length as the longer of the two arguments.  *)
let intersect left right =
  match (left, right) with
  | [||], _ ->
      right
  | _, [||] ->
      left
  | _, _ ->
      features_op2 Int64.logand left right

(** Calculate the features that are missing from [right],
  * but present in [left] *)
let diff left right =
  let diff64 a b = Int64.(logand a (lognot b)) in
  features_op2 diff64 left right

(** equality check that zero-extends if lengths differ *)
let is_equal left right =
  let len = max (Array.length left) (Array.length right) in
  zero_extend left len = zero_extend right len

(** is_subset left right returns true if left is a subset of right *)
let is_subset left right = is_equal (intersect left right) left

(** is_strict_subset left right returns true if left is a strict subset of right
    (left is a subset of right, but left and right are not equal) *)
let is_strict_subset left right =
  is_subset left right && not (is_equal left right)
