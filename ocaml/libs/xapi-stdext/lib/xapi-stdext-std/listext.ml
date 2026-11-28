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

module List = struct
  open! List

  (** Turn a list into a set *)
  let rec setify = function
    | [] ->
        []
    | x :: xs ->
        if mem x xs then
          setify xs
        else
          x :: setify xs

  let subset s1 s2 = List.for_all (fun s -> List.mem s s2) s1

  let set_equiv s1 s2 = subset s1 s2 && subset s2 s1

  (* Tail-recursive map. *)

  let[@tail_mod_cons] rec map_tr f l =
    match l with
    | [] ->
        []
    | [x] ->
        [f x]
    | x1 :: x2 :: xs ->
        let fx1 = f x1 in
        let fx2 = f x2 in
        fx1 :: fx2 :: map_tr f xs

  let position pred l =
    let aux (i, is) e =
      ( i + 1
      , if pred e then
          i :: is
        else
          is
      )
    in
    snd (fold_left aux (0, []) l)

  let try_map f l =
    let rec loop acc = function
      | [] ->
          Ok (List.rev acc)
      | x :: xs -> (
        match f x with Ok x -> loop (x :: acc) xs | Error _ as e -> e
      )
    in
    loop [] l

  let try_map_collect f l =
    let rec loop acc = function
      | [] ->
          Ok (List.rev acc)
      | x :: xs -> (
        match f x with
        | Ok r ->
            loop (r :: acc) xs
        | Error e ->
            Error (List.rev acc, e)
      )
    in
    loop [] l

  let try_map_any f l =
    let rec loop errs = function
      | [] ->
          Error (List.rev errs)
      | x :: xs -> (
        match f x with Ok _ as ok -> ok | Error e -> loop (e :: errs) xs
      )
    in
    loop [] l

  let take n list =
    let rec loop i acc = function
      | x :: xs when i < n ->
          loop (i + 1) (x :: acc) xs
      | _ ->
          List.rev acc
    in
    loop 0 [] list

  let drop n list =
    let rec loop i = function
      | _ :: xs when i < n ->
          loop (i + 1) xs
      | l ->
          l
    in
    loop 0 list

  let head = function [] -> None | x :: _ -> Some x

  let rec last = function [] -> None | [x] -> Some x | _ :: xs -> last xs

  let split_at n list =
    let rec loop i acc = function
      | x :: xs when i < n ->
          loop (i + 1) (x :: acc) xs
      | xs ->
          (List.rev acc, xs)
    in
    loop 0 [] list

  let rec between e = function
    | [] ->
        []
    | [h] ->
        [h]
    | h :: t ->
        h :: e :: between e t

  let intersect xs ys = List.filter (fun x -> List.mem x ys) xs

  let set_difference a b = List.filter (fun x -> not (List.mem x b)) a

  let assoc_default k l d = Option.value ~default:d (List.assoc_opt k l)

  let replace_assoc key new_value existing =
    (key, new_value) :: List.filter (fun (k, _) -> k <> key) existing

  let update_assoc update existing =
    update @ List.filter (fun (k, _) -> not (List.mem_assoc k update)) existing

  let find_index f l =
    let rec loop i = function
      | [] ->
          None
      | x :: _ when f x ->
          Some i
      | _ :: xs ->
          loop (i + 1) xs
    in
    loop 0 l
end
