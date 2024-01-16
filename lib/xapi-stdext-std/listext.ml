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
        if mem x xs then setify xs else x :: setify xs

  let subset s1 s2 =
    List.fold_left ( && ) true (List.map (fun s -> List.mem s s2) s1)

  let set_equiv s1 s2 = subset s1 s2 && subset s2 s1

  let iteri_right f list = iteri f (rev list)

  let rec inv_assoc k = function
    | [] ->
        raise Not_found
    | (v, k') :: _ when k = k' ->
        v
    | _ :: t ->
        inv_assoc k t

  (* Tail-recursive map. *)
  let map_tr f l = rev (rev_map f l)

  let count pred l =
    fold_left (fun count e -> count + if pred e then 1 else 0) 0 l

  let position pred l =
    let aux (i, is) e = (i + 1, if pred e then i :: is else is) in
    snd (fold_left aux (0, []) l)

  let rev_mapi f l =
    let rec aux n accu = function
      | h :: t ->
          aux (n + 1) (f n h :: accu) t
      | [] ->
          accu
    in
    aux 0 [] l

  let mapi_tr f l = rev (rev_mapi f l)

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

  let sub i j l = drop i l |> take (j - max i 0)

  let rec chop i l =
    match (i, l) with
    | j, _ when j < 0 ->
        invalid_arg "chop: index cannot be negative"
    | 0, l ->
        ([], l)
    | _, h :: t ->
        (fun (fr, ba) -> (h :: fr, ba)) (chop (i - 1) t)
    | _, [] ->
        invalid_arg "chop: index not in list"

  let rev_chop i l =
    let rec aux i fr ba =
      match (i, fr, ba) with
      | i, _, _ when i < 0 ->
          invalid_arg "rev_chop: index cannot be negative"
      | 0, fr, ba ->
          (fr, ba)
      | i, fr, h :: t ->
          aux (i - 1) (h :: fr) t
      | _ ->
          invalid_arg "rev_chop"
    in
    aux i [] l

  let chop_tr i l = (fun (fr, ba) -> (rev fr, ba)) (rev_chop i l)

  let rec dice m l =
    match chop m l with l, [] -> [l] | l1, l2 -> l1 :: dice m l2

  let remove i l =
    match rev_chop i l with
    | rfr, _ :: t ->
        rev_append rfr t
    | _ ->
        invalid_arg "remove"

  let insert i e l =
    match rev_chop i l with rfr, ba -> rev_append rfr (e :: ba)

  let replace i e l =
    match rev_chop i l with
    | rfr, _ :: t ->
        rev_append rfr (e :: t)
    | _ ->
        invalid_arg "replace"

  let morph i f l =
    match rev_chop i l with
    | rfr, h :: t ->
        rev_append rfr (f h :: t)
    | _ ->
        invalid_arg "morph"

  let rec between e = function
    | [] ->
        []
    | [h] ->
        [h]
    | h :: t ->
        h :: e :: between e t

  let between_tr e l =
    let rec aux accu e = function
      | [] ->
          rev accu
      | [h] ->
          rev (h :: accu)
      | h :: t ->
          aux (e :: h :: accu) e t
    in
    aux [] e l

  let inner fold_left2 base f l1 l2 g =
    fold_left2 (fun accu e1 e2 -> g accu (f e1 e2)) base l1 l2

  let rec is_sorted compare list =
    match list with
    | x :: y :: list ->
        if compare x y <= 0 then
          is_sorted compare (y :: list)
        else
          false
    | _ ->
        true

  let intersect xs ys = List.filter (fun x -> List.mem x ys) xs

  let set_difference a b = List.filter (fun x -> not (List.mem x b)) a

  let assoc_default k l d = Option.value ~default:d (List.assoc_opt k l)

  let map_assoc_with_key op al = List.map (fun (k, v1) -> (k, op k v1)) al

  (* Thanks to sharing we only use linear space. (Roughly double the space needed for the spine of the original list) *)
  let rec tails = function [] -> [[]] | _ :: xs as l -> l :: tails xs

  let replace_assoc key new_value existing =
    (key, new_value) :: List.filter (fun (k, _) -> k <> key) existing

  let update_assoc update existing =
    update @ List.filter (fun (k, _) -> not (List.mem_assoc k update)) existing

  let make_assoc op l = map (fun key -> (key, op key)) l

  let unbox_list l = List.filter_map Fun.id l

  let restrict_with_default default keys al =
    make_assoc (fun k -> assoc_default k al default) keys

  let range lower =
    let rec aux accu upper =
      if lower >= upper then
        accu
      else
        aux ((upper - 1) :: accu) (upper - 1)
    in
    aux []

  let find_minimum compare =
    let min a b = if compare a b <= 0 then a else b in
    function [] -> None | x :: xs -> Some (List.fold_left min x xs)
end
