(*
 * Copyright (C) 2026 Cloud Software Group
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

type 'a node = {
    value: 'a
  ; mutable prev: 'a node option
  ; mutable next: 'a node option
}

type 'a t = {mutable first: 'a node option; mutable last: 'a node option}

let create () = {first= None; last= None}

let node x = {value= x; prev= None; next= None}

let append t n =
  match t.last with
  | None ->
      let node = Some n in
      t.first <- node ;
      t.last <- node
  | Some lst ->
      let node = Some n in
      lst.next <- node ;
      n.prev <- t.last ;
      t.last <- node

(** [drop] a node [n] from (its) list [t]. The interesting property is
    that we can drop any element from its list that we know. However,
    we don't check that [n] is indeed a member of [t] and it's an
    unchecked error to pass an [n] that is not a member of [t].

    This is similar to a
    pointer-based implementation in C. We infer that we need to update
    the fist, last entry of the list of [n]'s prev or next is [None],
    hence it is the first or last element in the list. *)
let drop t n =
  let np = n.prev in
  let nn = n.next in
  ( match np with
  | None ->
      t.first <- nn
  | Some x ->
      x.next <- nn ;
      n.prev <- None
  ) ;
  match nn with
  | None ->
      t.last <- np
  | Some x ->
      x.prev <- np ;
      n.next <- None

let first t = t.first

let last t = t.last

let value node = node.value

let foldl f zero t =
  let rec loop acc = function
    | None ->
        acc
    | Some n ->
        loop (f acc n.value) n.next
  in
  loop zero t.first

let foldr f t zero =
  let rec loop acc = function
    | None ->
        acc
    | Some n ->
        loop (f n.value acc) n.prev
  in
  loop zero t.last

let to_list t = foldr (fun x xs -> x :: xs) t []

let from_list xs =
  let t = create () in
  List.iter (fun x -> append t (node x)) xs ;
  t

(* Nodes are compared by identity: a cyclic list has no structural equality *)
let same_node a b = a == b

let step = function None -> None | Some n -> n.next

(* [cycle_of first] follows [next] from [first]. It returns [None] if the
   chain ends, and [Some (start, length)] if it runs into a cycle: [start] is
   the position of the first node on the cycle, and [length] the number of
   nodes on it (Floyd's tortoise-and-hare algorithm). *)
let cycle_of first =
  let rec meet slow fast =
    let slow = step slow and fast = step (step fast) in
    match (slow, fast) with
    | _, None ->
        None
    | Some s, Some f when same_node s f ->
        Some s
    | _ ->
        meet slow fast
  in
  match meet first first with
  | None ->
      None
  | Some m ->
      let rec length n k =
        match n.next with
        | Some x when same_node x m ->
            k
        | Some x ->
            length x (k + 1)
        | None ->
            k (* unreachable: [m] is on a cycle *)
      in
      let rec start a b k =
        match (a, b) with
        | Some x, Some y when same_node x y ->
            k
        | _ ->
            start (step a) (step b) (k + 1)
      in
      Some (start first (Some m) 0, length m 1)

let compare cmp t1 t2 =
  (* Two cyclic lists repeat with period [lcm l1 l2] after position
     [max s1 s2], so if they agree on that many elements they agree on all
     of them. If either list ends, the walk ends with it. *)
  let rec gcd a b =
    if b = 0 then
      a
    else
      gcd b (a mod b)
  in
  let limit =
    match (cycle_of t1.first, cycle_of t2.first) with
    | Some (s1, l1), Some (s2, l2) ->
        Some (max s1 s2 + (l1 / gcd l1 l2 * l2))
    | _ ->
        None
  in
  let rec loop a b k =
    if Some k = limit then
      0
    else
      match (a, b) with
      | None, None ->
          0
      | None, Some _ ->
          -1
      | Some _, None ->
          1
      | Some na, Some nb ->
          let c = cmp na.value nb.value in
          if c <> 0 then
            c
          else
            loop na.next nb.next (k + 1)
  in
  loop t1.first t2.first 0

let equal t1 t2 = compare Stdlib.compare t1 t2 = 0
