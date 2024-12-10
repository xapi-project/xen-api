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
(* Imperative priority queue *)

type 'a event = {ev: 'a; time: Mtime.span}

type 'a t = {default: 'a event; mutable size: int; mutable data: 'a event array}

exception EmptyHeap

let create n default =
  if n <= 0 then
    invalid_arg "create"
  else
    let default = {ev= default; time= Mtime_clock.elapsed ()} in
    {default; size= 0; data= Array.make n default}

let is_empty h = h.size <= 0

let resize h =
  let n = h.size in
  assert (n > 0) ;
  let n' = 2 * n in
  let d = h.data in
  let d' = Array.make n' h.default in
  Array.blit d 0 d' 0 n ;
  h.data <- d'

let add h x =
  let n = h.size in
  (* resizing if needed *)
  if n = Array.length h.data then resize h ;
  let d = h.data in
  (* moving [x] up in the heap *)
  let rec moveup i =
    let fi = (i - 1) / 2 in
    if i > 0 && Mtime.Span.is_longer d.(fi).time ~than:x.time then (
      d.(i) <- d.(fi) ;
      moveup fi
    ) else
      d.(i) <- x
  in
  moveup n ;
  h.size <- n + 1

let maximum h =
  if h.size <= 0 then raise EmptyHeap ;
  h.data.(0)

let remove h s =
  if h.size <= 0 then raise EmptyHeap ;
  if s < 0 || s >= h.size then
    invalid_arg (Printf.sprintf "%s: index %d out of bounds" __FUNCTION__ s) ;
  let n = h.size - 1 in
  let d = h.data in
  let x = d.(n) in
  d.(n) <- h.default ;
  (* moving [x] up in the heap *)
  let rec moveup i =
    let fi = (i - 1) / 2 in
    if i > 0 && Mtime.Span.is_longer d.(fi).time ~than:x.time then (
      d.(i) <- d.(fi) ;
      moveup fi
    ) else
      d.(i) <- x
  in
  (* moving [x] down in the heap *)
  let rec movedown i =
    let j = (2 * i) + 1 in
    if j < n then
      let j =
        let j' = j + 1 in
        if j' < n && d.(j').time < d.(j).time then j' else j
      in
      if Mtime.Span.is_shorter d.(j).time ~than:x.time then (
        d.(i) <- d.(j) ;
        movedown j
      ) else
        d.(i) <- x
    else
      d.(i) <- x
  in
  if s = n then
    ()
  else if Mtime.Span.is_longer d.(s).time ~than:x.time then
    moveup s
  else
    movedown s ;
  h.size <- n

let find h ev =
  let rec iter n =
    if n < 0 then
      -1
    else if ev = h.data.(n).ev then
      n
    else
      iter (n - 1)
  in
  iter (h.size - 1)

let find_p h f =
  let rec iter n =
    if n < 0 then
      -1
    else if f h.data.(n).ev then
      n
    else
      iter (n - 1)
  in
  iter (h.size - 1)

let pop_maximum h =
  let m = maximum h in
  remove h 0 ; m

let check h =
  let d = h.data in
  for i = 1 to h.size - 1 do
    let fi = (i - 1) / 2 in
    let ordered = Mtime.Span.is_longer d.(i).time ~than:d.(fi).time in
    assert ordered
  done

let iter f h =
  let d = h.data in
  for i = 0 to h.size - 1 do
    f d.(i)
  done

(*
let fold f h x0 =
  let n = h.size in
  let d = h.data in
  let rec foldrec x i = if i >= n then x else foldrec (f d.(i) x) (succ i) in
  foldrec x0 0
*)
