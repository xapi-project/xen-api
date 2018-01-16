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

type 'a event = { ev: 'a;
                  time: Mtime.t }

type 'a t = {mutable size : int; mutable data : 'a event array }

exception EmptyHeap

let create n =
  if n<=0 then invalid_arg "create" else
    { size = -n; data=[| |] }

let is_empty h =
  h.size <= 0

let resize h =
  let n = h.size in
  assert (n>0);
  let n'=2*n in
  let d = h.data in
  let d' = Array.make n' d.(0) in
  Array.blit d 0 d' 0 n;
  h.data <- d'

let add h x =
  (* first addition: we allocate the array *)
  if h.size < 0 then begin
    h.data <- Array.make (- h.size) x; h.size <- 0
  end;
  let n = h.size in
  (* resizing if needed *)
  if n == Array.length h.data then resize h;
  let d = h.data in
  (* moving [x] up in the heap *)
  let rec moveup i =
    let (>>) = Mtime.is_later in
    let fi = (i - 1) / 2 in
    if i > 0 && (d.(fi).time >> x.time) then begin
      d.(i) <- d.(fi);
      moveup fi
    end else
      d.(i) <- x
  in
  moveup n;
  h.size <- n + 1

let maximum h =
  if h.size <= 0 then raise EmptyHeap;
  h.data.(0)

let remove h s =
  if h.size <= 0 then raise EmptyHeap;
  let n = h.size - 1 in
  h.size <- n;
  let d = h.data in
  let x = d.(n) in
  (* moving [x] down in the heap *)
  let rec movedown i =
    let j = 2 * i + 1 in
    let (<<) = Mtime.is_earlier in
    if j < n then
      let j =
        let j' = j + 1 in
        if j' < n && (d.(j').time < d.(j).time) then j' else j
      in
      if (d.(j).time << x.time) then begin
        d.(i) <- d.(j);
        movedown j
      end else
        d.(i) <- x
    else
      d.(i) <- x
  in
  movedown s

let find h ev =
  let rec iter n =
    if n < 0 then -1
    else if ev = h.data.(n).ev then n else iter (n-1)
  in
  iter (h.size-1)

let find_p h f =
  let rec iter n =
    if n < 0 then -1
    else if f h.data.(n).ev then n else iter (n-1)
  in
  iter (h.size-1)

let pop_maximum h = let m = maximum h in remove h 0; m

let iter f h =
  let d = h.data in
  for i = 0 to h.size - 1 do f d.(i) done

let fold f h x0 =
  let n = h.size in
  let d = h.data in
  let rec foldrec x i =
    if i >= n then x else foldrec (f d.(i) x) (succ i)
  in
  foldrec x0 0

(*
let _ =
  let test : int t = create 100 in
  for i=0 to 99 do
    let e = {time=Random.float 10.0; ev=i} in
    add test e
  done;
  for i=0 to 49 do
    let xx=find test i in
    remove test xx
  done;
(*  remove test xx;*)
  for i=0 to 49 do
    let e=pop_maximum test in
    Printf.printf "time: %f, site: %d\n" e.time e.ev
  done
*)
