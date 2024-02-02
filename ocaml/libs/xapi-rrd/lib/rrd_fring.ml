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

module BoundedFloat = Rrd_utils.BoundedFloat

type t = {
    size: int
  ; mutable current: int
  ; min: float
  ; max: float
  ; data: (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
}

let make size (init : float) minimum maximum =
  let ring =
    {
      size
    ; current= size - 1
    ; min= minimum
    ; max= maximum
    ; data= Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout size
    }
  in
  let bound =
    BoundedFloat.of_float ~minimum ~maximum ~f:BoundedFloat.To_Nan init
  in
  Bigarray.Array1.fill ring.data @@ BoundedFloat.to_float bound ;
  ring

let copy x =
  let y = make x.size nan x.min x.max in
  Bigarray.Array1.blit x.data y.data ;
  y.current <- x.current ;
  y

let length ring = ring.size

let push ring (e : float) =
  ring.current <- ring.current + 1 ;
  if ring.current = ring.size then
    ring.current <- 0 ;
  let bound =
    BoundedFloat.of_float ~minimum:ring.min ~maximum:ring.max
      ~f:BoundedFloat.To_Nan e
  in
  Bigarray.Array1.set ring.data ring.current @@ BoundedFloat.to_float bound

let peek ring i =
  if i >= ring.size then
    raise (Invalid_argument "peek: index") ;
  let index =
    let offset = ring.current - i in
    if offset >= 0 then offset else ring.size + offset
  in
  ring.data.{index}

let top ring = ring.data.{ring.current}

let iter_nb ring f nb =
  if nb > ring.size then
    raise (Invalid_argument "iter_nb: nb") ;
  (* FIXME: OPTIMIZE ME with 2 Array.iter ? *)
  for i = 0 to nb - 1 do
    f (peek ring i)
  done

(* iter directly on all element without using the index *)
let iter f a =
  for i = 0 to Bigarray.Array1.dim a - 1 do
    f a.{i}
  done

let raw_iter ring f = iter f ring.data

let iter ring f = iter_nb ring f ring.size

let get_nb ring nb =
  if nb > ring.size then
    raise (Invalid_argument "get_nb: nb") ;
  let a = Array.make nb (top ring) in
  for i = 1 to nb - 1 do
    (* FIXME: OPTIMIZE ME with 2 Array.blit *)
    a.(i) <- peek ring i
  done ;
  a

let get ring = get_nb ring ring.size
