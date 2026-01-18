(*
 * Copyright (c) Cloud Software Group, Inc
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

type 'a t = {q: 'a Queue.t; capacity: int; mutable dropped: int}

let make capacity = {q= Queue.create (); capacity; dropped= 0}

let capacity t = t.capacity

let dropped t = t.dropped

let add (type a) t (e : a) =
  while Queue.length t.q >= t.capacity do
    let (_ : a) = Queue.pop t.q in
    t.dropped <- t.dropped + 1
  done ;
  Queue.add e t.q

let to_seq t = Queue.to_seq t.q

let clear t = Queue.clear t.q
