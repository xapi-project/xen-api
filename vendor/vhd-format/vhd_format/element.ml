(*
 * Copyright (C) 2011-2013 Citrix Inc
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

type 'a t = [
  | `Copy of ('a * int64 * int64)
  | `Sectors of Cstruct.t
  | `Empty of int64
]

let sector_size = 512

let to_string = function
  | `Copy(_, offset, 1L) ->
    Printf.sprintf "1 sector copied starting at offset %Ld" offset
  | `Copy(_, offset, len) ->
    Printf.sprintf "%Ld sectors copied starting at offset %Ld" len offset
  | `Sectors x ->
    let text = String.escaped (Cstruct.to_string (Cstruct.sub x 0 16)) in
    if Cstruct.len x = sector_size
    then Printf.sprintf "1 sector \"%s...\"" text
    else Printf.sprintf "%d sectors \"%s...\"" (Cstruct.len x / sector_size) text
  | `Empty 1L ->
    "1 empty sector"
  | `Empty x ->
    Printf.sprintf "%Ld empty sectors" x

let len = function
  | `Copy(_, _, len) -> len
  | `Sectors x -> Int64.of_int (Cstruct.len x / sector_size)
  | `Empty x -> x

