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
external is_all_zeros : string -> int -> bool = "is_all_zeros"

external _find_a_nonzero : string -> int -> int -> int = "find_a_nonzero"
external _find_a_zero : string -> int -> int -> int = "find_a_zero"

let wrap f x len offset =
  let remaining = len - offset in
  if remaining <= 0 then raise (Invalid_argument "offset > length");
  let result = f x offset remaining in
  if result = remaining then None else Some (result + offset)

let find_a_nonzero = wrap _find_a_nonzero
let find_a_zero = wrap _find_a_zero

type substring = {
  buf: string;
  offset: int;
  len: int
}

let fold_over_nonzeros x len rounddown roundup f initial = 
  let rec inner acc offset = 
    if offset = len then acc
    else
      match find_a_nonzero x len offset with
      | None -> acc (* no more *)
      | Some s -> 
        let e = match find_a_zero x len s with
          | None -> len
          | Some e -> e in
        let e = min len (roundup e) in
        let s = max 0 (rounddown s) in
        inner (f acc { buf = x; offset = s; len = e - s }) e in
  inner initial 0

