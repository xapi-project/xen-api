(*
 * Copyright (C) Citrix Systems Inc.
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

(* Basic helper functions *)

let split str n =
  let l = String.length str in
  if n>l 
  then (str,"") 
  else (String.sub str 0 n, String.sub str n (l-n))

let break pred str =
  let l = String.length str in
  let rec inner = function 
    | 0 -> (str,"")
    | n -> 
      if pred str.[l-n] 
      then split str (l-n)
      else inner (n-1)
  in inner l

let str_drop_while pred str = 
  let l = String.length str in 
  let rec inner = function 
    | 0 -> ""
    | n ->
      if pred str.[l-n]
      then inner (n-1)
      else String.sub str (l-n) n
  in inner l

let marshal_int ?(bigendian=true) n x =
  let s = Bytes.create n in
  let f =
    if bigendian
    then function x -> n-x-1
    else function x -> x
  in
  let rec inner num i =
    if i=n then s else
      let chr = Int64.logand 0xffL num in
      Bytes.set s
        (f i)
        (char_of_int (Int64.to_int chr));
      inner (Int64.shift_right_logical num 8) (i+1)
  in
  inner x 0

let marshal_int8 x = marshal_int 1 (Int64.of_int x)
let marshal_int16 x = marshal_int 2 (Int64.of_int x)
let marshal_int32 x = marshal_int 4 (Int64.of_int32 x)
let marshal_int64 x = marshal_int 8 x

let unmarshal_int ?(bigendian=true) n s =
  if String.length s < n then failwith "Invalid argument";
  let f =
    if bigendian
    then function x -> x
    else function x -> n-x-1
  in
  let rec inner acc i =
    if i=n
    then acc
    else
      let newacc = Int64.logor
          (Int64.shift_left acc 8)
          (Int64.of_int (int_of_char s.[f i]))
      in
      inner newacc (i+1)
  in inner 0L 0

let unmarshal_int8 s = Int64.to_int (unmarshal_int 1 s)
let unmarshal_int16 s = Int64.to_int (unmarshal_int 2 s)
let unmarshal_int32 s = Int64.to_int32 (unmarshal_int 4 s)
let unmarshal_int64 s = unmarshal_int 8 s

let unmask mask str =
  if String.length str > 0 then begin
    for i=0 to String.length str - 1 do
      let j = i mod 4 in
      Bytes.set str i
        (char_of_int ((int_of_char str.[i]) lxor (int_of_char mask.[j])))
    done;
    str end else str

