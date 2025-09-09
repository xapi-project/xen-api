(*
 * Copyright (c) Cloud Software Group, Inc.
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

let is_valid = String.is_valid_utf_8

(* deprecated - reject invalid UTF-8 *)
let longest_valid_prefix str =
  let len = String.length str in
  let rec loop = function
    | i when i < len ->
        let dec = String.get_utf_8_uchar str i in
        if Uchar.utf_decode_is_valid dec then
          loop (i + Uchar.utf_decode_length dec)
        else
          String.sub str 0 i
    | i when i = len ->
        str
    | i ->
        String.sub str 0 i (* never reached *)
  in
  loop 0

module XML = struct
  (** some UTF-8 characters are not legal in XML. Assuming uchar is
      legal UTF-8, further check that it is legal in XML *)
  let is_legal uchar =
    let uchar = Uchar.to_int uchar in
    uchar >= 0x20 || uchar = 0x09 || uchar = 0x0a || uchar = 0x0d
  [@@inline]

  let is_valid str =
    let len = String.length str in
    let rec loop = function
      | i when i < len ->
          let dec = String.get_utf_8_uchar str i in
          Uchar.utf_decode_is_valid dec
          && is_legal (Uchar.utf_decode_uchar dec)
          && loop (i + Uchar.utf_decode_length dec)
      | _ ->
          true
    in
    loop 0

  (* deprecated - reject invalid UTF-8 *)
  let longest_valid_prefix str =
    let len = String.length str in
    let rec loop = function
      | i when i < len ->
          let dec = String.get_utf_8_uchar str i in
          if
            Uchar.utf_decode_is_valid dec
            && is_legal (Uchar.utf_decode_uchar dec)
          then
            loop (i + Uchar.utf_decode_length dec)
          else
            String.sub str 0 i
      | i when i = len ->
          str (* avoid copy *)
      | i ->
          String.sub str 0 i (* never reached *)
    in
    loop 0
end
