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
let encode = B64.encode ?pad:None ?alphabet:None
let decode s =
  let strip_whitespace s =
    let fold_right f string accu =
      let accu = ref accu in
      for i = String.length string - 1 downto 0 do
        accu := f string.[i] !accu
      done;
      !accu
    in
    let explode string =
      fold_right (fun h t -> h :: t) string []
    in
    let implode list =
      let of_char c = String.make 1 c in
      String.concat "" (List.map of_char list)
    in
    implode (List.filter (fun x->not (List.mem x [' ';'\t';'\n';'\r'])) (explode s))
  in
  B64.decode ?alphabet:None (strip_whitespace s)