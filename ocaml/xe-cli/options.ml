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
(* Default options in a file ~/.xe-cli *)

let parse_lines ls =
  let rec inner ls cur =
    try
      match ls with
        ""::ls -> inner ls cur  (* skip blank lines *)
      |	l::ls ->
        let colon = String.index l '=' in
        let token = String.sub l 0 colon in
        let value = String.sub l (colon+1) (String.length l - colon - 1) in
        inner ls ((token,value)::cur)
      | _ -> cur
    with Not_found ->
      Printf.fprintf stderr "Error parsing rc file. No defaults loaded\n";
      []
  in
  inner ls []

let read_rc () =
  try
    let home = Sys.getenv "HOME" in
    let rc_file = open_in (home^"/.xe") in
    let rec getlines cur =
      try
        let line = input_line rc_file in
        getlines (line::cur)
      with
        _ -> cur
    in
    let lines = getlines [] in
    parse_lines lines
  with
    _ -> []
