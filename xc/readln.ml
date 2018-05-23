(* Read newline-delimited strings from a file descriptor *)

type result      = Ok of string list | Error of string | EOF

let input        = Hashtbl.create 100 (* holds unconsumed input per fd *)
let lookup tbl k = try Hashtbl.find tbl k with Not_found -> []
let free fd      = Hashtbl.remove input fd

let read fd =
  let buffer_size  = 4096 in
  let buffer       = Bytes.make buffer_size '\000' in
  match Unix.read fd buffer 0 buffer_size with
  | 0 ->
    let pending = try Hashtbl.find input fd with Not_found -> Bytes.empty in
    Hashtbl.remove input fd;
    if pending = Bytes.empty then
      EOF
    else
      Error (Printf.sprintf "Unconsumed data at EOF: '%s'" (Bytes.to_string pending))
  | n ->
    let data = Bytes.sub buffer 0 n in
    let inpt = try Hashtbl.find input fd with Not_found -> Bytes.empty in
    Hashtbl.replace input fd (Bytes.cat inpt data);
    let rec loop msgs =
      let data  = Hashtbl.find input fd in (* never fails *)
      match Bytes.index data '\n' with
      | exception Not_found -> Ok(List.rev msgs)
      | index ->
        let remain = Bytes.sub data (index + 1) (Bytes.length data -index-1) in
        Hashtbl.replace input fd remain; (* reset input *)
        loop (Bytes.sub_string data 0 index :: msgs) (* store msg *)
    in
    loop []
  | exception Unix.Unix_error (error, _, _) ->
    Error(Unix.error_message error)

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
