(* Read newline-delimited strings from a file descriptor *)

type result = Ok of string list | Error of string | EOF

let input = Hashtbl.create 100 (* holds unconsumed input per fd *)

let free fd = Hashtbl.remove input fd

let read fd =
  let buffer_size = 4096 in
  let buffer = Bytes.make buffer_size '\000' in
  match Unix.read fd buffer 0 buffer_size with
  | 0 ->
      let pending =
        Option.value (Hashtbl.find_opt input fd) ~default:Bytes.empty
      in
      Hashtbl.remove input fd ;
      if pending = Bytes.empty then
        EOF
      else
        Error
          (Printf.sprintf "Unconsumed data at EOF: '%s'"
             (Bytes.to_string pending)
          )
  | n ->
      let rec loop msgs data =
        match Bytes.index_opt data '\n' with
        | None ->
            (List.rev msgs, data)
        | Some index ->
            let remain =
              Bytes.sub data (index + 1) (Bytes.length data - index - 1)
            in
            loop
              (Bytes.sub_string data 0 index :: msgs)
              remain (* reset input *)
      in
      let data = Bytes.sub buffer 0 n in
      let inpt =
        Option.value (Hashtbl.find_opt input fd) ~default:Bytes.empty
      in
      let inp_data = Bytes.cat inpt data in
      let res, data = loop [] inp_data in
      Hashtbl.replace input fd data ;
      Ok res
  | exception Unix.Unix_error (error, _, _) ->
      Error (Unix.error_message error)

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
