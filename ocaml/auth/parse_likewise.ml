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
(**
 * @group Access Control
 *)
 
type result = 
  | Success of (string * string) list
  | Failure of int * string

let string_list_of_result = function
  | Success pairs ->
      "result was success" :: (List.map (fun (k, v) -> Printf.sprintf "%s=[%s]" k v) pairs)
  | Failure (code, msg) ->
      [ Printf.sprintf "failure code=%d msg=[%s]" code msg ]

exception Parse_failure of (string * string)

let of_channel ic = 
  let success_of_channel ic = 
    let pairs = ref [] in
    try
      while true do
	let x = input_line ic in
	let colon = 
	  try
	    String.index x ':'
	  with Not_found -> raise (Parse_failure (x, ":")) in
	let key = String.sub x 0 colon in
	let value = String.sub x (colon + 1) (String.length x - colon - 1) in
	(* Now must de-hexify the value string *)
	let int_of_nibble y = match y with
	  | '0' .. '9' -> int_of_char y - (int_of_char '0')
	  | 'a' .. 'f' -> int_of_char y - (int_of_char 'a') + 10
	  | _ -> raise (Parse_failure(x, "hexified string"))
	in
	let num_bytes = String.length value / 2 in
	let result = String.make num_bytes ' ' in
	for i = 0 to num_bytes - 1 do
	  let high = int_of_nibble (value.[i * 2 + 0]) in
	  let low  = int_of_nibble (value.[i * 2 + 1]) in
	  let byte = high * 16 + low in
	  result.[i] <- char_of_int byte
	done;
	pairs := (key, result) :: !pairs
      done;
      Success []; (* never reached *)
    with 
    | End_of_file ->
      Success (List.rev !pairs) in
  let failure_of_channel ic = 
    let code = 
      let x = input_line ic in
      try int_of_string x with _ -> raise (Parse_failure(x, "failure code: integer")) in
    let msgbuf = Buffer.create 100 in (* only one line expected most of the time *)
    (try for nline = 1 to 20 do Buffer.add_string msgbuf (input_line ic) done (* up to 20 error lines *)
     with End_of_file -> ()); (* expected, nothing to do *)
    let msg = Buffer.contents msgbuf in
    Failure(code, msg) in

  match (input_line ic) with
  | "SUCCESS" -> 
      success_of_channel ic
  | "FAILURE" ->
      failure_of_channel ic
  | x -> raise (Parse_failure (x, "SUCCESS | FAILURE"))
