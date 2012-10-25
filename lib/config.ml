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

(** Allow settings to be stored in a config file as key = value pairs,
	as well as parsed directly from the commandline. *)

open Arg

let finally f g =
	try
		let result = f () in
		g ();
		result
	with e ->
		g ();
		raise e

let apply v = function
	| Unit f -> f ()
	| Set_string s -> s := v
	| String f -> f v
	| Bool f -> f (bool_of_string v)
	| Set_int i -> i := (int_of_string v)
	| Int f -> f (int_of_string v)
	| Set_float f -> f := (float_of_string v)
	| Float f -> f (float_of_string v)
	| _ -> failwith "Unsupported type in config file"

let read filename spec =
	(* Remove the unnecessary doc parameter *)
	let spec = List.map (fun (a, b, c) -> a, b) spec in
	let ic = open_in filename in
	finally
		(fun () ->
			try
				while true do
					let line = input_line ic in
					(* Strip comments *)
					match Re_str.(split_delim (regexp (quote "#"))) line with
					| [] -> ()
					| x :: _ ->
						begin match Re_str.bounded_split_delim (Re_str.regexp "\\s*=\\s*") x 2 with
						| key :: v :: [] ->
							if List.mem_assoc key spec then apply v (List.assoc key spec)
						| _ -> ()
						end
				done
			with End_of_file -> ()
		) (fun () -> close_in ic)
