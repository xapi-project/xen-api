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

module Sig = struct
	type t =
	[ `Int | `Bool | `Float | `String
	| `Product of t list
	| `Named_product of (string * t) list
	| `Named_sum of (string * t) list
	| `Option of t ]
end

module Val = struct
	type t = 
	[ `Int of int64
	| `Bool of bool
	| `Float of float
	| `String of string
	| `List of t list
	| `Dict of (string * t) list
	| `None ]

	let rec to_string (x:t) = match x with
	| `Int i    -> Printf.sprintf "Int(%Lu)" i
	| `Bool b   -> Printf.sprintf "Bool(%b)" b
	| `Float f  -> Printf.sprintf "Float(%f)" f
	| `String s -> Printf.sprintf "String(%s)" s
	| `List l   -> "List [ " ^ String.concat ", " (List.map to_string l) ^ " ]"
	| `Dict d   -> "Dict {" ^ String.concat ", " (List.map (fun (s,t) -> Printf.sprintf "%s: %s" s (to_string t)) d) ^ " }"
	| `None     -> "None"
end

(* The first argument is the list of record field names we already went trough *)
type callback = string list -> Val.t -> unit

type call = {
	name: string;
	params: Val.t list
}

type response =
	| Success of Val.t
	| Fault of int * string
