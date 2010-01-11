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

type t =
	| Int of int64
	| Bool of bool
	| Float of float
	| String of string
	| Enum of t list
	| Dict of (string * t) list
	| Null

open Printf
let map_strings sep fn l = String.concat sep (List.map fn l)
let rec to_string t = match t with
	| Int i      -> sprintf "I(%Li)" i
	| Bool b     -> sprintf "B(%b)" b
	| Float f    -> sprintf "F(%g)" f
	| String s   -> sprintf "S(%s)" s
	| Enum ts    -> sprintf "[%s]" (map_strings ";" to_string ts)
	| Dict ts    -> sprintf "{%s}" (map_strings ";" (fun (s,t) -> sprintf "%s:%s" s (to_string t)) ts)
	| Null       -> "N"


let rpc_of_t x = x
let rpc_of_int64 i = Int i
let rpc_of_bool b = Bool b
let rpc_of_float f = Float f
let rpc_of_string s = String s

let t_of_rpc x = x
let int64_of_rpc = function Int i -> i | _ -> failwith "int64_of_rpc"
let bool_of_rpc = function Bool b -> b | _ -> failwith "bool_of_rpc"
let float_of_rpc = function Float f -> f | _ -> failwith "float_of_rpc"
let string_of_rpc = function String s -> s | _ -> failwith "string_of_rpc"

type callback = string list -> t -> unit

type call = {
	name: string;
	params: t list;
}

let call name params = { name = name; params = params }

type response = {
	success: bool;
	contents: t;
}

let success v = { success = true; contents = v }
let failure v = { success = false; contents = v }
