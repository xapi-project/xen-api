(* (C) 2006-2010 Citrix Systems Inc.
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

open Features

(* Editions definitions *)

type edition = Free
exception Undefined_edition of string

let of_string = function
	| "free" | "XE Express" -> Free
	| x -> raise (Undefined_edition x)

let to_string = function
	| Free -> "free"

let to_short_string = function
	| Free -> "FREE"
	
let to_marketing_name = function
	| Free -> "Xen Cloud Platform"

(* Editions to features *)

let to_features = function
	| Free -> all_features

let to_int = function
	| _ -> 0

let equal e0 e1 =
	to_int e0 = to_int e1
	
let min l =
	Free

