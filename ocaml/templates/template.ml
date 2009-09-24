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
type t = { name: string;
	   parts: part list }

and part = 
  | Hole of string
  | String of string

let string_of substitutions t = 
  let assoc x substitutions = 
    try List.assoc x substitutions
    with Not_found ->
      failwith (Printf.sprintf "Couldn't find substitution [%s] in template %s"
		  x t.name) in

  let stuff = List.map (function 
			| Hole x -> assoc x substitutions
			| String x -> x) t.parts in
  String.concat "" stuff

let output_fmt ff t = 
  let part = function
    | Hole x -> Format.fprintf ff "<[=%s]>" x
    | String x -> Format.fprintf ff "%s" x in
  Format.fprintf ff "@[ <[ %s ]>" t.name;
  List.iter part t.parts;
  Format.fprintf ff "<[ end ]> @]"
