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

(* General DB utils *)

	  
exception Db_set_or_map_parse_fail of string
  
let parse_sexpr s : SExpr.t list =
  match SExpr_TS.of_string s with
    | SExpr.Node xs -> xs
    | _ -> raise (Db_set_or_map_parse_fail s)
	
let add_key_to_set key set =
  if List.mem (SExpr.String key) set 
  then set
  else SExpr.String key :: set
