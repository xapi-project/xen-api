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

(* Simple generation count implementation *)
module D = Debug.Debugger(struct let name = "sql" end)
open D

type t = int64

let of_string str : t = Int64.of_string str
let to_string g = Int64.to_string g
let add_int a b = Int64.add a (Int64.of_int b)
let null_generation = -1L

let filename dbconn = dbconn.Parse_db_conf.path^".generation"

let read dbconn =
  let gencount_fname = filename dbconn in
  try Int64.of_string (Unixext.string_of_file gencount_fname) with _ -> 0L
