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
open Base64

let usage () = 
  output_string stderr (Printf.sprintf "Usage: %s (encode|decode) string\n" Sys.argv.(0));
  exit 1

let _ = 
  if Array.length Sys.argv <> 3 then usage ();
  match Sys.argv.(1) with
  | "encode" ->
      print_string (encode Sys.argv.(2))
  | "decode" ->
      print_string (decode Sys.argv.(2))
  | _ -> 
      usage ()
