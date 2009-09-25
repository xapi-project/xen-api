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
(* test the "." and ".." removal code *)

let table = [ 
  "/tmp/../../../.././././../", "/";
  "/tmp/foo/bar/../../", "/tmp";
  "/tmp/foo/bar/.././..", "/tmp";
  "/tmp/foo/bar/./././../../", "/tmp";
  "/tmp/foo/bar/../../../", "/"
]

let _ = 
  List.iter (fun (input, output) ->
	       let output' = Unixext.resolve_dot_and_dotdot input in
	       if output <> output'
	       then failwith (Printf.sprintf "input = [%s] output = [%s] expected = [%s]" input output' output)
	    ) table
