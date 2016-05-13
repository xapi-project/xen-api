(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

open OUnit

(** sets env variable for log files *)
let setup_coverage_profiling name =
  let (//) = Filename.concat in
  let tmpdir =
    let getenv n   = try Sys.getenv n with Not_found -> "" in
    let dirs    = 
      [ getenv "TMP"
      ; getenv "TEMP"
      ; "/tmp"
      ; "/usr/tmp"
      ; "/var/tmp"
      ] in
    let is_dir  = function 
    | ""    -> false
    | path  -> try Sys.is_directory path with Sys_error _ -> false
    in try
      List.find is_dir dirs
    with
      Not_found -> failwith ("can't find temp directory "^__LOC__); exit 1
  in try 
    ignore (Sys.getenv "BISECT_FILE") 
  with Not_found ->
    Unix.putenv "BISECT_FILE" (tmpdir // Printf.sprintf "bisect-%s" name)
 


let base_suite =
	"base_suite" >:::
		[
			Network_test_lacp_properties.suite;
		]

let _ = 
  setup_coverage_profiling Sys.argv.(0);
  run_test_tt_main base_suite
