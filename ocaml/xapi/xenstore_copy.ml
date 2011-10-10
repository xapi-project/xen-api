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
(* Simple example program which recursively copies a xenstore subtree to another path. *)

open Xenstore_dump
open Xenstore

let _ = 

  let src = ref "" and dest = ref "" in
  Arg.parse 
    [ "-src", Arg.Set_string src, "source path";
      "-dest", Arg.Set_string dest, "destination path" ]
    (fun x -> Printf.fprintf stderr "Ignoring unknown parameter: %s\n" x)
    "Copy a xenstore subtree to another path";
  if !src = "" || !dest = "" then begin
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr "  %s <source path> <destination path>\n" Sys.argv.(0);
    exit 1
  end;
  let xs = Xs.domain_open () in
  restore ~xs !dest (dump ~xs !src)

