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

open Debug
open Bootloader

let _ =
  let disk = ref "" in
  Arg.parse [
    "-debug", Arg.Set debug_flag, "enable debug output"
  ]
    (fun x ->
       if !disk = "" then disk := x else
         warn ("Ignoring unexpected extra argument: " ^ x))
    "Test code for pygrub wrapper";

  let disk = !disk in
  if disk = "" then failwith "You must supply a disk name as an argument";

  extract_default_kernel disk
