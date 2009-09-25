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

(** Test the Unixext.safe_unlink function *)

open Unixext

let _ = 

  let src = ref None in

  Arg.parse 
    [ ]
    (fun x -> match !src with
     | None -> src := Some x
     | _ -> Printf.printf "Ignoring unknown argument: %s" x)
    "Unlink a file which may not exist, suppressing the ENOENT error";

  match !src with
  | Some a -> Unixext.unlink_safe a
  | None -> failwith "Missing filename"


	      
