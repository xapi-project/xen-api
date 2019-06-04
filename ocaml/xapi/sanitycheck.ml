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

module D=Debug.Make(struct let name="sanitycheck" end)
open D


let check_for_bad_link () =
  (* Look for the exception catching bug caused by dodgy linking (thanks, ocamlfind) *)
  try
    Unix.access "/etc/xapi.d/doesntexist" [ Unix.F_OK ]
  with
  | Unix.Unix_error(_, _, _) -> debug "Binary appears to be correctly linked"
  | e ->
    let msg = "This binary is broken: check your link lines (see c/s 4200:694e7dabb159)" in
    debug "%s" msg;
    failwith msg


