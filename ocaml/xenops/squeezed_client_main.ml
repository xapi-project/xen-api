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
open Squeezed_rpc
open Xenops_helpers

module D = Debug.Debugger(struct let name = Memory_interface.service_name end)
open D

let _ = 

  if Array.length Sys.argv < 2 then begin
    Printf.fprintf stderr "%s <fn name> [key=val]\n" Sys.argv.(0);
    Printf.fprintf stderr "  -- call function <fn name> with optional key=value arguments\n";
    exit 1
  end;
  let fn = Sys.argv.(1) in
  let args = List.tl (List.tl (Array.to_list Sys.argv)) in
  let args = List.map (fun x -> match Stringext.String.split ~limit:2 '=' x with
		       | [ key; v ] -> [ key, v ]
		       | _ -> debug "Skipping argument: %s" x; []) args in
  let args = List.concat args in
  with_xc_and_xs
    (fun _ xs ->
       let results = Rpc.client ~xs ~service:_service ~fn ~args in
       List.iter (fun (k, v) -> Printf.printf "%s=%s\n" k v) results
    )
