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
(* Hopefully, this handler will be able to look at the return codes of the cmds, and produce *)
(* a reasonable error. For now, just return the error code! *)
let generic_handler cmd n =
  raise (Api_errors.Server_error (Api_errors.internal_error, [string_of_int n]))

exception Process_output_error of string
let get_process_output ?(handler=generic_handler) cmd =
  let args = Stdext.Xstringext.String.split ' ' cmd in
  try
    fst (Forkhelpers.execute_command_get_output (List.hd args) (List.tl args))
  with
  | Forkhelpers.Spawn_internal_error(err,out,Unix.WEXITED n) ->
    handler cmd n
  | _ ->
    raise (Process_output_error cmd)

