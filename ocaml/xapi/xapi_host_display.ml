(*
 * Copyright (C) Citrix Systems Inc.
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

let script = "/opt/xensource/libexec/host-display"

let call_script ~command =
  let (stdout, _) = Forkhelpers.execute_command_get_output script [command]
  in String.trim stdout

let disable () =
  let (_: string) = call_script "disable" in ()

let enable () =
  let (_: string) = call_script "enable" in ()

let status () =
  match call_script "status" with
  | "disabled" -> `disabled
  | "enabled" -> `enabled
  | _ -> `unknown
