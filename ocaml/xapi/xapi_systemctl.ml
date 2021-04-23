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
(**
 * @system service management
*)

module D = Debug.Make (struct let name = "xapi_systemctl" end)

open D

type t = Start | Stop

exception Systemctl_fail of string

let to_string = function Start -> "start" | Stop -> "stop"

let perform ~wait_until_success ~service ~timeout op =
  let op_str = op |> to_string in
  try
    debug "%s %s" op_str service ;
    ignore
      (Forkhelpers.execute_command_get_output !Xapi_globs.systemctl
         [op_str; service]) ;
    if wait_until_success then (
      let is_active = Fe_systemctl.is_active service in
      let success_cond () =
        match op with Start -> is_active | Stop -> is_active |> not
      in
      try
        Helpers.retry_until_timeout ~timeout
          (Printf.sprintf "trying to %s %s" op_str service)
          success_cond
      with e ->
        debug "Fail to %s %s timeout %f" op_str service timeout ;
        raise e
    )
  with e ->
    let err_str = ExnHelper.string_of_exn e in
    error "Fail to %s %s with error %s" op_str service err_str ;
    raise (Systemctl_fail err_str)

let stop ?(timeout = 5.) ~wait_until_success service =
  perform ~wait_until_success ~service ~timeout Stop

let start ?(timeout = 5.) ~wait_until_success service =
  perform ~wait_until_success ~service ~timeout Start
