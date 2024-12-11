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
module L = Debug.Make (struct let name = "license" end)

module Date = Xapi_stdext_date.Date

let never = Ptime.of_year 2100 |> Option.get |> Date.of_ptime

let serialize_expiry = function
  | None ->
      "never"
  | Some date when Date.equal date never ->
      "never"
  | Some date ->
      Date.to_rfc3339 date

let get_expiry_date ~__context ~host =
  let license = Db.Host.get_license_params ~__context ~self:host in
  List.assoc_opt "expiry" license
  |> Fun.flip Option.bind (fun e -> if e = "never" then None else Some e)
  |> Option.map Xapi_stdext_date.Date.of_iso8601

let check_expiry ~__context ~host =
  let expired =
    match get_expiry_date ~__context ~host with
    | None ->
        false (* No expiry date means no expiry :) *)
    | Some expiry ->
        Xapi_stdext_date.Date.(is_later ~than:expiry (now ()))
  in
  if expired then
    raise Api_errors.(Server_error (license_expired, []))

let vm ~__context _vm =
  (* Here we check that the license is still valid - this should be the only place where this happens *)
  let host = Helpers.get_localhost ~__context in
  check_expiry ~__context ~host

(* XXX: why use a "with_" style function here? *)
let with_vm_license_check ~__context v f = vm ~__context v ; f ()
