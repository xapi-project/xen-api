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
module L = Debug.Make(struct let name="license" end)

let never, _ =
  let start_of_epoch = Unix.gmtime 0. in
  Unix.mktime {start_of_epoch with Unix.tm_year = 130}

let get_expiry_date ~__context ~host =
  let license = Db.Host.get_license_params ~__context ~self:host in
  if List.mem_assoc "expiry" license
  then Some (Stdext.Date.of_string (List.assoc "expiry" license))
  else None

let check_expiry ~__context ~host =
  let expired =
    match get_expiry_date ~__context ~host with
    | None -> false (* No expiry date means no expiry :) *)
    | Some date -> Unix.time () > (Stdext.Date.to_float date)
  in
  if expired then raise (Api_errors.Server_error (Api_errors.license_expired, []))

let vm ~__context vm =
  (* Here we check that the license is still valid - this should be the only place where this happens *)
  let host = Helpers.get_localhost ~__context in
  check_expiry ~__context ~host

(* XXX: why use a "with_" style function here? *)
let with_vm_license_check ~__context v f =
  vm ~__context v;
  f()
