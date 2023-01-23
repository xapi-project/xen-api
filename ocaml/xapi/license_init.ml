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

module D = Debug.Make (struct let name = "license" end)

open D

(* Dependency injection for unit tests *)
module type V6clientS = module type of V6_client

let v6client = ref (module V6_client : V6clientS)

let initialise_host_editions ~__context ~host =
  let dbg = Context.string_of_task __context in
  let editions =
    List.map V6_interface.(fun ed -> ed.title) (V6_client.get_editions dbg)
  in
  Db.Host.set_editions ~__context ~self:host ~value:editions

(* xapi calls this function upon startup *)
let initialise ~__context ~host =
  let module V6_client = (val !v6client : V6clientS) in
  let set_licensing edition features additional =
    debug "Setting license to %s" edition ;
    Db.Host.set_edition ~__context ~self:host ~value:edition ;
    (* Copy resulting license to the database *)
    Xapi_host.copy_license_to_db ~__context ~host ~features ~additional
  in
  try
    initialise_host_editions ~__context ~host ;
    let edition = Db.Host.get_edition ~__context ~self:host in
    Xapi_host.apply_edition_internal ~__context ~host ~edition
      ~additional:[("force", "true")]
  with
  | Api_errors.Server_error (code, []) when code = Api_errors.v6d_failure ->
      (* Couldn't communicate with v6d, so fall back to running in free/libre
         		 * "xcp" mode, with all standard features enabled and no additional
         		 * features advertised. This is the same as the "free" edition from v6d
         		 * for most purposes but not for pool-join: see assert_restrictions_match
         		 * in pre_join_checks in ocaml/xapi/xapi_pool.ml *)
      set_licensing "free/libre" Features.all_features []
  | _ ->
      ()
