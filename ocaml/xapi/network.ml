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

open Xmlrpc_client
open Network_interface

module D=Debug.Make(struct let name="network" end)
open D

module Net = Network_client.Client

(* Catch any uncaught networkd exceptions and transform into the most relevant XenAPI error.
   We do not want a XenAPI client to see a raw network error. *)
let transform_networkd_exn pif f =
  let reraise code params =
    error "Re-raising as %s [ %s ]" code (String.concat "; " params);
    raise (Api_errors.Server_error(code, params)) in
  try
    f ()
  with
  | Network_error (Script_missing script) ->
    let e = Printf.sprintf "script %s missing" script in
    reraise Api_errors.pif_configuration_error [Ref.string_of pif; e]
  | Network_error (Script_error params) ->
    let e = Printf.sprintf "script error [%s]" (String.concat ", "
                                                  (List.map (fun (k, v) -> k ^ " = " ^ v) params)) in
    reraise Api_errors.pif_configuration_error [Ref.string_of pif; e]
  | Network_error (Read_error file) | Network_error (Write_error file) ->
    let e = "failed to access file " ^ file in
    reraise Api_errors.pif_configuration_error [Ref.string_of pif; e]
  | Network_error Not_implemented ->
    let e = "networkd function not implemented" in
    reraise Api_errors.pif_configuration_error [Ref.string_of pif; e]
  | Network_error (Vlan_in_use (device, vlan)) ->
    reraise Api_errors.vlan_in_use [device; string_of_int vlan]
  | e ->
    error "Caught %s while trying to plug a PIF" (ExnHelper.string_of_exn e);
    reraise Api_errors.pif_configuration_error [Ref.string_of pif; ""]

