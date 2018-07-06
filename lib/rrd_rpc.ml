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

(* A helper function for extracting the dictionary out of the RPC type. *)
let dict_of_rpc ~(rpc : Rpc.t) : (string * Rpc.t) list =
  match rpc with Rpc.Dict d -> d | _ -> raise Rrd_protocol.Invalid_payload

(* A helper function for extracting the enum/list out of the RPC type. *)
let list_of_rpc ~(rpc : Rpc.t) : Rpc.t list =
  match rpc with Rpc.Enum l -> l | _ -> raise Rrd_protocol.Invalid_payload

(* [assoc_opt ~key ~default l] gets string value associated with [key] in
 * [l], returning [default] if no mapping is found. *)
let assoc_opt ~(key : string) ~(default : string)
    (l : (string * Rpc.t) list) : string =
  try Rpc.string_of_rpc (List.assoc key l) with
  | Not_found -> default
  | e -> raise e

(* Converts string to the corresponding datasource type. *)
let ds_ty_of_string (s : string) : Rrd.ds_type =
  match String.lowercase_ascii s with
  | "gauge" -> Rrd.Gauge
  | "absolute" -> Rrd.Absolute
  | "derive" -> Rrd.Derive
  | _ -> raise Rrd_protocol.Invalid_payload

(* Converts a string to value of datasource owner type. *)
let owner_of_string (s : string) : Rrd.ds_owner =
  match Astring.String.cuts ~sep:" " (String.lowercase_ascii s) with
  | ["host"] -> Rrd.Host
  | ["vm"; uuid] -> Rrd.VM uuid
  | ["sr"; uuid] -> Rrd.SR uuid
  | _ -> raise Rrd_protocol.Invalid_payload
