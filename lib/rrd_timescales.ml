(*
 * Copyright (C) 2015 Citrix Systems Inc.
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
(*
 * Timescales: this allows an RRD server to advertise which Timescales
 * are available, to avoid clients having to already know or guess.
 *)

type t = {
  name: string;
  num_intervals: int;
  interval_in_steps: int;
} [@@deriving rpc]

type ts = t list [@@deriving rpc]

let make ~name ~num_intervals ~interval_in_steps () =
  { name; num_intervals; interval_in_steps }

let name_of t = t.name

let to_span t =
  t.num_intervals * t.interval_in_steps * 5 (* ??? *)

let interval_to_span t =
  t.interval_in_steps * 5

let to_json ts = Jsonrpc.to_string (rpc_of_ts ts)
let of_json txt = ts_of_rpc (Jsonrpc.of_string txt)
