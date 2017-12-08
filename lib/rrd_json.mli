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

val json_of_ds: ?owner:Rrd.ds_owner ->
  ?rshift:int -> Ds.ds -> Buffer.t -> unit

val json_of_dss: header:string -> int64 -> (Rrd.ds_owner * Ds.ds) list -> string

val json_metadata_of_ds: ?owner:Rrd.ds_owner -> Ds.ds -> Buffer.t -> unit

val json_metadata_of_dss: (Rrd.ds_owner * Ds.ds) list -> string
