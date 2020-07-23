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

exception Invalid_header_string
exception Invalid_length
exception Invalid_checksum
exception Invalid_payload
exception No_update
exception Payload_too_large
exception Read_error

type payload = {
  timestamp: int64;
  datasources : (Rrd.ds_owner * Ds.ds) list;
}

type protocol = {
  make_payload_reader: unit -> (Cstruct.t -> payload);
  make_payload_writer: unit -> ((int -> Cstruct.t) -> payload -> unit);
}
