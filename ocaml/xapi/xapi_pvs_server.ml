(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

(** Implementation of PVS Server *)

open Stdext
open Listext

module D = Debug.Make(struct let name = "xapi_pvs_server" end)

let introduce ~__context ~addresses ~first_port ~last_port ~farm =
  List.iter
    (fun address -> Helpers.assert_is_valid_ip `ipv4 "addresses" address)
    addresses;
  Helpers.assert_is_valid_tcp_udp_port_range
    ~first_port:(Int64.to_int first_port) ~first_name:"first_port"
    ~last_port:(Int64.to_int last_port) ~last_name:"last_port";
  if not (Db.is_valid_ref __context farm)
  then raise Api_errors.(Server_error (invalid_value ,[
      "farm"; Ref.string_of farm
    ]));
  let pvs_server = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.PVS_server.create ~__context
    ~ref:pvs_server ~uuid ~addresses:(List.setify addresses)
    ~first_port ~last_port ~farm;
  pvs_server

let forget ~__context ~self = Db.PVS_server.destroy ~__context ~self
