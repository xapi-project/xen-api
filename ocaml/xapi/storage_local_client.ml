(*
 * Copyright (C) 2011 Citrix Systems Inc.
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

module D=Debug.Make(struct let name="storage_server" end)
open D

let f : (Rpc.call -> Rpc.response) option ref = ref None

let rpc call = match !f with
  | Some f -> f call
  | None ->
    error "Storage_server.process not defined; unable to make local SMAPI calls";
    failwith "Storage_server.process not defined; unable to make local SMAPI calls"

module Client = Storage_interface.Client(struct let rpc = rpc end)
include Client
