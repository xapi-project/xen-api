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

open Xenops_interface
open Xenops_utils

(* The network manipulation scripts need to find the VM metadata
   given only an interface name (e.g. "tapX.Y" or "fooUUID"?) *)

module Interface = struct
  type t = {
    name: string;
    vif: Vif.id;
  } [@@deriving rpc]
end

module DB = TypedTable(struct
    include Interface
    let namespace = "interface"
    type key = string
    let key x = [ x ]
  end)

