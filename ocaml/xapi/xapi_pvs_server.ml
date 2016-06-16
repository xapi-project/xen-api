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

module D = Debug.Make(struct let name = "xapi_pvs_server" end)

let not_implemented x =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ x ]))

let introduce ~__context ~addresses ~first_port ~last_port ~farm =
  not_implemented "PVS_server.introduce"

let forget ~__context ~self =
  not_implemented "PVS_server.forget"
