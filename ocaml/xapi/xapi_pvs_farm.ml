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

(* This module implements methods for the PVS_farm class *)

module D = Debug.Make(struct let name = "xapi_pvs_farm" end)

let not_implemented x =
  raise (Api_errors.Server_error (Api_errors.not_implemented, [ x ]))

let introduce ~__context ~name =
  not_implemented "PVS_farm.introduce"

let forget ~__context ~self =
  not_implemented "PVS_farm.forget"

let set_name ~__context ~self ~value =
  not_implemented "PVS_farm.set_name"

let add_cache_storage ~__context ~self ~value =
  not_implemented "PVS_farm.add_cache_storage"

let remove_cache_storage ~__context ~self ~value =
  not_implemented "PVS_farm.remove_cache_storage"



