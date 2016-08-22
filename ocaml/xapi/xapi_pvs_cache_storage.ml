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

module E = Api_errors

let api_error msg xs = raise (E.Server_error (msg, xs))
let str ref = Ref.string_of ref

let create ~__context ~site ~sR ~size =
  let caches = Db.PVS_cache_storage.get_all ~__context in
  let srs = List.map (fun pcs -> Db.PVS_cache_storage.get_SR ~__context ~self:pcs) caches in

  if List.mem sR srs then
    api_error E.pvs_site_sr_already_added [str site; str sR]
  else
    let cache_storage = Ref.make () in
    let uuid = Uuidm.to_string (Uuidm.create `V4) in
    Db.PVS_cache_storage.create ~__context ~ref:cache_storage ~uuid ~site ~sR ~size;
    cache_storage

let destroy ~__context ~self =
  Db.PVS_cache_storage.destroy ~__context ~self
