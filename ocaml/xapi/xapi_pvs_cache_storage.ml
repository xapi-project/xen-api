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

let create ~__context ~site ~sR ~size =
  let cache_storage = Ref.make () in
  let uuid = Uuidm.to_string (Uuidm.create `V4) in
  Db.PVS_cache_storage.create ~__context ~ref:cache_storage ~uuid ~site ~sR ~size;
  cache_storage

let destroy ~__context ~self =
  Db.PVS_cache_storage.destroy ~__context ~self
