(*
 * Copyright (C) 2023 Cloud Software Group
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
let set_default ~__context ~pool ~host =
  let pool_uuid = Db.Pool.get_uuid ~__context ~self:pool in
  let host_uuid = Db.Host.get_uuid ~__context ~self:host in
  let host_label = Db.Host.get_name_label ~__context ~self:host in
  let tags =
    [("pool", pool_uuid); ("host", host_uuid); ("host name", host_label)]
  in
  let endpoints = !Xapi_globs.tracing_default_endpoints in
  let processors = !Xapi_globs.tracing_default_processors in
  let filters = !Xapi_globs.tracing_default_filters in
  Tracing.TracerProviders.set_default ~tags ~endpoints ~processors ~filters
