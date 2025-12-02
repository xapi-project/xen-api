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

val bucket_table : Rate_limit.Bucket_table.t

val create :
     __context:Context.t
  -> client_id:string
  -> burst_size:float
  -> fill_rate:float
  -> [`Rate_limit] Ref.t

val destroy : __context:Context.t -> self:[`Rate_limit] API.Ref.t -> unit

val register_xapi_globs : unit -> unit
