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

val introduce : __context:Context.t -> name_label:string ->
  name_description:string -> pVS_uuid:string -> API.ref_PVS_site

val forget_internal : __context:Context.t -> self:API.ref_PVS_site ->
  cleanup_storage:(Context.t -> API.ref_PVS_site -> unit)->
  unit

val forget : __context:Context.t -> self:API.ref_PVS_site -> unit

val set_PVS_uuid : __context:Context.t ->
  self:API.ref_PVS_site -> value:string -> unit
