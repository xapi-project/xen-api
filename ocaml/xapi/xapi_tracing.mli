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

val set_default :
  __context:Context.t -> pool:API.ref_pool -> host:API.ref_host -> unit

val set_status :
  __context:Context.t -> self:API.ref_Tracing -> status:bool -> unit

val set_tags :
     __context:Context.t
  -> self:API.ref_Tracing
  -> tags:(string * string) list
  -> unit

val set_endpoints :
  __context:Context.t -> self:API.ref_Tracing -> endpoints:string list -> unit

val set_components :
  __context:Context.t -> self:API.ref_Tracing -> components:string list -> unit

val set_filters :
  __context:Context.t -> self:API.ref_Tracing -> filters:string list -> unit

val set_processors :
  __context:Context.t -> self:API.ref_Tracing -> processors:string list -> unit
