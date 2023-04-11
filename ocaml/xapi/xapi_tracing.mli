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

val initialise : __context:Context.t -> unit

val create :
     __context:Context.t
  -> name_label:string
  -> hosts:API.ref_host list
  -> status:bool
  -> tags:(string * string) list
  -> endpoints:string list
  -> components:string list
  -> filters:string list
  -> processors:string list
  -> unit

val destroy : __context:Context.t -> self:API.ref_Tracing -> unit

val set_hosts :
  __context:Context.t -> self:API.ref_Tracing -> hosts:API.ref_host list -> unit

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
