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

exception No_cache_sr_available

val check_cache_availability :
  __context:Context.t ->
  host:API.ref_host ->
  farm:API.ref_PVS_farm ->
  (API.ref_SR * API.ref_VDI option) option

val on_proxy_start :
  __context:Context.t -> host:API.ref_host -> farm:API.ref_PVS_farm -> unit

val on_sr_remove : __context:Context.t -> sr:API.ref_SR -> unit
