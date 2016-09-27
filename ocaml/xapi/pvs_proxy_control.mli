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

val proxy_port_name : API.vIF_t -> string
val get_running_proxies : __context:Context.t -> site:API.ref_PVS_site -> API.ref_PVS_proxy list

val start_proxy : __context:Context.t -> API.ref_VIF -> API.ref_PVS_proxy -> bool
val stop_proxy : __context:Context.t -> API.ref_VIF -> API.ref_PVS_proxy -> unit

val clear_proxy_state : __context:Context.t -> API.ref_VIF -> API.ref_PVS_proxy -> unit

val find_proxy_for_vif : __context:Context.t -> vif:API.ref_VIF -> API.ref_PVS_proxy option
val maybe_start_proxy_for_vif : __context:Context.t -> vif:API.ref_VIF -> unit
val maybe_stop_proxy_for_vif : __context:Context.t -> vif:API.ref_VIF -> unit

val remove_site_on_localhost : __context:Context.t -> site:API.ref_PVS_site -> unit
