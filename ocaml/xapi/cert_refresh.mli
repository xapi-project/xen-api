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

(** For this host, generate a new host certificate and distribute its
public parts. When this function returns, pool members still would
accept the old certificate and hence need to perform some clean-up
actions *)

val host :
  __context:Context.t -> type':[< `host | `host_internal] -> API.ref_Certificate

(** On this host, remove stale certs for [host] after [host]'s
certificates were refreshed. This needs to be executed on every 
host of the pool *)

val remove_stale_cert :
     __context:Context.t
  -> host:API.ref_host
  -> type':[< `host | `host_internal]
  -> unit
