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

(* start AD external auth backend daemon *)
val start_backend_daemon : wait_until_success:bool -> unit

(* stop AD external auth backend daemon *)
val stop_backend_daemon : wait_until_success:bool -> unit

(* init AD external auth backend service *)
val init_service : __context:Context.t -> unit

(* methods to implement auth signature *)
val methods : Auth_signature.t
