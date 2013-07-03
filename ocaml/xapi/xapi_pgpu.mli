(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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
(** Module that defines API functions for PGPU objects
 * @group Graphics
 *)

(** Synchronise the PGPU objects in the database with the actual devices in the host. *)
val update_gpus : __context:Context.t -> host:API.ref_host -> unit

(** Move the PGPU to a new GPU group. *)
val set_GPU_group : __context:Context.t -> self:API.ref_PGPU ->
	value: API.ref_GPU_group -> unit
