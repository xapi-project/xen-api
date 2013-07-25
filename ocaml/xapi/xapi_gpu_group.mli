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

(** Module that defines API functions for GPU_group objects
 * @group Graphics
 *)

(** Create a GPU group. *)
val create :
  __context:Context.t ->
  name_label:string ->
  name_description:string ->
  other_config:(string * string) list -> [ `GPU_group ] Ref.t

(** Destroy a GPU group. *)
val destroy : __context:Context.t -> self:[ `GPU_group ] Ref.t -> unit

(** Find a GPU group for a given pGPU, or create a new one. *)
val find_or_create :
  __context:Context.t -> [ `PGPU ] Ref.t -> [ `GPU_group ] Ref.t

val get_allowed_VGPU_types :
	__context:Context.t -> self:[ `GPU_group ] Ref.t -> [ `VGPU_type ] Ref.t list

val set_GPU_types :
	__context:Context.t -> self:[ `GPU_group ] Ref.t -> value:string list -> unit
