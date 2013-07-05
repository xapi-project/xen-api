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
(** Module that defines API functions for VGPU objects
 * @group Graphics
 *)

(** Create a VGPU. *)
val create :
  __context:Context.t ->
  vM:[ `VM ] Ref.t ->
  gPU_group:[ `GPU_group ] Ref.t ->
  device:string -> other_config:(string * string) list ->
  _type:[ `VGPU_type ] Ref.t -> [ `VGPU ] Ref.t

(** Destroy a VGPU. *)
val destroy : __context:Context.t -> self:[ `VGPU ] Ref.t -> unit

(** Duplicate a VGPU. *)
val copy :
  __context:Context.t ->
  vm:[ `VM ] Ref.t -> [ `VGPU ] Ref.t -> [ `VGPU ] Ref.t
