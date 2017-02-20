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

(** Clear a VGPU's scheduled_to_be_resident_on field and set its resident_on
 *  field. This should always run on the pool master. *)
val atomic_set_resident_on :
  __context:Context.t ->
  self:[ `VGPU ] Ref.t ->
  value:[ `PGPU ] Ref.t ->
  unit

(** Duplicate a VGPU. *)
val copy :
  __context:Context.t ->
  vm:[ `VM ] Ref.t -> [ `VGPU ] Ref.t -> [ `VGPU ] Ref.t

(* Determine whether a VGPU requires passthrough of an entire PGPU, or
 * will be only require part of the PGPU. *)
val requires_passthrough : __context:Context.t -> self:API.ref_VGPU ->
  [ `PF | `VF ] option
