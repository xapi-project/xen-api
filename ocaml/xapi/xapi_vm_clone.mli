(*
 * Copyright (C) 20015 Citrix Systems Inc.
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

(* Clone code is parameterised over this so it can be shared with copy *)
type disk_op_t =
  | Disk_op_clone
  | Disk_op_copy of API.ref_SR option
  | Disk_op_snapshot
  | Disk_op_checkpoint

val disk_snapshot_type: string
val quiesced: string

val snapshot_info:
  power_state:[< `Halted
              | `Migrating
              | `Paused
              | `Running
              | `ShuttingDown
              | `Suspended ] ->
  is_a_snapshot:bool ->
  (string * string) list

(* epoch hint for netapp backend *)
val make_driver_params:
  unit ->
  (string * string) list

(* Clone a list of disks, if any error occurs then delete all the ones we've
 * got. Reverse the list at the end, so that the disks are returned in the
 * same order as the [vbds] parameter. *)
val safe_clone_disks:
  (Rpc.call -> Rpc.response Client.Id.t) ->
  [<`session] Ref.t ->
  disk_op_t ->
  __context:Context.t ->
  [ `VBD ] API.Ref.t list ->
  (string * string) list ->
  ([ `VBD ] API.Ref.t * API.ref_VDI * bool) list

val clone_single_vdi:
  ?progress:int64 * int64 * float ->
  (Rpc.call -> Rpc.response Client.Id.t) ->
  [<`session] Ref.t ->
  disk_op_t ->
  __context:Context.t ->
  [<`VDI] Ref.t -> (string * string) list ->
  API.ref_VDI

(* NB this function may be called when the VM is suspended for copy/clone operations. Snapshot can be done in live.*)
val clone :
  ?snapshot_info_record:(string * string) list ->
  disk_op_t ->
  __context:Context.t ->
  vm:[ `VM ] API.Ref.t -> new_name:string ->
  [ `VM ] Ref.t
