(*
 * Copyright (c) Cloud Software Group
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

module type SMAPIv2_MIRROR = Storage_interface.MIRROR

module Copy : sig
  val copy_into_sr :
       task:Storage_task.Storage_task.task_handle
    -> dbg:string
    -> sr:Storage_interface.sr
    -> vdi:Storage_interface.vdi
    -> vm:Storage_interface.vm
    -> url:string
    -> dest:Storage_interface.sr
    -> dest_base:Storage_interface.vdi option
    -> image_format:string
    -> verify_dest:bool
    -> Storage_interface.async_result_t option
  (** Copies [vdi] into [dest] as a snapshot based on [dest_base], or as the
      start of a new chain when [dest_base] is [None]. *)
end

module MIRROR : SMAPIv2_MIRROR

val assert_migratable :
     __context:Context.t
  -> vm_uuid:string
  -> active_vdis:[`VDI] API.Ref.t list
  -> snapshot_vdis:[`VDI] API.Ref.t list
  -> unit
(** Rejects snapshot layouts the destination cannot reconstruct: VDI-level
    snapshots that no VM snapshot accounts for, and snapshot VDIs whose active
    disk has been deleted or is not part of the migration. *)
