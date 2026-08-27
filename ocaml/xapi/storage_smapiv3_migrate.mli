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
