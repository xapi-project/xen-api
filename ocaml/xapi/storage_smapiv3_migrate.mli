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
(** SXM v3 (SMAPIv3) pre-flight check: reject snapshot shapes the destination's
    VM-snapshot-tree reconstruction cannot express (hidden VDI.snapshots and
    orphan snapshot VDIs on SMAPIv3 SRs). Raises
    [Api_errors.operation_not_allowed] when such VDIs are present. *)
