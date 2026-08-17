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

(* Below exposed only for ease of testing *)

(** A node in the VM snapshot tree, projected onto a single disk position. *)
type snapshot_tree_node = {
    vdi_uuid: string
  ; snapshot_time: Clock.Date.t
  ; on_active_path: bool
  ; children: snapshot_tree_node list
}

val get_snapshot_tree :
  dbg:string -> vdi:Storage_interface.vdi -> snapshot_tree_node list
(** Projects each snapshot VM onto [vdi]'s lineage; reads only the database.
    Roots are sorted by [snapshot_time], oldest first. *)
