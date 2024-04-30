(*
 * Copyright (c) 2024 Cloud Software Group
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

val maybe_update_vm_anti_affinity_alert_for_vm :
  __context:Context.t -> vm:[`VM] API.Ref.t -> unit
(** updates VM anti-affinity alert with a given VM.*)

val remove_vm_anti_affinity_alert :
  __context:Context.t -> groups:[`VM_group] API.Ref.t list -> unit
(** removes VM anti-affinity alert with given groups.*)

val update_vm_anti_affinity_alert :
  __context:Context.t -> groups:[`VM_group] API.Ref.t list -> unit
(** updates VM anti-affinity alert with given groups.*)

val maybe_update_alerts_on_feature_change :
     __context:Context.t
  -> old_restrictions:(string * string) list
  -> new_restrictions:(string * string) list
  -> unit
(** Updates the VM anti-affinity alert only when Features.VM_group changes.

    @param __context The context information.
    @param old_restrictions The old feature restrictions represented as an association list.
           Each entry in the list contains a feature identifier and its corresponding restriction status.
    @param new_restrictions The new feature restrictions represented as an association list.
           Each entry in the list contains a feature identifier and its corresponding restriction status.
    Example:
      [
        ("restrict_vlan", "true");
        ("restrict_vm_group", "false")
      ]
*)
