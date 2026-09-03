(*
 * Copyright (C) 2026 Cloud Software Group
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

(** partition_mode — GPU partition logic
 * Some GPUs can be partitioned: a single physical card is carved into several smaller,
 * isolated pieces, each of which can be assigned to a different VM. In this
 * library, we abstract this state into a card is either partitioned or not, and
 * that state can be changed, sometimes only across a host reboot.
 *
 * When a card can be partitioned across multiple axes, e.g. memory and compute,
 * we can obtain the global state of the card (it is partitioned if at least one
 * axis is partitioned) by using the `combine: mode -> mode -> mode` operation.
 *)

type mode =
  | Not_supported  (** the card cannot be partitioned at all *)
  | Disabled  (** capable, not partitioned, nothing pending *)
  | Enable_on_reboot
  | Enabled  (** card partitioned across at least one axis *)
  | Disable_on_reboot

val combine : mode -> mode -> mode
(** [combine mode1 mode2] produces composite of two per-axis modes: the card is
    partitioned if mode1 or mode2 are. [Not_supported] is the identity.
    Note that [combine Enable_on_reboot Disable_on_reboot == Enabled]*)

val string_of_mode : mode -> string
