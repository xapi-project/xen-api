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

(** The state of one partition axis of a card *)
type axis = private {
    capable: bool  (** the axis exists on this card *)
  ; enabled: bool  (** the axis is partitioned now *)
  ; after: bool
        (** the axis is partitioned after the next reboot. This is an
            absolute state, not a pending change *)
}

val absent : axis
(** The state of an axis the card does not have *)

val combine_axis : axis -> axis -> axis
(** Pointwise OR, with [absent] as the identity *)

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

val axis_to_mode : axis -> mode

val axis_of_mode : mode -> axis
(** Inverse of [axis_to_mode] *)

val combine_all : mode list -> mode
(** [combine_all modes] is the composite of all the axes of a card, or
    [Not_supported] if it has none *)

val of_observations : mode option list -> mode option
(** [of_observations axes] is the composite of the per-axis observations,
    where [None] is an axis that could not be read. Any unread axis makes the
    composite [None], since the remaining axes alone could under-report it *)

val to_api :
     mode option
  -> [> `not_supported
     | `disabled
     | `enable_on_reboot
     | `enabled
     | `disable_on_reboot
     | `unknown ]
(** The value of [PGPU.partition_mode] for a composite, with [None] as
    [`unknown] *)
