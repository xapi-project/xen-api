(*
 * Copyright (C) Citrix Systems Inc.
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
(** Create a VUSB. *)
val create :
  __context:Context.t ->
  vM:[ `VM ] Ref.t ->
  uSB_group:[ `USB_group ] Ref.t ->
  other_config:(string * string) list ->
  [ `VUSB ] Ref.t

(** Unplug a VUSB from vm. *)
val unplug:
  __context:Context.t ->
  self:[`VUSB] API.Ref.t -> unit

(** Destroy a VUSB. *)
val destroy : __context:Context.t -> self:[ `VUSB ] Ref.t -> unit
