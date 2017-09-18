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


(** Create a USB group. *)
val create :
  __context:Context.t ->
  name_label:string ->
  name_description:string ->
  other_config:(string * string) list -> [ `USB_group ] Ref.t

(** Destroy a USB group. *)
val destroy : __context:Context.t -> self:[ `USB_group ] Ref.t -> unit
