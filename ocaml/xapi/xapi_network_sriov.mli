(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

(** Create a network-sriov object on the specific PIF and network, it will internally create a logical PIF to connect the network-sriov and network. Topo: physical PIF - network-sriov - logical PIF - network *)
val create :
  __context:Context.t ->
  pif:[ `PIF ] Ref.t ->
  network:[ `network ] Ref.t ->
  [ `network_sriov ] Ref.t


(** Destroy a network-sriov object, and it will automatically destroy the logical PIF that bonded with it. *)
val destroy : __context:Context.t -> self:[ `network_sriov ] Ref.t -> unit



